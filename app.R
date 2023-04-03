#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

options(warn = -1)
library(shiny)
library(shinyWidgets)
library(DT)
library(tidyverse)
library(lubridate)

#Clear FFS dl file

writeLines("", "~/Documents/Fantasy-Premier-League/fpl-optimization/data/ffs_dl_output.txt")
writeLines("", "~/Documents/Fantasy-Premier-League/fpl-optimization/data/solver_output.txt")

#Current status of the game
fpl_api <- jsonlite::fromJSON("https://fantasy.premierleague.com/api/bootstrap-static/")
curr_gw <- fpl_api$events[fpl_api$events[,"is_current"], "id"]
curr_gw_time <- fpl_api$events[fpl_api$events[,"is_current"], "deadline_time"]
next_gw <- fpl_api$events[fpl_api$events[,"is_next"], "id"]
next_gw_time <- fpl_api$events[fpl_api$events[,"is_next"], "deadline_time"]

players <- fpl_api$elements
teams <- fpl_api$teams

fpl_table <- dplyr::full_join(players, teams, by = c("team" = "id"))

fpl_table$web_name <- gsub("[.] ", ".", stringi::stri_trans_general(fpl_table$web_name, id = "Latin-ASCII"))

if(file.exists("~/Documents/Fantasy-Premier-League/fpl-optimization/data/fplreview.csv")) {
  ffs_file_timestamp <- file.info("~/Documents/Fantasy-Premier-League/fpl-optimization/data/fplreview.csv")$mtime
  
} else ffs_file_timestamp <- "File does not exist!"

if(file.exists("~/Documents/Fantasy-Premier-League/fpl-optimization/data/kiwi.csv")) {
  kiwi_file_timestamp <- file.info("~/Documents/Fantasy-Premier-League/fpl-optimization/data/kiwi.csv")$mtime
} else kiwi_file_timestamp <- "File does not exist!"

#Define the UI
ui <- fluidPage(
  
  shinyjs::useShinyjs(),
  # Application title
  titlePanel("GD's FPL tool"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      width = 3,
      h3("Download data"),
      headerPanel(""),
      div(
        id = "download_div",
        style = "word-wrap: break-word",
        h5("1. Login to Fantasy Premier League and copy the contents of 'https://fantasy.premierleague.com/api/my-team/your-team-id/' to team.json file"),
        actionButton(inputId = "team_json", label = "Open team.json file", icon = icon("file"), class = "btn-default"),
        headerPanel(""),
        headerPanel(""),
        h5("2. Download data from Fantasy Football Scout"),
        actionButton(inputId = "ffs_data", label = "Download Data from FFS", icon = icon("download"), class = "btn-default"),
        headerPanel(""),
        headerPanel(""),
        h5("(Optional) Download data from theFPLKiwi"),
        actionButton(inputId = "kiwi_data", label = "Download Data from theFPLKiwi", icon = icon("download"), class = "btn-default", onclick ="window.open('https://thefplkiwi.github.io/webpage/', '_blank')"),
        headerPanel("")
      ),
      hr(),
      h3("Adjust solver settings"),
      div(
        id = "settings",
        # uiOutput("horizon_slider"),
        sliderInput(inputId = "horizon", label = "Horizon (GW)", min = next_gw, max = ifelse((next_gw + 12) > 38, 38, (next_gw + 12)), step = 1, value = ifelse((next_gw + 6) > 38, 38, (next_gw + 6))),
        sliderInput(inputId = "decay_base", label = "Decay", min = 0, max = 1.0, value = 1.0, step = 0.01),
        sliderInput(inputId = "ft_value", label = "EV difference to consider for transfer", min = 0.8, max = 4.0, step = 0.1, value = 1.5),
        radioButtons(inputId = "no_future_transfer", label = "No transfers?", choices = c("True" = "true", "False" = "false"), selected = "false", inline = T),
        sliderInput(inputId = "no_transfer_last_gws", label = "Prevent transfers in the last X GW's of the horizon", min = 0, max = 8, value = 1, step = 1),
        radioButtons(inputId = "randomized", label = "Introduce noise in EV?", choices = c("True" = "true", "False" = "false"), selected = "false", inline = T),
        sliderInput(inputId = "xmin_lb", label = "Filter players below xMins", min = 0, max = 60, value = 30, step = 5),
        sliderInput(inputId = "ev_per_price_cutoff", label = "Filter players with lowest EV per price (percentile)", min = 0, max = 50, value = 20, step = 5),
        selectizeInput(inputId = "keep", label = "Do not remove these players when filtering for EV per price", choices = NULL, multiple = T),
        selectizeInput(inputId = "banned", label = "Force exclude these players", choices = NULL, multiple = T),
        selectizeInput(inputId = "locked", label = "Do not remove these players", choices = NULL, multiple = T),
        sliderInput(inputId = "secs", label = "Limit time of solver", min = 300, max = 1200, value = 300, step = 300),
        sliderInput(inputId = "hit_limit", label = "How many hits allowed in the horizon?", min = 0, max = 8, value = 0, step = 1),
        selectizeInput(inputId = "use_wc", label = "Force use Wildcard in GW", choices = NULL),
        selectizeInput(inputId = "use_bb", label = "Force use Bench Boost in GW", choices = NULL),
        selectizeInput(inputId = "use_fh", label = "Force use Free Hit in GW", choices = NULL),
        selectizeInput(inputId = "no_transfer_gws", label = "Force block transfers in GW", choices = NULL, multiple = T),
        fluidRow(
          column(10, textAreaInput(inputId = "booked_transfers", label = "Booked transfers", value = "", rows = 2, resize = "none")),
          column(2, actionButton(inputId = "book_clear", label = "", icon = icon("broom"), class = "btn-default", style = "margin-top: 25px"))
        ),
        fluidRow(
          column(3, selectizeInput(inputId = "book_gw", label = "GW", choices = NULL)),
          column(4, selectizeInput(inputId = "book_player", label = "Player", choices = NULL)),
          column(3, radioButtons(inputId = "book_in_out", label = "Transfer:", choices = c("In" = "in", "Out" = "out"), selected = "in")),
          column(2, actionButton(inputId = "book_add", label = "", icon = icon("add"), class = "btn-default"))
        ),
        radioButtons(inputId = "solver", label = "Use solver", choices = c("Cbc" = "cbc", "HiGHS" = "highs"), selected = "highs", inline = T),
        radioButtons(inputId = "no_opposing_play", label = "Do not allow players that are playing each other", choices = c("True" = "true", "False" = "false"), selected = "false", inline = T),
        sliderInput(inputId = "iteration", label = "How many iterations of solving?", min = 1, max = 5, value = 1, step = 1),
        selectInput(inputId = "iteration_criteria", label = "Iteration Criteria", choices = c(
          "Force to replace player to buy current GW in each solution" = "this_gw_transfer_in",
          "An alternative move with either bought or sold players to be different" = "this_gw_transfer_in_out"
        ), selected = "this_gw_transfer_in_out"),
        selectInput(inputId = "datasource", label = "Data Source", choices = c(
          "Mixed" = "mixed",
          "FFS" = "review",
          "Kiwi" = "kiwi"
        ), selected = "review"),
        shinyjs::hidden(sliderInput(inputId = "review_weight", label = "Weight of review data", min = 0, max = 100, value = 80, step = 5)),
        shinyjs::hidden(sliderInput(inputId = "kiwi_weight", label = "Weight of kiwi data", min = 0, max = 100, value = 100 - 80, step = 5)),
        headerPanel("")
      ),
      hr(),
      actionButton(inputId = "solve", label = "Solve", icon = icon("calculator"), class = "btn-success")
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      tabsetPanel(
        type = "tabs",
        tabPanel(
          "Data preparation",
          div(
            id = "current_state",
            h4("Current status:"),
            fluidRow(
              column(4, htmlOutput(outputId = "curr_gw")),
              column(4, htmlOutput(outputId = "next_gw")),
              column(4, htmlOutput(outputId = "next_gw_time"))
            ),
            htmlOutput(outputId = "ffs_data_timestamp"),
            htmlOutput(outputId = "kiwi_data_timestamp")
          ),
          hr(),
          shinyjs::hidden(div(
            id = "download_console",
            h4("Console output for data download script:"),
            htmlOutput(outputId = "dl_console"),
            tags$head(
              tags$style("#dl_console {background-color: gray; font-family: 'Courier New', Courier, 'Lucida Sans Typewriter', 'Lucida Typewriter'; color: white; max-height: 200px; overflow-y:scroll; display: flex; flex-direction: column-reverse}")
            )
          )),
          hr(),
          switchInput(inputId = "show_data", label = "Show FFS Data", onStatus = "success", offStatus = "danger", inline = T, width = "auto", labelWidth = "150px"),
          shinyjs::hidden(div(
            id = "ffs_data_div",
            h4("FFS Data:"),
            DTOutput(outputId = "ffs_data_show")
          )),
          hr(),
          shinyjs::hidden(div(
            id = "solver_div",
            h4("Console output for solver:"),
            htmlOutput(outputId = "solver_console"),
            tags$head(
              tags$style("#solver_console {background-color: darkgray; font-family: 'Courier New', Courier, 'Lucida Sans Typewriter', 'Lucida Typewriter'; color: white; max-height: 400px; overflow-y:scroll; display: flex; flex-direction: column-reverse}")
            )
          ))
        ),
        tabPanel(
          "Results",
          headerPanel(""),
          fluidRow(
            column(3, actionButton(inputId = "show_transfers", label = "Show latest transfers", icon = icon("exchange"))),
            column(3, actionButton(inputId = "show_results", label = "Show latest results", icon = icon("medal")))
          ),
          DT::DTOutput(outputId = "transfers"),
          headerPanel(""),
          headerPanel(""),
          DT::DTOutput(outputId = "results")
        ),
        tabPanel(
          "Compare results",
          headerPanel(""),
          fluidRow(
            column(6, selectInput(inputId = "compare_type", label = "Compare results from", choices = c("this gameweek" = "current", "all gameweeks" = "all"), selected = "current", multiple = F, width = "100%")),
            column(3, actionButton(inputId = "compare_show", label = "Show comparisons", icon = icon("code-compare")))
          ),
          uiOutput(outputId = "comparisons")
        )
      )
    )
  )
)


#Server
source("transfer_planner/functions.R")

server <- function(input, output, session) {
  
  output$curr_gw <- renderText(paste("Current GW:", curr_gw))
  
  output$next_gw <- renderText(paste("Next GW:", next_gw))
  
  output$next_gw_time <- renderText(paste("Next deadline:", lubridate::as_datetime(next_gw_time, tz = "Europe/Berlin")))
  
  output$ffs_data_timestamp <- renderText(paste("Last update of FFS data file:", ffs_file_timestamp))
  
  output$kiwi_data_timestamp <- renderText(paste("Last update of theFPLKiwi data file:", kiwi_file_timestamp))
  
  # output$horizon_slider <- renderUI({
  #   sliderInput(inputId = "horizon", label = "Horizon", min = next_gw, max = (next_gw + 7), step = 1, value = (next_gw + 7))
  # })
  
  observeEvent(input$team_json, {
    system("open -a TextEdit ~/Documents/Fantasy-Premier-League/fpl-optimization/data/team.json")
  })
  
  rv <- reactiveValues(textstream = c(""), timer = reactiveTimer(500), started = F)
  observeEvent(input$ffs_data, {
    shinyjs::show(id = "download_console")
    rv$started <- T
    system2("Rscript", "~/Documents/Fantasy-Premier-League/get_data.R", wait = F)
  })
  observe({
    rv$timer()
    if(isolate(rv$started)) rv$textstream <- paste(readLines("~/Documents/Fantasy-Premier-League/fpl-optimization/data/ffs_dl_output.txt"), collapse = "<br/>")
  })
  output$dl_console <- renderUI(HTML(rv$textstream))
  
  observeEvent(input$show_data, {
    if(input$show_data == T) {
      ffs_data_table <- read_csv("~/Documents/Fantasy-Premier-League/fpl-optimization/data/fplreview.csv")
      
      vals_m <- unlist(ffs_data_table %>% select(contains("xMins")))
      # vals <- ifelse(vals != 0, log10(vals))
      brks_m <- seq(min(vals_m), max(vals_m), length.out = 19)
      clrs_m <- paste0(
        "rgb(",
        round(seq(247, 35, length.out = length(brks_m) + 1), 0), ",",
        round(seq(252, 139, length.out = length(brks_m) + 1), 0), ",",
        round(seq(245, 69, length.out = length(brks_m) + 1), 0),
        ")"
      )
      
      inspect_cols <- ffs_data_table %>% select(contains("_Pts")) %>% names()
      col_order <- purrr::map(
        names(ffs_data_table), 
        function(x) {
          if (x %in% inspect_cols) {
            c(x, gsub("_Pts", "_xMins", x))
          } else if (!grepl("_xMins", x)){
            x
          }
        }
      ) %>% 
        unlist()
      
      ffs_data_table <- ffs_data_table %>% 
        select(all_of(col_order)) %>% 
        mutate(total_xp = reduce(select(ffs_data_table, contains("Pts")), `+`)) %>% 
        arrange(-total_xp) %>% 
        select(-total_xp)
      
      shinyjs::show(id = "ffs_data_div")
      
      # x <- reactiveValues()
      df <- as.data.frame(ffs_data_table, stringsAsFactors=F)
      df_show <- datatable(df,
                           rownames = F,
                           editable = T,
                           filter = "top",
                           selection = "none",
                           extensions = c(
                             "ColReorder",
                             "Buttons"
                           ),
                           options = list(
                             dom = "lBRrftpi",
                             autoWidth = T,
                             pageLength = 20,
                             scrollX = T,
                             ColReorder = T,
                             buttons = list(list(extend = 'csv', filename= 'fplreview'), 'print', 'copy')
                           )) %>% 
        formatStyle(columns = seq(8, ncol(ffs_data_table), 2), backgroundColor = styleInterval(brks_m, clrs_m))
      
      eval_envir = rlang::new_environment(data = list("format_columns" = format_columns), parent = rlang::current_env())
      
      output$ffs_data_show <- renderDT(eval(parse(text = format_columns(ffs_data_table = ffs_data_table)), eval_envir))
      
      proxy = dataTableProxy("ffs_data_show")
      observeEvent(input$ffs_data_show_cell_edit, {
        info = input$ffs_data_show_cell_edit
        i = info$row
        j = info$col
        v = info$value
        # info <- data.frame(
        #   row = i,
        #   col = j + 1,
        #   value = v
        # )
        # str(info)
        
        new_i <- i
        new_j <- j - 1
        new_val <- round((df[[i, j]]/ffs_data_table[[i, (j+1)]])*v, 2)
        new_info <- data.frame(
          row = new_i,
          col = new_j,
          value = new_val
        )
        # str(new_info)
        
        df <<- editData(df, info, proxy, rownames = F)
        
        if(j > 6 & j%%2==1){
          df <<- editData(df, new_info, proxy, rownames = F)
        }
        
        # replaceData(proxy = dataTableProxy("ffs_data_show"), data = df, resetPaging = FALSE)
      })
      
    } else if(input$show_data == F) {
      shinyjs::hide(id = "ffs_data_div")
    }
  })
  
  
  
  observe({
    updateSelectizeInput(session = session, inputId = "keep", choices = player_list(), selected = character(0), server = T, options = list(placeholder = 'Search for a player'))
  })
  
  observe({
    updateSelectizeInput(session = session, inputId = "banned", choices = player_list(), selected = character(0), server = T, options = list(placeholder = 'Search for a player'))
  })
  
  observe({
    updateSelectizeInput(session = session, inputId = "locked", choices = player_list(), selected = character(0), server = T, options = list(placeholder = 'Search for a player'))
  })
  
  observe({
    updateSelectizeInput(session = session, inputId = "book_gw", choices = Map(seq, next_gw, input$horizon), selected = character(0), server = T)
  })
  
  observe({
    updateSelectizeInput(session = session, inputId = "book_player", choices = player_list(), selected = character(0), server = T)
  })
  
  observeEvent(input$book_add, {
    updateTextAreaInput(session = session, inputId = "booked_transfers", value = ifelse(input$book_in_out == "in", gsub(pattern = "^, ", "", toString(c(input$booked_transfers, paste0("GW: ", input$book_gw, " | Transfer in: ", input$book_player)))), gsub(pattern = "^, ", "", toString(c(input$booked_transfers, paste0("GW: ", input$book_gw, " | Transfer out: ", input$book_player))))))
    })
  
  observeEvent(input$book_clear, {
    updateTextAreaInput(session = session, inputId = "booked_transfers", value = "")
  })
  
  observe({
    updateSelectizeInput(session = session, inputId = "use_wc", choices = c("Any", Map(seq, next_gw, input$horizon)), selected = character(0), server = T)
  })
  
  observe({
    updateSelectizeInput(session = session, inputId = "use_bb", choices = c("Any", Map(seq, next_gw, input$horizon)), selected = character(0), server = T)
  })
  
  observe({
    updateSelectizeInput(session = session, inputId = "use_fh", choices = c("Any", Map(seq, next_gw, input$horizon)), selected = character(0), server = T)
  })
  
  observe({
    updateSelectizeInput(session = session, inputId = "no_transfer_gws", choices = Map(seq, next_gw, input$horizon), selected = character(0), server = T)
  })
  
  observeEvent(input$datasource, {
    if(input$datasource == "mixed") {
      shinyjs::show(id = "review_weight")
      shinyjs::show(id = "kiwi_weight")
    } else {
      shinyjs::hide(id = "review_weight")
      shinyjs::hide(id = "kiwi_weight")
    }
  })
  
  observeEvent(input$review_weight,  {
    updateSliderInput(session = session, inputId = "kiwi_weight", value = 100 - input$review_weight)
  })
  
  observeEvent(input$kiwi_weight,  {
    updateSliderInput(session = session, inputId = "review_weight", value = 100 - input$kiwi_weight)
  })
  
  rv2 <- reactiveValues(textstream = c(""), timer = reactiveTimer(500), started = F)
  observeEvent(input$solve, {
    
    ###solver settings
    solver_settings <- jsonlite::read_json("~/Documents/Fantasy-Premier-League/fpl-optimization/data/regular_settings_original.json")
    
    solver_settings$horizon <- input$horizon - next_gw + 1
    solver_settings$decay_base <- input$decay_base
    solver_settings$ft_value <- input$ft_value
    solver_settings$no_future_transfer <- as.logical(input$no_future_transfer)
    solver_settings$no_transfer_last_gws <- input$no_transfer_last_gws
    solver_settings$randomized <- as.logical(input$randomized)
    solver_settings$xmin_lb <- input$xmin_lb
    solver_settings$ev_per_price_cutoff <- input$ev_per_price_cutoff
    solver_settings$keep <-  if(length(input$keep) > 1) fpl_table$id[match(input$keep, player_list())] else if(length(input$keep) == 1) list(fpl_table$id[match(input$keep, player_list())]) else list()
    solver_settings$banned <-  if(length(input$banned) > 1) fpl_table$id[match(input$banned, player_list())] else if(length(input$banned) == 1) list(fpl_table$id[match(input$banned, player_list())]) else list()
    solver_settings$locked <- if(length(input$locked) > 1) fpl_table$id[match(input$locked, player_list())] else if(length(input$locked) == 1) list(fpl_table$id[match(input$locked, player_list())]) else list()
    solver_settings$secs <- input$secs
    solver_settings$hit_limit <- as.numeric(input$hit_limit)
    if(input$use_wc != "") {
      if(input$use_wc == "Any") {
        solver_settings$chip_limits$wc <- 1
      } else {
        solver_settings$use_wc <- as.numeric(input$use_wc)
        solver_settings$chip_limits$wc <- 1
      }
    }
    if(input$use_bb != "") {
      if(input$use_bb == "Any") {
        solver_settings$chip_limits$bb <- 1
      } else {
        solver_settings$use_bb <- as.numeric(input$use_bb)
        solver_settings$chip_limits$bb <- 1
      }
    }
    if(input$use_fh != "") {
      if(input$use_fh == "Any") {
        solver_settings$chip_limits$fh <- 1
      } else {
        solver_settings$use_fh <- as.numeric(input$use_fh)
        solver_settings$chip_limits$fh <- 1
      }
    }
    solver_settings$no_transfer_gws <- if(length(input$no_transfer_gws) == 0) NULL else list(as.numeric(input$no_transfer_gws))
    if(input$booked_transfers != "") {
      xx <- strsplit(input$booked_transfers, split = ", ")[[1]]
      solver_settings$booked_transfers <- lapply(1:length(xx) , function(y) {
        gw <- stringr::str_match(xx[y], "GW: (\\d+)")[1, 2]
        transfer_in_out <- stringr::str_match(xx[y], "Transfer (\\w+): (.*$)")[1, 2]
        transfer_player <- stringr::str_match(xx[y], "Transfer (\\w+): (.*$)")[1, 3]
        transfer_id <- fpl_table$id[match(transfer_player, player_list())]
        yy <- jsonlite::fromJSON(paste0('{"gw": ', gw, ', "transfer_', transfer_in_out,'": ', transfer_id, '}'))
        yy$gw <- jsonlite::unbox(yy$gw)
        yy[[paste0("transfer_", transfer_in_out)]] <- jsonlite::unbox(yy[[paste0("transfer_", transfer_in_out)]])
        yy
      })
    }
    solver_settings$solver <- input$solver
    solver_settings$no_opposing_play <- as.logical(input$no_opposing_play)
    solver_settings$iteration <- as.numeric(input$iteration)
    solver_settings$iteration_criteria <- input$iteration_criteria
    solver_settings$datasource <- input$datasource
    if(input$datasource == "mixed") {
      solver_settings$data_weights$review <- as.numeric(input$review_weight)
      solver_settings$data_weights$kiwi <- as.numeric(input$kiwi_weight)
    }
    
    # print(solver_settings)
    
    jsonlite::write_json(solver_settings, path = "~/Documents/Fantasy-Premier-League/fpl-optimization/data/regular_settings.json", pretty = T, auto_unbox = T, null = "null")
    
    ###
    shinyjs::show(id = "solver_div")
    rv2$started <- T
    system2("Rscript", "~/Documents/Fantasy-Premier-League/run_solver.R", wait = F)
  })
  observe({
    rv2$timer()
    if(isolate(rv2$started)) rv2$textstream <- paste(readLines("~/Documents/Fantasy-Premier-League/fpl-optimization/data/solver_output.txt"), collapse = "<br/>")
  })
  output$solver_console <- renderUI(HTML(rv2$textstream))
  
  observeEvent(input$compare_show, {
    tables <- compareResults(which_gw = input$compare_type)
    settings <- comparisonParams(which_gw = input$compare_type)
    # iters <- as.numeric(str_match(settings, "Iteration\\(s\\): (\\d)")[,2])
    names(tables) <- paste0(settings, "<br/>", "Total xPts: ", sapply(tables, function(x) sum(as.numeric(x[nrow(x), -1]))), " ", 1:length(tables))
    output$comparisons <- renderUI({
      tableList <- imap(tables, ~ {
        tagList(
          h5(HTML(gsub(" \\d+$", "", .y))), # Note we can sprinkle in other UI elements
          headerPanel(""),
          DTOutput(outputId = paste0("table_", str_match(.y, " (\\d+)$")[,2])),
          hr()
        )
      })
      tagList(lapply(tableList, br))
    })
    # Now render each output
    iwalk(tables, ~ {
      output_name <- paste0("table_", str_match(.y, " (\\d+)$")[,2])
      output[[output_name]] <- renderDT(datatable(.x,
                                                  rownames = F,
                                                  editable = F,
                                                  escape = F,
                                                  selection = "none",
                                                  # filter = "top",
                                                  extensions = c(
                                                    "Responsive"
                                                  ),
                                                  options = list(
                                                    dom = "rt",
                                                    autoWidth = T,
                                                    pageLength = 20,
                                                    scrollX = T,
                                                    # ColReorder = F,
                                                    # buttons = list(list(extend = 'csv', filename= 'plan_download'), 'print', 'copy'),
                                                    responsive = F,
                                                    columnDefs = list(
                                                      list(width = '50px', targets = c(0)),
                                                      list(width = '500px', targets = "_all")
                                                    )
                                                  )))
    })
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
