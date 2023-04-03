#Functions for server

player_list <- function() {
  xx <- paste0(fpl_table[["web_name"]], " (", fpl_table[["short_name"]], ")")
  return(xx)
}

summary_file_parser <- function(summary_file) {
  txt_oi <- readLines(paste0("~/Documents/Fantasy-Premier-League/fpl-optimization/data/results/", summary_file))
  start_idx <- grep("\\*\\* GW", txt_oi)
  lineup_idx <- grep("Lineup: ", txt_oi)
  end_idx <- which(txt_oi == "")
  all_gws <- txt_oi[start_idx]
  all_gws <- gsub(":", "", gsub("\\*\\* ", "", all_gws))
  transfer_costs <- ifelse(grepl("CHIP", txt_oi[start_idx+1]), txt_oi[start_idx+2], txt_oi[start_idx+1])
  all_buys <- NULL
  all_sells <- NULL
  all_chips <- NULL
  all_lineups <- NULL
  all_xps <- NULL
  for(i in 1:length(start_idx)) {
    buy_sells <- txt_oi[start_idx[i]:lineup_idx[i]]
    all_buys[i] <- paste(grep("Buy ", buy_sells, value = T), collapse = "<br/>")
    all_sells[i] <- paste(grep("Sell ", buy_sells, value = T), collapse = "<br/>")
    all_chips[i] <- paste(grep("CHIP", buy_sells, value = T), collapse = "<br/>")
    lineups <- txt_oi[lineup_idx[i]:(end_idx[i]-2)]
    all_lineups[i] <- paste(grep("\t", lineups, value = T), collapse = "<br/>")
    all_xps[i] <- paste(gsub("Lineup xPts: ", "", grep("Lineup xPts:", lineups, value = T)), collapse = "<br/>")
  }
  # if(any(all_chips != "")) {
  #   all_buys[all_buys == ""] <- all_chips[all_chips != ""]
  # }
  
  hit_pts <- as.numeric(str_match(transfer_costs, "PT=(\\d)")[,2])*4
  buys_xp <- str_match_all(all_buys, "(\\d+.\\d+) xP")
  buys_xp <- lapply(buys_xp, function(x) as.numeric(x[,2]))
  sells_xp <- str_match_all(all_sells, "(\\d+.\\d+) xP")
  sells_xp <- lapply(sells_xp, function(x) as.numeric(x[,2]))
  xp_gain <- sapply(1:length(all_buys), function(i) sum(buys_xp[[i]]-sells_xp[[i]]))
  
  out_table <- as_tibble(cbind(all_gws, transfer_costs, all_buys, all_sells, xp_gain, all_chips, all_lineups, all_xps)) %>% 
    mutate(all_xps = as.character(as.numeric(all_xps)- hit_pts), xp_gain = if_else(xp_gain == 0, "", as.character(xp_gain))) %>% 
    rename(`Transfer costs` = transfer_costs, Buy = all_buys, Sell = all_sells, `xP Gain from transfers` = xp_gain, Chip = all_chips, Lineup = all_lineups, xPts = all_xps) %>% 
    pivot_longer(cols = -1, names_to = "GW") %>% 
    pivot_wider(names_from = all_gws, values_from = value)
  out_table
}

compareResults <- function(which_gw) {
  if(which_gw == "current") {
    last_deadline <- as_datetime(curr_gw_time)
    curr_deadline <- as_datetime(next_gw_time)
    transfer_window <- interval(last_deadline, curr_deadline)
    
    summary_files_list <- rev(list.files(path = "~/Documents/Fantasy-Premier-League/fpl-optimization/data/results", pattern = "summary_"))
    
    dates_times <- stringr::str_match(summary_files_list, "^summary_(.*)_(.*)_\\d+.txt")[,c(2,3)]
    dates_times[,2] <- gsub("-", ":", dates_times[,2])
    dates_times <- sapply(1:nrow(dates_times), function(x) paste(dates_times[x,], collapse = " "))
    
    files_oi <- summary_files_list[sapply(1:length(dates_times), function(x) ymd_hms(dates_times[x]) %within% transfer_window)]
    
    lapply(files_oi, summary_file_parser)
  } else if(which_gw == "all") {
    summary_files_list <- rev(list.files(path = "~/Documents/Fantasy-Premier-League/fpl-optimization/data/results", pattern = "summary_"))
    
    lapply(summary_files_list, summary_file_parser)
  }
}

settings_file_parser <- function(settings_file_time) {
  txt_oi <- jsonlite::read_json(paste0("~/Documents/Fantasy-Premier-League/fpl-optimization/data/logs/", settings_file_time, "_settings.json"))
  
  paste0(
    "Timestamp: ", settings_file_time, "<br/>",
    
    "Horizon: ", txt_oi$horizon, "&emsp;", "Decay: ", txt_oi$decay_base, "&emsp;", "Noise in EV: ", txt_oi$randomized, "<br/>",
    
    "Locked players: ", toString(player_list()[match(unlist(txt_oi$locked), fpl_table$id)]), "<br/>",
    
    "Banned players: ", toString(player_list()[match(unlist(txt_oi$banned), fpl_table$id)]), "<br/>",
    
    "Hits allowed: ", txt_oi$hit_limit, "<br/>",
    
    "Force chip usage: ", "WC - ", ifelse(txt_oi$chip_limits$wc==1 & is.null(txt_oi$use_wc), "Any", ifelse(is.null(txt_oi$use_wc), "None", txt_oi$use_wc)), " FH - ", ifelse(txt_oi$chip_limits$fh==1 & is.null(txt_oi$use_fh), "Any", ifelse(is.null(txt_oi$use_fh), "None", txt_oi$use_fh)), " BB - ", ifelse(txt_oi$chip_limits$bb==1 & is.null(txt_oi$use_bb), "Any", ifelse(is.null(txt_oi$use_bb), "None", txt_oi$use_bb)), "<br/>",
    
    "Booked transfers: ", if(length(txt_oi$booked_transfers) > 0) {paste(sapply(1:length(txt_oi$booked_transfers), function(i) {
      paste(
        paste("GW-", txt_oi$booked_transfers[[i]]$gw),
        if(!is.null(txt_oi$booked_transfers[[i]]$transfer_in)) {
          paste("IN-", toString(player_list()[match(txt_oi$booked_transfers[[i]]$transfer_in, fpl_table$id)]))
        }else if(!is.null(txt_oi$booked_transfers[[i]]$transfer_out)) {
          paste("OUT-", toString(player_list()[match(txt_oi$booked_transfers[[i]]$transfer_out, fpl_table$id)]))
        },
        # paste("OUT-", toString(player_list()[match(txt_oi$booked_transfers[[i]]$transfer_out, fpl_table$id)])),
        sep = " "
      )
    }), collapse = "<br/>")}, "<br/>",
    
    "Iteration(s): ", txt_oi$iteration, "&emsp;", "Data source: ", if(txt_oi$datasource == "review") "FFS" else txt_oi$datasource
  )
}

comparisonParams <- function(which_gw) {
  if(which_gw == "current") {
    last_deadline <- as_datetime(curr_gw_time)
    curr_deadline <- as_datetime(next_gw_time)
    transfer_window <- interval(last_deadline, curr_deadline)
    
    summary_files_list <- rev(list.files(path = "~/Documents/Fantasy-Premier-League/fpl-optimization/data/results", pattern = "summary_"))
    
    dates_times <- stringr::str_match(summary_files_list, "^summary_(.*)_(.*)_\\d+.txt")[,c(2,3)]
    # dates_times[,2] <- gsub("-", ":", dates_times[,2])
    dates_times <- sapply(1:nrow(dates_times), function(x) paste(dates_times[x,], collapse = "_"))
    dates_times_oi <- dates_times[sapply(1:length(dates_times), function(x) ymd_hms(dates_times[x]) %within% transfer_window)]
    
    xx <- sapply(dates_times_oi, settings_file_parser)
    # names(xx) <- dates_times
    xx
  } else if(which_gw == "all") {
    
    summary_files_list <- rev(list.files(path = "~/Documents/Fantasy-Premier-League/fpl-optimization/data/results", pattern = "summary_"))
    
    dates_times <- stringr::str_match(summary_files_list, "^summary_(.*)_(.*)_\\d+.txt")[,c(2,3)]
    # dates_times[,2] <- gsub("-", ":", dates_times[,2])
    dates_times <- sapply(1:nrow(dates_times), function(x) paste(dates_times[x,], collapse = "_"))
    
    xx <- sapply(unique(dates_times), settings_file_parser)
    # names(xx) <- dates_times
    xx
  }
}

format_columns <- function(ffs_data_table) {
  pts_tbl <- ffs_data_table %>% 
    select(contains("Pts"))
  pts_indx <- grep("Pts", colnames(ffs_data_table))
  
  brks_list <- list()
  clrs_list <- list()
  for(i in 1:ncol(pts_tbl)) {
    vals_p <- unlist(pts_tbl[,i])
    # vals <- ifelse(vals != 0, log10(vals))
    brks_list[[i]] <- seq(min(vals_p), max(vals_p), length.out = 19)
    clrs_list[[i]] <- paste0(
      "rgb(",
      round(seq(247, 35, length.out = length(brks_list[[i]]) + 1), 0), ",",
      round(seq(252, 139, length.out = length(brks_list[[i]]) + 1), 0), ",",
      round(seq(245, 69, length.out = length(brks_list[[i]]) + 1), 0),
      ")"
    )
  }
  
  c1 <- paste("df_show %>%", paste(paste0("formatStyle(columns = ", pts_indx[-length(pts_indx)], ", backgroundColor = styleInterval(", brks_list[-length(brks_list)], ", ", clrs_list[-length(clrs_list)], ")) %>% "), collapse = " "))
  
  c2 <- paste0("formatStyle(columns = ", pts_indx[length(pts_indx)], ", backgroundColor = styleInterval(", brks_list[length(brks_list)], ", ", clrs_list[length(clrs_list)], "))")
  
  format_cmd <- paste(c1, c2)
  
  format_cmd
}