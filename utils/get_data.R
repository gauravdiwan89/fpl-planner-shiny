library(tidyverse)
library(RSelenium)
library(rvest)

setwd("~/Documents/Fantasy-Premier-League/")

sink(file = "fpl-optimization/data/ffs_dl_output.txt")

fpl_api <- jsonlite::fromJSON("https://fantasy.premierleague.com/api/bootstrap-static/")
curr_gw <- fpl_api$events[fpl_api$events[,"is_next"], "id"]
last_gw <- if((curr_gw + 12) < 39) (curr_gw + 12) else 38

system("docker run -d -p 4445:4444 selenium/standalone-firefox")

remDr <- remoteDriver(port=4445L)
Sys.sleep(3)
remDr$open()
Sys.sleep(5)
remDr$navigate("https://rate-my-team.fantasyfootballscout.co.uk/players/")
# remDr$getCurrentUrl()
cat("Downloading xP from RMT tool...\n")
uin <- remDr$findElement(using = "id", value = "username")
uin$sendKeysToElement(list("username"))
pin <- remDr$findElement(using = "id", value = "password")
pin$sendKeysToElement(list(keyring::key_get(service = "ffs", username = "username")))
pin$sendKeysToElement(list(key = "enter"))
Sys.sleep(5)

remDr$navigate(paste0("https://rate-my-team.fantasyfootballscout.co.uk/players/?&team=0&position=0&maxval=0&minval=0&order=0&first=", curr_gw,"&last=", last_gw,""))
Sys.sleep(5)

tbl_oi <- remDr$getPageSource()[[1]] %>% 
  read_html() %>%
  html_table()
tbl_oi <- tbl_oi[[1]]
remDr$close()

xp_table <- tbl_oi %>% 
  select(Name = Player, Team, Pos, Price, contains ("GW"), Total) %>% 
  pivot_longer(contains("GW")) %>% 
  separate(value, into = c("Pts", "drop", "xMins"), sep = " ") %>% 
  select(-drop) %>% 
  pivot_wider(names_from = name, names_glue = "{name}_{.value}", values_from = c(Pts, xMins)) %>% 
  mutate(across(.cols = contains("_Pts"), .fns = ~ trimws(gsub("adjP", "", .)))) %>% 
  rename_with(.cols = contains("GW"), .fn = ~ paste0(gsub("GW", "", .x, perl = T))) %>% 
  mutate(BV = gsub("m", "", Price), SV = gsub("m", "", Price)) %>% 
  mutate(ID = row_number()) %>% 
  select(ID, Name, Team, Pos, BV, SV, contains("_Pts"), contains("_xMins")) %>% 
  distinct() %>% 
  mutate(Team = gsub("Tottenham", "Spurs", Team)) %>% 
  mutate(Team = gsub("Nottingham Forest", "Nott'm Forest", Team)) 

head(xp_table)

###api data
cat("Creating Merged table for optimization...\n")

players <- fpl_api$elements
teams <- fpl_api$teams

fpl_table <- full_join(players, teams, by = c("team" = "id"))

fpl_table$web_name <- gsub("[.] ", ".", stringi::stri_trans_general(fpl_table$web_name, id = "Latin-ASCII"))

xp_table$Name <- stringi::stri_trans_general(xp_table$Name, id = "Latin-ASCII")

##cleanUp names in ffs table

bad_idx_ffs <- which(is.na(match(xp_table$Name, fpl_table$web_name)))
bad_in_ffs <- xp_table$Name[bad_idx_ffs]
bad_in_fpl <- fpl_table$web_name[which(is.na(match(fpl_table$web_name, xp_table$Name)))]

ffs_bad_short <- paste0(str_match(bad_in_ffs, "\\((.)")[,2], ".", str_match(bad_in_ffs, "^(\\w+) \\(")[,2])

fix1 <- match(ffs_bad_short, bad_in_fpl)
ffs_bad1 <- which(!is.na(fix1))
xp_table$Name[bad_idx_ffs[ffs_bad1]] <- bad_in_fpl[fix1[!is.na(fix1)]]

ffs_bad2 <- which(is.na(fix1))
ffs_bad_single <- str_match(bad_in_ffs, "^(\\w+) \\(")[,2]
ffs_bad_single[is.na(ffs_bad_single)] <- if_else(grepl(" ", bad_in_ffs[is.na(ffs_bad_single)]), str_match(bad_in_ffs[is.na(ffs_bad_single)], " (\\w+)$")[,2], bad_in_ffs[is.na(ffs_bad_single)])
xp_table$Name[bad_idx_ffs[ffs_bad2]] <- ffs_bad_single[ffs_bad2]

merged_data <- right_join(fpl_table, xp_table, by = c("web_name" = "Name", "name" = "Team")) %>% 
  drop_na(id) %>% 
  distinct(code.x, .keep_all = T) %>% 
  select(ID = id, Name = web_name, Team = name, Pos, BV, SV, contains("_Pts"), contains("_xMins"))

merged_data %>% 
  as_tibble() %>% 
  rowwise() %>% 
  mutate(across(contains("_Pts"), ~ paste(str_match_all(., "\\S")[[1]][,1], collapse = ""))) %>% 
  write_csv("fpl-optimization/data/fplreview.csv")

system("docker stop $(docker ps -a -q)")
Sys.sleep(5)
cat("DONE!!\n")

sink()