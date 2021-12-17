library(tidyverse)
library(rvest)
# library(httr)
library(dplyr)
library(stringr)
# library(readxl)
library(progress)
library(RSelenium)
#library(mail)
library(jsonlite)
library(janitor)
library(httr)
library(janitor)
library(tidyr)
library(tm)
library(SnowballC)
library(stringdist)
library(data.table)
library(RPostgreSQL)
library(DBI)
library(RPostgres)
library(rstudioapi)
library(keyring)
library(XML)
library(lpSolve)
library(ffanalytics)
library(xml2)


header.true <- function(df) {
  names(df) <- as.character(unlist(df[1,]))
  df[-1,]
}


#### Fantasy Pros/Yahoo ####
scrape_fantasy_pros <- function(){
  print("Fantasy Pros - QB")
  
  # qb_link <- read_html("https://www.fantasypros.com/nfl/projections/qb.php?week=17")
  qb_link <- read_html("https://www.fantasypros.com/nfl/projections/qb.php?week=0")
  
  qb_player <- qb_link %>%
    html_nodes(".player-name") %>%
    html_text()
  
  qb_team <- qb_link %>%
    html_nodes(".js-tr-game-select .player-label") %>%
    html_text() %>%
    substr(nchar(.)-3, nchar(.)-1) %>%
    str_remove(" ")
  
  qb_table <- qb_link %>%
    html_nodes(xpath = '//*[@id="data"]') %>%
    html_table()
  
  qb_table <- qb_table[[1]] %>%
    header.true() %>%
    header.true()
  
  qb_table$Player <- qb_player
  qb_table$team <- qb_team
  colnames(qb_table) <- c("player", "pass_att", "pass_cmp", "pass_yd", "pass_td", "int", "rush_att", "rush_yd",
                          "rush_td", "fum", "fpts", "team")
  qb_table <- qb_table %>%
    select(player, team, pass_yd, pass_td, int, rush_yd, rush_td, fum) %>%
    mutate(pass_yd = str_remove_all(pass_yd, ","),
           rush_yd = str_remove_all(rush_yd, ","))
  
  num_cols <- colnames(qb_table)
  num_cols <- num_cols[-(1:2)]
  
  qb_table[num_cols] <- sapply(qb_table[num_cols],as.numeric)
  
  qb_table <- qb_table %>%
    mutate(player = str_remove(player, "'"),
           player = str_remove_all(player, "\\."),
           player = str_remove_all(player, " Jr"),
           player = str_remove_all(player, " IV"),
           player = str_remove_all(player, " III"),
           player = str_remove_all(player, " II"),
           player = tolower(player),
           position = "QB",
           source = "Fantasy Pros/Yahoo",
           rec = NA,
           rec_yd = NA,
           rec_td = NA,
           two_pt = NA,
           fg_50 = NA,
           fg_40_49 = NA,
           fg_39 = NA,
           missed_fg_40 = NA,
           missed_fg_39_less = NA,
           def_td = NA,
           def_int = NA,
           def_fum = NA,
           def_block = NA,
           def_safety = NA,
           def_sack = NA,
           def_0 = NA,
           def_1_6 = NA,
           def_7_13 = NA,
           def_14_20 = NA,
           def_21_27 = NA,
           def_28_34 = NA,
           def_35 = NA,
           def_avg = NA,
           pa = NA,
           pa_miss = NA,
           bye = NA) %>%
    select(player, team, position, source, pass_td, pass_yd, int, fum, rush_yd, rush_td, rec, rec_yd, rec_td,
           two_pt, fg_50, fg_40_49, fg_39, missed_fg_40, missed_fg_39_less, def_td, def_int, def_fum, def_block,
           def_safety, def_sack, def_0, def_1_6, def_7_13, def_14_20, def_21_27, def_28_34, def_35, pa, pa_miss, def_avg, bye) %>%
    mutate_if(is.logical, as.numeric) 
  
  Sys.sleep(2)
  
  print("Fantasy Pros - RB")
  
  # rb_link <- read_html("https://www.fantasypros.com/nfl/projections/rb.php?week=17")
  rb_link <- read_html("https://www.fantasypros.com/nfl/projections/rb.php?week=0")
  
  rb_player <- rb_link %>%
    html_nodes(".player-name") %>%
    html_text()
  
  rb_team <- rb_link %>%
    html_nodes(".js-tr-game-select .player-label") %>%
    html_text() %>%
    substr(nchar(.)-3, nchar(.)-1) %>%
    str_remove(" ")
  
  rb_table <- rb_link %>%
    html_nodes(xpath = '//*[@id="data"]') %>%
    html_table()
  
  rb_table <- rb_table[[1]] %>%
    header.true() %>%
    header.true()
  
  rb_table$Player <- rb_player
  rb_table$team <- rb_team
  colnames(rb_table) <- c("player", "rush_att", "rush_yd", "rush_td", "rec", "rec_yd", "rec_td","fum", "fpts", "team")
  rb_table <- rb_table %>%
    select(player, team, rush_yd, rush_td, rec, rec_yd, rec_td, fum) %>%
    mutate(rec_yd = str_remove_all(rec_yd, ","),
           rush_yd = str_remove_all(rush_yd, ","))
  
  num_cols <- colnames(rb_table)
  num_cols <- num_cols[-(1:2)]
  
  rb_table[num_cols] <- sapply(rb_table[num_cols],as.numeric)
  
  rb_table <- rb_table %>%
    mutate(player = str_remove(player, "'"),
           player = str_remove_all(player, "\\."),
           player = str_remove_all(player, " Jr"),
           player = str_remove_all(player, " IV"),
           player = str_remove_all(player, " III"),
           player = str_remove_all(player, " II"),
           player = tolower(player),
           position = "RB",
           source = "Fantasy Pros/Yahoo",
           pass_td = NA, 
           pass_yd = NA, 
           int = NA,
           two_pt = NA,
           fg_50 = NA,
           fg_40_49 = NA,
           fg_39 = NA,
           missed_fg_40 = NA,
           missed_fg_39_less = NA,
           def_td = NA,
           def_int = NA,
           def_fum = NA,
           def_block = NA,
           def_safety = NA,
           def_sack = NA,
           def_0 = NA,
           def_1_6 = NA,
           def_7_13 = NA,
           def_14_20 = NA,
           def_21_27 = NA,
           def_28_34 = NA,
           def_35 = NA,
           def_avg = NA,
           pa = NA,
           pa_miss = NA,
           bye = NA) %>%
    select(player, team, position, source, pass_td, pass_yd, int, fum, rush_yd, rush_td, rec, rec_yd, rec_td,
           two_pt, fg_50, fg_40_49, fg_39, missed_fg_40, missed_fg_39_less, def_td, def_int, def_fum, def_block,
           def_safety, def_sack, def_0, def_1_6, def_7_13, def_14_20, def_21_27, def_28_34, def_35, pa, pa_miss, def_avg, bye) %>%
    mutate_if(is.logical, as.numeric) 
  
  
  Sys.sleep(2)
  
  print("Fantasy Pros - WR")
  
  # wr_link <- read_html("https://www.fantasypros.com/nfl/projections/wr.php?week=17")
  wr_link <- read_html("https://www.fantasypros.com/nfl/projections/wr.php?week=0")
  
  wr_player <- wr_link %>%
    html_nodes(".player-name") %>%
    html_text()
  
  wr_team <- wr_link %>%
    html_nodes(".js-tr-game-select .player-label") %>%
    html_text() %>%
    substr(nchar(.)-3, nchar(.)-1) %>%
    str_remove(" ")
  
  wr_table <- wr_link %>%
    html_nodes(xpath = '//*[@id="data"]') %>%
    html_table()
  
  wr_table <- wr_table[[1]] %>%
    header.true() %>%
    header.true()
  
  wr_table$Player <- wr_player
  wr_table$team <- wr_team
  colnames(wr_table) <- c("player", "rec", "rec_yd", "rec_td", "rush_att", "rush_yd", "rush_td","fum", "fpts", "team")
  wr_table <- wr_table %>%
    select(player, team, rush_yd, rush_td, rec, rec_yd, rec_td, fum) %>%
    mutate(rec_yd = str_remove_all(rec_yd, ","),
           rush_yd = str_remove_all(rush_yd, ","))
  
  num_cols <- colnames(wr_table)
  num_cols <- num_cols[-(1:2)]
  
  wr_table[num_cols] <- sapply(wr_table[num_cols],as.numeric)
  
  wr_table <- wr_table %>%
    mutate(player = str_remove(player, "'"),
           player = str_remove_all(player, "\\."),
           player = str_remove_all(player, " Jr"),
           player = str_remove_all(player, " IV"),
           player = str_remove_all(player, " III"),
           player = str_remove_all(player, " II"),
           player = tolower(player),
           position = "WR",
           source = "Fantasy Pros/Yahoo",
           pass_td = NA, 
           pass_yd = NA, 
           int = NA,
           two_pt = NA,
           fg_50 = NA,
           fg_40_49 = NA,
           fg_39 = NA,
           missed_fg_40 = NA,
           missed_fg_39_less = NA,
           def_td = NA,
           def_int = NA,
           def_fum = NA,
           def_block = NA,
           def_safety = NA,
           def_sack = NA,
           def_0 = NA,
           def_1_6 = NA,
           def_7_13 = NA,
           def_14_20 = NA,
           def_21_27 = NA,
           def_28_34 = NA,
           def_35 = NA,
           def_avg = NA,
           pa = NA,
           pa_miss = NA,
           bye = NA) %>%
    select(player, team, position, source, pass_td, pass_yd, int, fum, rush_yd, rush_td, rec, rec_yd, rec_td,
           two_pt, fg_50, fg_40_49, fg_39, missed_fg_40, missed_fg_39_less, def_td, def_int, def_fum, def_block,
           def_safety, def_sack, def_0, def_1_6, def_7_13, def_14_20, def_21_27, def_28_34, def_35, pa, pa_miss, def_avg, bye) %>%
    mutate_if(is.logical, as.numeric) 
  
  Sys.sleep(2)
  
  print("Fantasy Pros - TE")
  
  # te_link <- read_html("https://www.fantasypros.com/nfl/projections/te.php?week=17")
  te_link <- read_html("https://www.fantasypros.com/nfl/projections/te.php?week=0")
  
  te_player <- te_link %>%
    html_nodes(".player-name") %>%
    html_text()
  
  te_team <- te_link %>%
    html_nodes(".js-tr-game-select .player-label") %>%
    html_text() %>%
    substr(nchar(.)-3, nchar(.)-1) %>%
    str_remove(" ")
  
  te_table <- te_link %>%
    html_nodes(xpath = '//*[@id="data"]') %>%
    html_table()
  
  te_table <- te_table[[1]] %>%
    header.true() %>%
    header.true()
  
  te_table$Player <- te_player
  te_table$team <- te_team
  colnames(te_table) <- c("player", "rec", "rec_yd", "rec_td", "fum", "fpts", "team")
  te_table <- te_table %>%
    select(player, team, rec, rec_yd, rec_td, fum) %>%
    mutate(rec_yd = str_remove_all(rec_yd, ","))
  
  num_cols <- colnames(te_table)
  num_cols <- num_cols[-(1:2)]
  
  te_table[num_cols] <- sapply(te_table[num_cols],as.numeric)
  
  te_table <- te_table %>%
    mutate(player = str_remove(player, "'"),
           player = str_remove_all(player, "\\."),
           player = str_remove_all(player, " Jr"),
           player = str_remove_all(player, " IV"),
           player = str_remove_all(player, " III"),
           player = str_remove_all(player, " II"),
           player = tolower(player),
           position = "TE",
           source = "Fantasy Pros/Yahoo",
           rush_yd = NA, 
           rush_td = NA, 
           pass_td = NA, 
           pass_yd = NA, 
           int = NA,
           two_pt = NA,
           fg_50 = NA,
           fg_40_49 = NA,
           fg_39 = NA,
           missed_fg_40 = NA,
           missed_fg_39_less = NA,
           def_td = NA,
           def_int = NA,
           def_fum = NA,
           def_block = NA,
           def_safety = NA,
           def_sack = NA,
           def_0 = NA,
           def_1_6 = NA,
           def_7_13 = NA,
           def_14_20 = NA,
           def_21_27 = NA,
           def_28_34 = NA,
           def_35 = NA,
           def_avg = NA,
           pa = NA,
           pa_miss = NA,
           bye = NA) %>%
    select(player, team, position, source, pass_td, pass_yd, int, fum, rush_yd, rush_td, rec, rec_yd, rec_td,
           two_pt, fg_50, fg_40_49, fg_39, missed_fg_40, missed_fg_39_less, def_td, def_int, def_fum, def_block,
           def_safety, def_sack, def_0, def_1_6, def_7_13, def_14_20, def_21_27, def_28_34, def_35, pa, pa_miss, def_avg, bye) %>%
    mutate_if(is.logical, as.numeric) 
  
  Sys.sleep(2)
  
  print("Fantasy Pros - DST")
  
  # dst_link <- read_html("https://www.fantasypros.com/nfl/projections/dst.php?week=17")
  dst_link <- read_html("https://www.fantasypros.com/nfl/projections/dst.php?week=0")
  
  dst_player <- dst_link %>%
    html_nodes(".player-name") %>%
    html_text()
  
  dst_table <- dst_link %>%
    html_nodes(xpath = '//*[@id="data"]') %>%
    html_table()
  
  dst_table <- dst_table[[1]]
  
  dst_table$Player <- dst_player
  colnames(dst_table) <- c("player", "def_sack", "def_int", "def_fum", "def_ff", "def_td", "def_safety", "pa", "yds_against", "fpts")
  dst_table <- dst_table %>%
    select(player, def_sack, def_int, def_fum, def_td, def_safety, pa)
  
  num_cols <- colnames(dst_table)
  num_cols <- num_cols[-(1)]
  
  dst_table[num_cols] <- sapply(dst_table[num_cols],as.numeric)
  
  dst_table <- dst_table %>%
    mutate(player = str_remove_all(player, "'"),
           player = str_remove_all(player, " III"),
           player = str_remove_all(player, "\\."),
           player = tolower(player),
           player = str_remove_all(player, " jr"),
           player = case_when(
             player == "arizona cardinals" ~ "arizona",
             player == "atlanta falcons" ~ "atlanta",
             player == "baltimore ravens" ~ "baltimore",
             player == "buffalo bills" ~ "buffalo",
             player == "carolina panthers" ~ "carolina",
             player == "chicago bears" ~ "chicago",
             player == "cincinnati bengals" ~ "cincinnati",
             player == "cleveland browns" ~ "cleveland",
             player == "dallas cowboys" ~ "dallas",
             player == "denver broncos" ~ "denver",
             player == "detroit lions" ~ "detroit",
             player == "green bay packers" ~ "green bay",
             player == "houston texans" ~ "houston",
             player == "indianapolis colts" ~ "indianapolis",
             player == "jacksonville jaguars" ~ "jacksonville",
             player == "kansas city chiefs" ~ "kansas city",
             player == "las vegas raiders" ~ "las vegas",
             player == "los angeles chargers" ~ "la chargers",
             player == "los angeles rams" ~ "la rams",
             player == "miami dolphins" ~ "miami",
             player == "minnesota vikings" ~ "minnesota",
             player == "new england patriots" ~ "new england",
             player == "new orleans saints" ~ "new orleans",
             player == "new york giants" ~ "ny giants",
             player == "new york jets" ~ "ny jets",
             player == "philadelphia eagles" ~ "philadelphia",
             player == "pittsburgh steelers" ~ "pittsburgh",
             player == "san francisco 49ers" ~ "san francisco",
             player == "seattle seahawks" ~ "seattle",
             player == "tampa bay buccaneers" ~ "tampa bay",
             player == "tennessee titans" ~ "tennessee",
             player == "washington football team" ~ "washington",
             TRUE ~ player
           ),
           def_avg = pa/17,
           position = "DST",
           source = "Fantasy Pros/Yahoo",
           team = NA,
           rush_yd = NA, 
           rush_td = NA, 
           pass_td = NA, 
           pass_yd = NA,
           fum = NA,
           rec = NA, 
           rec_yd = NA, 
           rec_td = NA,
           int = NA,
           two_pt = NA,
           fg_50 = NA,
           fg_40_49 = NA,
           fg_39 = NA,
           missed_fg_40 = NA,
           missed_fg_39_less = NA,
           def_block = NA,
           def_0 = NA,
           def_1_6 = NA,
           def_7_13 = NA,
           def_14_20 = NA,
           def_21_27 = NA,
           def_28_34 = NA,
           def_35 = NA,
           pa = NA,
           pa_miss = NA,
           bye = NA) %>%
    select(player, team, position, source, pass_td, pass_yd, int, fum, rush_yd, rush_td, rec, rec_yd, rec_td,
           two_pt, fg_50, fg_40_49, fg_39, missed_fg_40, missed_fg_39_less, def_td, def_int, def_fum, def_block,
           def_safety, def_sack, def_0, def_1_6, def_7_13, def_14_20, def_21_27, def_28_34, def_35, pa, pa_miss, def_avg, bye) %>%
    mutate_if(is.logical, as.numeric) 
  
  
  Sys.sleep(2)
  
  fantasy_pros_scrape <- qb_table %>%
    rbind(rb_table) %>%
    rbind(wr_table) %>%
    rbind(te_table) %>%
    rbind(dst_table)
  
  return(fantasy_pros_scrape)
}
fantasy_pros_scrape <- scrape_fantasy_pros()


#### Fantasy Sharks ####
scrape_fantasy_sharks <- function(){
  print("Fantasy Sharks - QB/RB/WR/TE")
  
  player_table <- fromJSON("https://www.fantasysharks.com/apps/Projections/SeasonProjections.php?pos=ALL&format=json")
  
  num_cols <- colnames(player_table)
  num_cols <- num_cols[c(-1, -2, -3, -4, -5, -6)]
  
  player_table[num_cols] <- sapply(player_table[num_cols],as.numeric)
  player_table <- player_table[c(4,5,6,7,9,10,11,13,14,15,16,17,18)]
  colnames(player_table) <- c("player", "position", "team", "bye", "pass_yd", "pass_td", "int", "rush_yd", "rush_td", "fum",
                              "rec", "rec_yd", "rec_td")
  
  player_table <- player_table %>%
    mutate(first_name = sapply(strsplit(player, ", "), `[`, 2),
           last_name = sapply(strsplit(player, ", "), `[`, 1),
           player = paste0(first_name, " ", last_name),
           player = str_remove(player, "'"),
           player = str_remove_all(player, "\\."),
           player = str_remove_all(player, " Jr"),
           player = str_remove_all(player, " IV"),
           player = str_remove_all(player, " III"),
           player = str_remove_all(player, " II"),
           player = tolower(player),
           source = "Fantasy Sharks",
           two_pt = NA,
           fg_50 = NA,
           fg_40_49 = NA,
           fg_39 = NA,
           missed_fg_40 = NA,
           missed_fg_39_less = NA,
           def_td = NA,
           def_int = NA,
           def_fum = NA,
           def_block = NA,
           def_safety = NA,
           def_sack = NA,
           def_0 = NA,
           def_1_6 = NA,
           def_7_13 = NA,
           def_14_20 = NA,
           def_21_27 = NA,
           def_28_34 = NA,
           def_35 = NA,
           def_avg = NA,
           pa = NA,
           pa_miss = NA) %>%
    select(player, team, position, source, pass_td, pass_yd, int, fum, rush_yd, rush_td, rec, rec_yd, rec_td,
           two_pt, fg_50, fg_40_49, fg_39, missed_fg_40, missed_fg_39_less, def_td, def_int, def_fum, def_block,
           def_safety, def_sack, def_0, def_1_6, def_7_13, def_14_20, def_21_27, def_28_34, def_35, pa, pa_miss, def_avg, bye) %>%
    mutate_if(is.logical, as.numeric)
  
  print("Fantasy Sharks - DST")
  
  Sys.sleep(2)
  
  dst_table <- fromJSON("https://www.fantasysharks.com/apps/Projections/SeasonProjections.php?pos=D&format=json")
  
  num_cols <- colnames(dst_table)
  num_cols <- num_cols[c(-1, -2, -3, -4, -5)]
  
  dst_table[num_cols] <- sapply(dst_table[num_cols],as.numeric)
  dst_table <- dst_table[c(4,5,6,7,8,9,10,11)]
  colnames(dst_table) <- c("player", "team", "bye", "def_td", "def_int", "def_fum", "def_sack", "pa")
  
  dst_table <- dst_table %>%
    mutate(first_name = sapply(strsplit(player, ", "), `[`, 2),
           last_name = sapply(strsplit(player, ", "), `[`, 1),
           player = paste0(first_name, " ", last_name),
           player = str_remove(player, "'"),
           player = str_remove_all(player, "\\."),
           player = str_remove_all(player, " Jr"),
           player = str_remove_all(player, " IV"),
           player = str_remove_all(player, " III"),
           player = str_remove_all(player, " II"),
           player = tolower(player),
           player = case_when(
             player == "arizona cardinals" ~ "arizona",
             player == "atlanta falcons" ~ "atlanta",
             player == "baltimore ravens" ~ "baltimore",
             player == "buffalo bills" ~ "buffalo",
             player == "carolina panthers" ~ "carolina",
             player == "chicago bears" ~ "chicago",
             player == "cincinnati bengals" ~ "cincinnati",
             player == "cleveland browns" ~ "cleveland",
             player == "dallas cowboys" ~ "dallas",
             player == "denver broncos" ~ "denver",
             player == "detroit lions" ~ "detroit",
             player == "green bay packers" ~ "green bay",
             player == "houston texans" ~ "houston",
             player == "indianapolis colts" ~ "indianapolis",
             player == "jacksonville jaguars" ~ "jacksonville",
             player == "kansas city chiefs" ~ "kansas city",
             grepl("raiders", player) ~ "las vegas",
             player == "los angeles chargers" ~ "la chargers",
             player == "los angeles rams" ~ "la rams",
             player == "miami dolphins" ~ "miami",
             player == "minnesota vikings" ~ "minnesota",
             player == "new england patriots" ~ "new england",
             player == "new orleans saints" ~ "new orleans",
             player == "new york giants" ~ "ny giants",
             player == "new york jets" ~ "ny jets",
             player == "philadelphia eagles" ~ "philadelphia",
             player == "pittsburgh steelers" ~ "pittsburgh",
             player == "san francisco 49ers" ~ "san francisco",
             player == "seattle seahawks" ~ "seattle",
             player == "tampa bay buccaneers" ~ "tampa bay",
             player == "tennessee titans" ~ "tennessee",
             player == "washington football team" ~ "washington",
             TRUE ~ player
           ),
           position = "DST",
           source = "Fantasy Sharks",
           def_avg = pa/17,
           rush_yd = NA, 
           rush_td = NA, 
           pass_td = NA, 
           pass_yd = NA,
           int = NA,
           fum = NA,
           rec = NA, 
           rec_yd = NA, 
           rec_td = NA,
           two_pt = NA,
           fg_50 = NA,
           fg_40_49 = NA,
           fg_39 = NA,
           missed_fg_40 = NA,
           missed_fg_39_less = NA,
           def_block = NA,
           def_safety = NA,
           def_0 = NA,
           def_1_6 = NA,
           def_7_13 = NA,
           def_14_20 = NA,
           def_21_27 = NA,
           def_28_34 = NA,
           def_35 = NA,
           pa = NA,
           pa_miss = NA) %>%
    select(player, team, position, source, pass_td, pass_yd, int, fum, rush_yd, rush_td, rec, rec_yd, rec_td,
           two_pt, fg_50, fg_40_49, fg_39, missed_fg_40, missed_fg_39_less, def_td, def_int, def_fum, def_block,
           def_safety, def_sack, def_0, def_1_6, def_7_13, def_14_20, def_21_27, def_28_34, def_35, pa, pa_miss, def_avg, bye) %>%
    mutate_if(is.logical, as.numeric)
  
  fs_scrape <- player_table %>%
    rbind(dst_table)
  
  return(fs_scrape)
}
fantasy_sharks_scrape <- scrape_fantasy_sharks()

#### PFF ####
scrape_pff <- function(){
  print("Pro Football Focus - All")

  pff_scrape <- read_csv("pff_season_proj_2021.csv") %>%
    mutate(missed_fg_39_less = (fgAtt019+fgAtt2029+fgAtt3039)-(fgMade019+fgMade2029+fgMade3039),
           missed_fg_40 = (fgAtt4049+fgAtt50plus)-(fgMade4049+fgMade50plus),
           fg_50 = fgMade50plus,
           fg_40_49 = fgMade4049,
           fg_39 = (fgMade019+fgMade2029+fgMade3039),
           pa = patMade,
           pa_miss = patAtt-patMade)
  pff_scrape <- pff_scrape[c(2,3,4,5,11,12,13,16,17,19,20,21,23,24,39,40,41,43,44,46,47,48,
               49,50,51,52,53,64,65,66,67,68,69,70)] %>%
    mutate(dstTd = dstTd+dstReturnTd)
  
  pff_scrape <- pff_scrape[-20]
  
  colnames(pff_scrape) <- c("player", "team", "position", "bye", "pass_yd", "pass_td", "int", "rush_yd", "rush_td",
                            "rec", "rec_yd", "rec_td", "fum", "two_pt", "def_sack", "def_safety", "def_int",
                            "def_fum", "def_td", "def_0", "def_1_6", "def_7_13", "def_14_20", "def_21_27",
                            "def_28_34", "def_35", "missed_fg_39_less", "missed_fg_40", "fg_50", "fg_40_49", "fg_39",
                            "pa", "pa_miss")
  
  pff_scrape <- pff_scrape %>%
    mutate(player= ifelse(position == "dst", team, player),
           position = toupper(position)) %>%
    mutate(player = str_remove_all(player, "'"),
           player = str_remove_all(player, " III"),
           player = str_remove_all(player, " II"),
           player = str_remove_all(player, "\\."),
           player = tolower(player),
           player = str_remove_all(player, " jr"),
           player = case_when(
             player == "arz" ~ "arizona",
             player == "atl" ~ "atlanta",
             player == "blt" ~ "baltimore",
             player == "buf" ~ "buffalo",
             player == "car" ~ "carolina",
             player == "chi" ~ "chicago",
             player == "cin" ~ "cincinnati",
             player == "clv" ~ "cleveland",
             player == "dal" ~ "dallas",
             player == "den" ~ "denver",
             player == "det" ~ "detroit",
             player == "gb" ~ "green bay",
             player == "hst" ~ "houston",
             player == "ind" ~ "indianapolis",
             player == "jax" ~ "jacksonville",
             player == "kc" ~ "kansas city",
             player == "lv" ~ "las vegas",
             player == "lac" ~ "la chargers",
             player == "la" ~ "la rams",
             player == "mia" ~ "miami",
             player == "min" ~ "minnesota",
             player == "ne" ~ "new england",
             player == "no" ~ "new orleans",
             player == "nyg" ~ "ny giants",
             player == "nyj" ~ "ny jets",
             player == "phi" ~ "philadelphia",
             player == "pit" ~ "pittsburgh",
             player == "sf" ~ "san francisco",
             player == "sea" ~ "seattle",
             player == "tb" ~ "tampa bay",
             player == "ten" ~ "tennessee",
             player == "was" ~ "washington",
             TRUE ~ player
           ),
           source = "PFF",
           def_block = NA,
           def_avg = NA) %>%
    select(player, team, position, source, pass_td, pass_yd, int, fum, rush_yd, rush_td, rec, rec_yd, rec_td,
           two_pt, fg_50, fg_40_49, fg_39, missed_fg_40, missed_fg_39_less, def_td, def_int, def_fum, def_block,
           def_safety, def_sack, def_0, def_1_6, def_7_13, def_14_20, def_21_27, def_28_34, def_35, pa, pa_miss, def_avg, bye) %>%
    mutate_if(is.logical, as.numeric)

  return(pff_scrape)
}
pff_scrape <- scrape_pff()

#### NFL ####
scrape_nfl <- function(){
  print("NFL.com - Player")
  
  player_link <- read_html("https://fantasy.nfl.com/research/projections?position=O&sort=projectedPts&statCategory=projectedStats&statSeason=2021&statType=seasonProjectedStats")
  player_table <- xml_child(xml_child(xml_child(xml_child(xml_child(xml_child(xml_child(xml_child(xml_child(xml_child(xml_child(player_link, 2), 1), 3), 1), 2), 1), 1), 1), 2), 1), 1) %>%
    html_table() %>%
    header.true()
  
  player_player <- player_link %>%
    html_nodes(".what-playerCard") %>%
    html_text()
  
  player_team <- player_link %>%
    html_nodes("em") %>%
    html_text() %>%
    substr(nchar(.)-2, nchar(.)) %>%
    str_remove(" ") %>%
    .[9:33]
  
  player_position <- player_link %>%
    html_nodes("em") %>%
    html_text() %>%
    substr(1,2) %>%
    .[9:33]
  
  player_player <- player_player[!grepl("View News", player_player)]
  
  player_table$Player <- player_player
  player_table$team <- player_team
  player_table$position <- player_position
  player_table <- player_table[,c(1,17,18,4,5,6,7,8,9,10,11,14,15)]
  colnames(player_table) <- c("player", "team", "position", "pass_yd", "pass_td", "int", "rush_yd", "rush_td", "rec", "rec_yd", "rec_td", "two_pt",
                              "fum")
  
  for(i in 1:35) {
    Sys.sleep(1)
    n <- (i*25) + 1
    player_link_temp <- read_html(paste0("https://fantasy.nfl.com/research/projections?offset=",n,"&position=O&sort=projectedPts&statCategory=projectedStats&statSeason=2021&statType=seasonProjectedStats&statWeek=1"))
    
    player_table_temp <- xml_child(xml_child(xml_child(xml_child(xml_child(xml_child(xml_child(xml_child(xml_child(xml_child(xml_child(player_link_temp, 2), 1), 3), 1), 2), 1), 1), 1), 2), 1), 1) %>%
      html_table() %>%
      header.true()
    
    player_player_temp <- player_link_temp %>%
      html_nodes(".what-playerCard") %>%
      html_text()
    
    player_team_temp <- player_link_temp %>%
      html_nodes("em") %>%
      html_text() %>%
      substr(nchar(.)-2, nchar(.)) %>%
      str_remove(" ") %>%
      .[9:33]
    
    
    player_position_temp <- player_link_temp %>%
      html_nodes("em") %>%
      html_text() %>%
      substr(1,2) %>%
      .[9:33]
    
    player_player_temp <- player_player_temp[!grepl("View News", player_player_temp)]
    
    player_table_temp$Player <- player_player_temp
    player_table_temp$team <- player_team_temp
    player_table_temp$position <- player_position_temp
    player_table_temp <- player_table_temp[,c(1,17,18,4,5,6,7,8,9,10,11,14,15)]
    colnames(player_table_temp) <- c("player",  "team", "position","pass_yd", "pass_td", "int", "rush_yd", "rush_td", "rec", "rec_yd", "rec_td", "two_pt",
                                     "fum")
    
    player_table <- rbind(player_table, player_table_temp)
  }
  
  num_cols <- colnames(player_table)
  num_cols <- num_cols[-c(1,2,3)]
  
  player_table[num_cols] <- sapply(player_table[num_cols],as.numeric)
  
  player_table[num_cols] <- sapply(player_table[num_cols],replace_na, 0)
  
  player_table <- player_table %>%
    mutate(player = str_remove(player, "'"),
           player = str_remove_all(player, "\\."),
           player = str_remove_all(player, " Jr"),
           player = str_remove_all(player, " IV"),
           player = str_remove_all(player, " III"),
           player = str_remove_all(player, " II"),
           player = tolower(player),
           source = "NFL",
           bye = NA,
           fg_50 = NA,
           fg_40_49 = NA,
           fg_39 = NA,
           missed_fg_40 = NA,
           missed_fg_39_less = NA,
           def_td = NA,
           def_int = NA,
           def_fum = NA,
           def_block = NA,
           def_safety = NA,
           def_sack = NA,
           def_0 = NA,
           def_1_6 = NA,
           def_7_13 = NA,
           def_14_20 = NA,
           def_21_27 = NA,
           def_28_34 = NA,
           def_35 = NA,
           def_avg = NA,
           pa = NA,
           pa_miss = NA) %>%
    select(player, team, position, source, pass_td, pass_yd, int, fum, rush_yd, rush_td, rec, rec_yd, rec_td,
           two_pt, fg_50, fg_40_49, fg_39, missed_fg_40, missed_fg_39_less, def_td, def_int, def_fum, def_block,
           def_safety, def_sack, def_0, def_1_6, def_7_13, def_14_20, def_21_27, def_28_34, def_35, pa, pa_miss, def_avg, bye) %>%
    mutate_if(is.logical, as.numeric)
  
  Sys.sleep(2)
  
  print("NFL.com - DST")
  
  dst_link <- read_html("https://fantasy.nfl.com/research/projections?position=8&statCategory=projectedStats&statSeason=2021&statType=seasonProjectedStats&statWeek=1")
  
  dst_table <- xml_child(xml_child(xml_child(xml_child(xml_child(xml_child(xml_child(xml_child(xml_child(xml_child(xml_child(dst_link, 2), 1), 3), 1), 2), 1), 1), 1), 2), 1), 1) %>%
    html_table() %>%
    header.true()
  
  dst_player <- dst_link %>%
    html_nodes(".what-playerCard") %>%
    html_text()
  
  dst_player <- dst_player[!grepl("View News", dst_player)]
  
  dst_table$Player <- dst_player
  
  dst_table <- dst_table[,c(13,4,5,6,7,8,10,11)]
  
  colnames(dst_table) <- c("player", "def_sack", "def_int", "def_fum", "def_safety", "def_td", "def_ret_td", "pts_allow")
  
  for(i in 1) {
    Sys.sleep(1)
    n <- (i*25) + 1
    dst_link_temp <- read_html(paste0("https://fantasy.nfl.com/research/projections?offset=",n,"&position=8&sort=projectedPts&statCategory=projectedStats&statSeason=2021&statType=seasonProjectedStats&statWeek=1"))
    
    dst_table_temp <- xml_child(xml_child(xml_child(xml_child(xml_child(xml_child(xml_child(xml_child(xml_child(xml_child(xml_child(dst_link_temp, 2), 1), 3), 1), 2), 1), 1), 1), 2), 1), 1) %>%
      html_table() %>%
      header.true()
    
    dst_player_temp <- dst_link_temp %>%
      html_nodes(".what-playerCard") %>%
      html_text()
    
    dst_player_temp <- dst_player_temp[!grepl("View News", dst_player_temp)]
    
    dst_table_temp$Player <- dst_player_temp
    
    dst_table_temp <- dst_table_temp[,c(13,4,5,6,7,8,10,11)]
    
    colnames(dst_table_temp) <- c("player", "def_sack", "def_int", "def_fum", "def_safety", "def_td", "def_ret_td", "pts_allow")
    
    dst_table <- rbind(dst_table, dst_table_temp)
  }
  
  num_cols <- colnames(dst_table)
  num_cols <- num_cols[c(-1)]
  
  dst_table[num_cols] <- sapply(dst_table[num_cols],as.numeric)
  
  dst_table[num_cols] <- sapply(dst_table[num_cols],replace_na, 0)
  
  dst_table <- dst_table %>%
    mutate(player = str_remove(player, "'"),
           player = str_remove_all(player, "\\."),
           player = str_remove_all(player, " Jr"),
           player = str_remove_all(player, " IV"),
           player = str_remove_all(player, " III"),
           player = str_remove_all(player, " II"),
           player = tolower(player),
           player = case_when(
             player == "arizona cardinals" ~ "arizona",
             player == "atlanta falcons" ~ "atlanta",
             player == "baltimore ravens" ~ "baltimore",
             player == "buffalo bills" ~ "buffalo",
             player == "carolina panthers" ~ "carolina",
             player == "chicago bears" ~ "chicago",
             player == "cincinnati bengals" ~ "cincinnati",
             player == "cleveland browns" ~ "cleveland",
             player == "dallas cowboys" ~ "dallas",
             player == "denver broncos" ~ "denver",
             player == "detroit lions" ~ "detroit",
             player == "green bay packers" ~ "green bay",
             player == "houston texans" ~ "houston",
             player == "indianapolis colts" ~ "indianapolis",
             player == "jacksonville jaguars" ~ "jacksonville",
             player == "kansas city chiefs" ~ "kansas city",
             player == "las vegas raiders" ~ "las vegas",
             player == "los angeles chargers" ~ "la chargers",
             player == "los angeles rams" ~ "la rams",
             player == "miami dolphins" ~ "miami",
             player == "minnesota vikings" ~ "minnesota",
             player == "new england patriots" ~ "new england",
             player == "new orleans saints" ~ "new orleans",
             player == "new york giants" ~ "ny giants",
             player == "new york jets" ~ "ny jets",
             player == "philadelphia eagles" ~ "philadelphia",
             player == "pittsburgh steelers" ~ "pittsburgh",
             player == "san francisco 49ers" ~ "san francisco",
             player == "seattle seahawks" ~ "seattle",
             player == "tampa bay buccaneers" ~ "tampa bay",
             player == "tennessee titans" ~ "tennessee",
             player == "washington football team" ~ "washington",
             TRUE ~ player
           ),
           position = "DST",
           source = "NFL",
           team = NA,
           team = as.character(team),
           def_avg = pts_allow/17,
           def_td = def_td+def_ret_td,
           rush_yd = NA, 
           bye = NA,
           rush_td = NA, 
           pass_td = NA, 
           pass_yd = NA,
           int = NA,
           fum = NA,
           rec = NA, 
           rec_yd = NA, 
           rec_td = NA,
           two_pt = NA,
           fg_50 = NA,
           fg_40_49 = NA,
           fg_39 = NA,
           missed_fg_40 = NA,
           missed_fg_39_less = NA,
           def_block = NA,
           def_safety = NA,
           def_0 = NA,
           def_1_6 = NA,
           def_7_13 = NA,
           def_14_20 = NA,
           def_21_27 = NA,
           def_28_34 = NA,
           def_35 = NA,
           pa = NA,
           pa_miss = NA) %>%
    select(player, team, position, source, pass_td, pass_yd, int, fum, rush_yd, rush_td, rec, rec_yd, rec_td,
           two_pt, fg_50, fg_40_49, fg_39, missed_fg_40, missed_fg_39_less, def_td, def_int, def_fum, def_block,
           def_safety, def_sack, def_0, def_1_6, def_7_13, def_14_20, def_21_27, def_28_34, def_35, pa, pa_miss, def_avg, bye) %>%
    mutate_if(is.logical, as.numeric)
  
  nfl_scrape <- rbind(player_table, dst_table)
  
  return(nfl_scrape)
}
nfl_scrape <- scrape_nfl()

#### CBS ####
scrape_cbs <- function(){
  print("CBS - RB")
  
  rbLink <- read_html("https://www.cbssports.com/fantasy/football/stats/RB/2021/season/projections/nonppr/")
  
  rbPlayer <- rbLink %>%
    html_nodes(".CellPlayerName--long a") %>%
    html_text()
  
  rbRushYds <- rbLink %>%
    html_nodes(".TableBase-bodyTd--number:nth-child(4)") %>%
    html_text() %>%
    as.numeric()
  
  rbRushTds <- rbLink %>%
    html_nodes(".TableBase-bodyTd--number:nth-child(6)") %>%
    html_text() %>%
    as.numeric()
  
  rbRec <- rbLink %>%
    html_nodes(".TableBase-bodyTd--number:nth-child(8)") %>%
    html_text() %>%
    as.numeric()
  
  rbRecYds <- rbLink %>%
    html_nodes(".TableBase-bodyTd--number:nth-child(9)") %>%
    html_text() %>%
    as.numeric()
  
  rbRecTds <- rbLink %>%
    html_nodes(".TableBase-bodyTd--number:nth-child(12)") %>%
    html_text() %>%
    as.numeric()
  
  rbFumb <- rbLink %>%
    html_nodes(".TableBase-bodyTd--number:nth-child(13)") %>%
    html_text() %>%
    as.numeric()
  
  rbTeam <- rbLink %>%
    html_nodes(".CellPlayerName-team") %>%
    html_text() %>%
    str_remove("\n                        ") %>%
    str_remove("\n                    ") %>%
    str_remove_all(" ") %>%
    .[seq(from = 1, to = length(.)-1, by = 2)]
  
  rbTable <- data.frame("player" = rbPlayer,
                        "position" = "RB",
                        "team" = rbTeam,
                        "rush_yd" = rbRushYds,
                        "rush_td" = rbRushTds,
                        "rec" = rbRec,
                        "rec_yd" = rbRecYds,
                        "rec_td" = rbRecTds,
                        "fum" = rbFumb)
  
  num_cols <- colnames(rbTable)
  num_cols <- num_cols[-(1:3)]
  
  rbTable[num_cols] <- sapply(rbTable[num_cols],as.numeric)
  
  rbTable <- rbTable %>%
    mutate(player = str_remove(player, "'"),
           player = str_remove_all(player, "\\."),
           player = str_remove_all(player, " Jr"),
           player = str_remove_all(player, " IV"),
           player = str_remove_all(player, " III"),
           player = str_remove_all(player, " II"),
           player = tolower(player),
           position = "RB",
           source = "CBS",
           pass_td = NA, 
           pass_yd = NA, 
           int = NA,
           two_pt = NA,
           fg_50 = NA,
           fg_40_49 = NA,
           fg_39 = NA,
           missed_fg_40 = NA,
           missed_fg_39_less = NA,
           def_td = NA,
           def_int = NA,
           def_fum = NA,
           def_block = NA,
           def_safety = NA,
           def_sack = NA,
           def_0 = NA,
           def_1_6 = NA,
           def_7_13 = NA,
           def_14_20 = NA,
           def_21_27 = NA,
           def_28_34 = NA,
           def_35 = NA,
           def_avg = NA,
           pa = NA,
           pa_miss = NA,
           bye = NA) %>%
    select(player, team, position, source, pass_td, pass_yd, int, fum, rush_yd, rush_td, rec, rec_yd, rec_td,
           two_pt, fg_50, fg_40_49, fg_39, missed_fg_40, missed_fg_39_less, def_td, def_int, def_fum, def_block,
           def_safety, def_sack, def_0, def_1_6, def_7_13, def_14_20, def_21_27, def_28_34, def_35, pa, pa_miss, def_avg, bye) %>%
    mutate_if(is.logical, as.numeric) 
  
  
  Sys.sleep(2)
  
  print("CBS - WR")
  
  wrLink <- read_html("https://www.cbssports.com/fantasy/football/stats/WR/2021/season/projections/nonppr/")
  
  wrPlayer <- wrLink %>%
    html_nodes(".CellPlayerName--long a") %>%
    html_text()
  
  wrRushYds <- wrLink %>%
    html_nodes(".TableBase-bodyTd--number:nth-child(10)") %>%
    html_text() %>%
    as.numeric()
  
  wrRushTds <- wrLink %>%
    html_nodes(".TableBase-bodyTd--number:nth-child(12)") %>%
    html_text() %>%
    as.numeric()
  
  wrRec <- wrLink %>%
    html_nodes(".TableBase-bodyTd--number:nth-child(4)") %>%
    html_text() %>%
    as.numeric()
  
  wrRecYds <- wrLink %>%
    html_nodes(".TableBase-bodyTd--number:nth-child(5)") %>%
    html_text() %>%
    as.numeric()
  
  wrRecTds <- wrLink %>%
    html_nodes(".TableBase-bodyTd--number:nth-child(8)") %>%
    html_text() %>%
    as.numeric()
  
  wrFumb <- wrLink %>%
    html_nodes(".TableBase-bodyTd--number:nth-child(13)") %>%
    html_text() %>%
    as.numeric()
  
  wrTeam <- wrLink %>%
    html_nodes(".CellPlayerName-team") %>%
    html_text() %>%
    str_remove("\n                        ") %>%
    str_remove("\n                    ") %>%
    str_remove_all(" ") %>%
    .[seq(from = 1, to = length(.)-1, by = 2)]
  
  wrTable <- data.frame("player" = wrPlayer,
                        "team" = wrTeam,
                        "position" = "WR",
                        "rush_yd" = wrRushYds,
                        "rush_td" = wrRushTds,
                        "rec" = wrRec,
                        "rec_yd" = wrRecYds,
                        "rec_td" = wrRecTds,
                        "fum" = wrFumb)
  
  num_cols <- colnames(wrTable)
  num_cols <- num_cols[-(1:3)]
  
  wrTable[num_cols] <- sapply(wrTable[num_cols],as.numeric)
  
  wrTable <- wrTable %>%
    mutate(player = str_remove(player, "'"),
           player = str_remove_all(player, "\\."),
           player = str_remove_all(player, " Jr"),
           player = str_remove_all(player, " IV"),
           player = str_remove_all(player, " III"),
           player = str_remove_all(player, " II"),
           player = tolower(player),
           position = "WR",
           source = "CBS",
           pass_td = NA, 
           pass_yd = NA, 
           int = NA,
           two_pt = NA,
           fg_50 = NA,
           fg_40_49 = NA,
           fg_39 = NA,
           missed_fg_40 = NA,
           missed_fg_39_less = NA,
           def_td = NA,
           def_int = NA,
           def_fum = NA,
           def_block = NA,
           def_safety = NA,
           def_sack = NA,
           def_0 = NA,
           def_1_6 = NA,
           def_7_13 = NA,
           def_14_20 = NA,
           def_21_27 = NA,
           def_28_34 = NA,
           def_35 = NA,
           def_avg = NA,
           pa = NA,
           pa_miss = NA,
           bye = NA) %>%
    select(player, team, position, source, pass_td, pass_yd, int, fum, rush_yd, rush_td, rec, rec_yd, rec_td,
           two_pt, fg_50, fg_40_49, fg_39, missed_fg_40, missed_fg_39_less, def_td, def_int, def_fum, def_block,
           def_safety, def_sack, def_0, def_1_6, def_7_13, def_14_20, def_21_27, def_28_34, def_35, pa, pa_miss, def_avg, bye) %>%
    mutate_if(is.logical, as.numeric) 
  
  Sys.sleep(2)
  
  print("CBS - TE")
  
  teLink <- read_html("https://www.cbssports.com/fantasy/football/stats/TE/2021/season/projections/nonppr/")
  
  tePlayer <- teLink %>%
    html_nodes(".CellPlayerName--long a") %>%
    html_text()
  
  teRec <- teLink %>%
    html_nodes(".TableBase-bodyTd--number:nth-child(4)") %>%
    html_text() %>%
    as.numeric()
  
  teRecYds <- teLink %>%
    html_nodes(".TableBase-bodyTd--number:nth-child(5)") %>%
    html_text() %>%
    as.numeric()
  
  teRecTds <- teLink %>%
    html_nodes(".TableBase-bodyTd--number:nth-child(8)") %>%
    html_text() %>%
    as.numeric()
  
  teFumb <- teLink %>%
    html_nodes(".TableBase-bodyTd--number:nth-child(9)") %>%
    html_text() %>%
    as.numeric()
  
  teTeam <- teLink %>%
    html_nodes(".CellPlayerName-team") %>%
    html_text() %>%
    str_remove("\n                        ") %>%
    str_remove("\n                    ") %>%
    str_remove_all(" ") %>%
    .[seq(from = 1, to = length(.)-1, by = 2)]
  
  teTable <- data.frame("player" = tePlayer,
                        "team" = teTeam,
                        "pos" = "TE",
                        "rec" = teRec,
                        "rec_yd" = teRecYds,
                        "rec_td" = teRecTds,
                        "fum" = teFumb)

  
  num_cols <- colnames(teTable)
  num_cols <- num_cols[-(1:3)]
  
  teTable[num_cols] <- sapply(teTable[num_cols],as.numeric)
  
  teTable <- teTable %>%
    mutate(player = str_remove(player, "'"),
           player = str_remove_all(player, "\\."),
           player = str_remove_all(player, " Jr"),
           player = str_remove_all(player, " IV"),
           player = str_remove_all(player, " III"),
           player = str_remove_all(player, " II"),
           player = tolower(player),
           position = "TE",
           source = "CBS",
           pass_td = NA, 
           pass_yd = NA, 
           int = NA,
           rush_yd = NA, 
           rush_td = NA,
           two_pt = NA,
           fg_50 = NA,
           fg_40_49 = NA,
           fg_39 = NA,
           missed_fg_40 = NA,
           missed_fg_39_less = NA,
           def_td = NA,
           def_int = NA,
           def_fum = NA,
           def_block = NA,
           def_safety = NA,
           def_sack = NA,
           def_0 = NA,
           def_1_6 = NA,
           def_7_13 = NA,
           def_14_20 = NA,
           def_21_27 = NA,
           def_28_34 = NA,
           def_35 = NA,
           def_avg = NA,
           pa = NA,
           pa_miss = NA,
           bye = NA) %>%
    select(player, team, position, source, pass_td, pass_yd, int, fum, rush_yd, rush_td, rec, rec_yd, rec_td,
           two_pt, fg_50, fg_40_49, fg_39, missed_fg_40, missed_fg_39_less, def_td, def_int, def_fum, def_block,
           def_safety, def_sack, def_0, def_1_6, def_7_13, def_14_20, def_21_27, def_28_34, def_35, pa, pa_miss, def_avg, bye) %>%
    mutate_if(is.logical, as.numeric) 
  
  Sys.sleep(2)
  
  print("CBS - K")
  
  klink <- read_html("https://www.cbssports.com/fantasy/football/stats/K/2021/season/projections/nonppr/")
  
  k_player <- klink %>%
    html_nodes(".CellPlayerName--long a") %>%
    html_text()
  
  k_1_19 <- klink %>%
    html_nodes(".TableBase-bodyTd--number:nth-child(6)") %>%
    html_text() %>%
    as.numeric()
  
  k_1_19_a <- klink %>%
    html_nodes(".TableBase-bodyTd--number:nth-child(7)") %>%
    html_text() %>%
    as.numeric()
  
  k_20_29 <- klink %>%
    html_nodes(".TableBase-bodyTd--number:nth-child(8)") %>%
    html_text() %>%
    as.numeric()
  
  k_20_29_a <- klink %>%
    html_nodes(".TableBase-bodyTd--number:nth-child(9)") %>%
    html_text() %>%
    as.numeric()
  
  k_30_39 <- klink %>%
    html_nodes(".TableBase-bodyTd--number:nth-child(10)") %>%
    html_text() %>%
    as.numeric()
  
  k_30_39_a <- klink %>%
    html_nodes(".TableBase-bodyTd--number:nth-child(11)") %>%
    html_text() %>%
    as.numeric()
  
  k_40_49 <- klink %>%
    html_nodes(".TableBase-bodyTd--number:nth-child(12)") %>%
    html_text() %>%
    as.numeric()
  
  k_40_49_a <- klink %>%
    html_nodes(".TableBase-bodyTd--number:nth-child(13)") %>%
    html_text() %>%
    as.numeric()
  
  k_50 <- klink %>%
    html_nodes(".TableBase-bodyTd--number:nth-child(14)") %>%
    html_text() %>%
    as.numeric()
  
  k_50_a <- klink %>%
    html_nodes(".TableBase-bodyTd--number:nth-child(15)") %>%
    html_text() %>%
    as.numeric()
  
  k_xpm <- klink %>%
    html_nodes(".TableBase-bodyTd--number:nth-child(16)") %>%
    html_text() %>%
    as.numeric()
  
  k_xpm_a <- klink %>%
    html_nodes(".TableBase-bodyTd--number:nth-child(17)") %>%
    html_text() %>%
    as.numeric()
  
  kTeam <- klink %>%
    html_nodes(".CellPlayerName-team") %>%
    html_text() %>%
    str_remove("\n                        ") %>%
    str_remove("\n                    ") %>%
    str_remove_all(" ") %>%
    .[c(1,3,5,7,9,11,13,15,17,19,21,23,25,27,29,31,33,35,37,39,41,43,45,47,49,51,53,55,57,59,61,63,65)]
  
  kTable <- data.frame("player" = k_player,
                       "position" = "K",
                       "team" = kTeam,
                       "k_1" = k_1_19,
                       "k_20" = k_20_29,
                       "k_30" = k_30_39,
                       "k_40" = k_40_49,
                       "k_50" = k_50,
                       "k_x" = k_xpm,
                       "k_1_a" = k_1_19_a,
                       "k_20_a" = k_20_29_a,
                       "k_30_a" = k_30_39_a,
                       "k_40_a" = k_40_49_a,
                       "k_50_a" = k_50_a,
                       "k_x_a" = k_xpm_a)
  
  
  num_cols <- colnames(kTable)
  num_cols <- num_cols[-(1:3)]
  
  kTable[num_cols] <- sapply(kTable[num_cols],as.numeric)
  
  kTable <- kTable %>%
    mutate(player = str_remove(player, "'"),
           player = str_remove_all(player, "\\."),
           player = str_remove_all(player, " Jr"),
           player = str_remove_all(player, " IV"),
           player = str_remove_all(player, " III"),
           player = str_remove_all(player, " II"),
           player = tolower(player),
           position = "K",
           source = "CBS",
           pass_td = NA, 
           pass_yd = NA, 
           int = NA,
           rush_yd = NA, 
           rush_td = NA,
           two_pt = NA,
           rec = NA, 
           fum = NA,
           rec_yd = NA, 
           rec_td = NA,
           fg_50 = k_50,
           fg_40_49 = k_40,
           fg_39 = k_1+k_20+k_30,
           missed_fg_40 = (k_50_a+k_40_a)-(k_50+k_40),
           missed_fg_39_less = (k_1_a+k_20_a+k_30_a)-(k_1+k_20+k_30),
           def_td = NA,
           def_int = NA,
           def_fum = NA,
           def_block = NA,
           def_safety = NA,
           def_sack = NA,
           def_0 = NA,
           def_1_6 = NA,
           def_7_13 = NA,
           def_14_20 = NA,
           def_21_27 = NA,
           def_28_34 = NA,
           def_35 = NA,
           def_avg = NA,
           pa = k_x,
           pa_miss = k_x_a-k_x,
           bye = NA) %>%
    select(player, team, position, source, pass_td, pass_yd, int, fum, rush_yd, rush_td, rec, rec_yd, rec_td,
           two_pt, fg_50, fg_40_49, fg_39, missed_fg_40, missed_fg_39_less, def_td, def_int, def_fum, def_block,
           def_safety, def_sack, def_0, def_1_6, def_7_13, def_14_20, def_21_27, def_28_34, def_35, pa, pa_miss, def_avg, bye) %>%
    mutate_if(is.logical, as.numeric) 
  
  Sys.sleep(2)
  
  print("CBS - DST")
  
  dstlink <- read_html("https://www.cbssports.com/fantasy/football/stats/DST/2020/season/projections/nonppr/")
  
  dst_player <- dstlink %>%
    html_nodes(".TeamName a") %>%
    html_text()
  
  dst_int <- dstlink %>%
    html_nodes(".TableBase-bodyTd--number:nth-child(2)") %>%
    html_text() %>%
    as.numeric()
  
  dst_s <- dstlink %>%
    html_nodes(".TableBase-bodyTd--number:nth-child(3)") %>%
    html_text() %>%
    as.numeric()
  
  dst_sack <- dstlink %>%
    html_nodes(".TableBase-bodyTd--number:nth-child(4)") %>%
    html_text() %>%
    as.numeric()
  
  dst_fum <- dstlink %>%
    html_nodes(".TableBase-bodyTd--number:nth-child(6)") %>%
    html_text() %>%
    as.numeric()
  
  dst_td <- dstlink %>%
    html_nodes(".TableBase-bodyTd--number:nth-child(8)") %>%
    html_text() %>%
    as.numeric()
  
  dst_pts <- dstlink %>%
    html_nodes(".TableBase-bodyTd--number:nth-child(9)") %>%
    html_text() %>%
    as.numeric()
  
  
  
  dst_table <- data.frame("player" = dst_player,
                          "position" = "DST",
                          "def_int" = dst_int,
                          "def_safety" = dst_s,
                          "def_sack" = dst_sack,
                          "def_fum" = dst_fum,
                          "def_td" = dst_td,
                          "pa_def" = dst_pts)

  
  
  num_cols <- colnames(dst_table)
  num_cols <- num_cols[-(1:2)]
  
  dst_table[num_cols] <- sapply(dst_table[num_cols],as.numeric)
  
  dst_table <- dst_table %>%
    mutate(player = str_remove(player, "'"),
           player = str_remove_all(player, "\\."),
           player = str_remove_all(player, " Jr"),
           player = str_remove_all(player, " IV"),
           player = str_remove_all(player, " III"),
           player = str_remove_all(player, " II"),
           player = tolower(player),
           position = "DST",
           source = "CBS",
           team = NA,
           team = as.character(team),
           pass_td = NA, 
           pass_yd = NA, 
           int = NA,
           fum = NA,
           rec = NA, 
           rec_yd = NA, 
           rec_td = NA,
           rush_yd = NA, 
           rush_td = NA,
           two_pt = NA,
           fg_50 = NA,
           fg_40_49 = NA,
           fg_39 = NA,
           missed_fg_40 = NA,
           missed_fg_39_less = NA,
           def_block = NA,
           def_0 = NA,
           def_1_6 = NA,
           def_7_13 = NA,
           def_14_20 = NA,
           def_21_27 = NA,
           def_28_34 = NA,
           def_35 = NA,
           def_avg = pa_def/17,
           pa = NA,
           pa_miss = NA,
           bye = NA) %>%
    select(player, team, position, source, pass_td, pass_yd, int, fum, rush_yd, rush_td, rec, rec_yd, rec_td,
           two_pt, fg_50, fg_40_49, fg_39, missed_fg_40, missed_fg_39_less, def_td, def_int, def_fum, def_block,
           def_safety, def_sack, def_0, def_1_6, def_7_13, def_14_20, def_21_27, def_28_34, def_35, pa, pa_miss, def_avg, bye) %>%
    mutate_if(is.logical, as.numeric) 
  
  qb_link <- read_html("https://www.cbssports.com/fantasy/football/stats/QB/2020/season/projections/nonppr/")
  
  qb_player <- qb_link %>%
    html_nodes(".CellPlayerName--long a") %>%
    html_text()
  
  qb_yds <- qb_link %>%
    html_nodes(".TableBase-bodyTd--number:nth-child(5)") %>%
    html_text() %>%
    as.numeric()
  
  qb_td <- qb_link %>%
    html_nodes(".TableBase-bodyTd--number:nth-child(7)") %>%
    html_text() %>%
    as.numeric()
  
  qb_int <- qb_link %>%
    html_nodes(".TableBase-bodyTd--number:nth-child(8)") %>%
    html_text() %>%
    as.numeric()
  
  qb_ryds <- qb_link %>%
    html_nodes(".TableBase-bodyTd--number:nth-child(11)") %>%
    html_text() %>%
    as.numeric()
  
  qb_rtd <- qb_link %>%
    html_nodes(".TableBase-bodyTd--number:nth-child(13)") %>%
    html_text() %>%
    as.numeric()
  
  qb_fum <- qb_link %>%
    html_nodes(".TableBase-bodyTd--number:nth-child(14)") %>%
    html_text() %>%
    as.numeric()
  
  
  qbTeam <- qb_link %>%
    html_nodes(".CellPlayerName-team") %>%
    html_text() %>%
    str_remove("\n                        ") %>%
    str_remove("\n                    ") %>%
    str_remove_all(" ") %>%
  .[seq(from = 1, to = length(.)-1, by = 2)]
  
  qb_table <- data.frame("player" = qb_player,
                         "position" = "QB",
                         "team" = qbTeam,
                         "pass_yd" = qb_yds,
                         "pass_td" = qb_td,
                         "int" = qb_int,
                         "rush_yd" = qb_ryds,
                         "rush_td" = qb_rtd,
                         "fum" = qb_fum)
  
  
  num_cols <- colnames(qb_table)
  num_cols <- num_cols[-(1:3)]
  
  qb_table[num_cols] <- sapply(qb_table[num_cols],as.numeric)
  
  qb_table <- qb_table %>%
    mutate(player = str_remove(player, "'"),
           player = str_remove_all(player, "\\."),
           player = str_remove_all(player, " Jr"),
           player = str_remove_all(player, " IV"),
           player = str_remove_all(player, " III"),
           player = str_remove_all(player, " II"),
           player = tolower(player),
           position = "QB",
           source = "CBS",
           rec = NA, 
           rec_yd = NA, 
           rec_td = NA,
           two_pt = NA,
           fg_50 = NA,
           fg_40_49 = NA,
           fg_39 = NA,
           missed_fg_40 = NA,
           missed_fg_39_less = NA,
           def_td = NA,
           def_int = NA,
           def_fum = NA,
           def_block = NA,
           def_safety = NA,
           def_sack = NA,
           def_0 = NA,
           def_1_6 = NA,
           def_7_13 = NA,
           def_14_20 = NA,
           def_21_27 = NA,
           def_28_34 = NA,
           def_35 = NA,
           def_avg = NA,
           pa = NA,
           pa_miss = NA,
           bye = NA) %>%
    select(player, team, position, source, pass_td, pass_yd, int, fum, rush_yd, rush_td, rec, rec_yd, rec_td,
           two_pt, fg_50, fg_40_49, fg_39, missed_fg_40, missed_fg_39_less, def_td, def_int, def_fum, def_block,
           def_safety, def_sack, def_0, def_1_6, def_7_13, def_14_20, def_21_27, def_28_34, def_35, pa, pa_miss, def_avg, bye) %>%
    mutate_if(is.logical, as.numeric) 
  
  cbs_scrape <- qb_table %>%
    rbind(rbTable) %>%
    rbind(wrTable) %>%
    rbind(teTable) %>%
    rbind(kTable) %>%
    rbind(dst_table)
  
  return(cbs_scrape)
}
cbs_scrape <- scrape_cbs()

#### razzball ####
scrape_razzball <- function() {
  print("razzball - QB")
  
  qb_link <- read_html("https://football.razzball.com/projections-qb-restofseason")
  
  qb_table <- qb_link %>%
    html_nodes(xpath = '//*[@id="neorazzstatstable"]') %>%
    html_table()
  
  qb_table <- qb_table[[1]]
  qb_table <- qb_table[,c(2,3,10,13,14,17,19,20)]
  colnames(qb_table) <- c("player", "team", "pass_yd", "pass_td", "int", "rush_yd", "rush_td", "fum")
  
  num_cols <- colnames(qb_table)
  num_cols <- num_cols[c(-1, -2)]
  
  qb_table[num_cols] <- sapply(qb_table[num_cols],as.numeric)
  
  qb_table <- qb_table %>%
    mutate(player = str_remove(player, "'"),
           player = str_remove_all(player, "\\."),
           player = str_remove_all(player, " Jr"),
           player = str_remove_all(player, " IV"),
           player = str_remove_all(player, " III"),
           player = str_remove_all(player, " II"),
           player = tolower(player),
           position = "QB",
           source = "Razzball",
           rec = NA,
           rec_yd = NA,
           rec_td = NA,
           two_pt = NA,
           fg_50 = NA,
           fg_40_49 = NA,
           fg_39 = NA,
           missed_fg_40 = NA,
           missed_fg_39_less = NA,
           def_td = NA,
           def_int = NA,
           def_fum = NA,
           def_block = NA,
           def_safety = NA,
           def_sack = NA,
           def_0 = NA,
           def_1_6 = NA,
           def_7_13 = NA,
           def_14_20 = NA,
           def_21_27 = NA,
           def_28_34 = NA,
           def_35 = NA,
           def_avg = NA,
           pa = NA,
           pa_miss = NA,
           bye = NA) %>%
    select(player, team, position, source, pass_td, pass_yd, int, fum, rush_yd, rush_td, rec, rec_yd, rec_td,
           two_pt, fg_50, fg_40_49, fg_39, missed_fg_40, missed_fg_39_less, def_td, def_int, def_fum, def_block,
           def_safety, def_sack, def_0, def_1_6, def_7_13, def_14_20, def_21_27, def_28_34, def_35, pa, pa_miss, def_avg, bye) %>%
    mutate_if(is.logical, as.numeric) 
  
  Sys.sleep(2)
  print("razzball - flex")
  
  flex_link <- read_html("https://football.razzball.com/projections-flex-restofseason")
  
  flex_table <- flex_link %>%
    html_nodes(xpath = '//*[@id="neorazzstatstable"]') %>%
    html_table()
  
  flex_table <- flex_table[[1]]
  flex_table <- flex_table[,c(2,3,4,10,12,15,16,19)]
  colnames(flex_table) <- c("player", "position", "team", "rush_yd", "rush_td", "rec", "rec_yd", "rec_td")
  
  num_cols <- colnames(flex_table)
  num_cols <- num_cols[c(-1, -2, -3)]
  
  flex_table[num_cols] <- sapply(flex_table[num_cols],as.numeric)
  
  flex_table <- flex_table %>%
    mutate(player = str_remove(player, "'"),
           player = str_remove_all(player, "\\."),
           player = str_remove_all(player, " Jr"),
           player = str_remove_all(player, " IV"),
           player = str_remove_all(player, " III"),
           player = str_remove_all(player, " II"),
           player = tolower(player),
           source = "Razzball",
           pass_td = NA, 
           pass_yd = NA, 
           int = NA,
           fum = NA,
           two_pt = NA,
           fg_50 = NA,
           fg_40_49 = NA,
           fg_39 = NA,
           missed_fg_40 = NA,
           missed_fg_39_less = NA,
           def_td = NA,
           def_int = NA,
           def_fum = NA,
           def_block = NA,
           def_safety = NA,
           def_sack = NA,
           def_0 = NA,
           def_1_6 = NA,
           def_7_13 = NA,
           def_14_20 = NA,
           def_21_27 = NA,
           def_28_34 = NA,
           def_35 = NA,
           def_avg = NA,
           pa = NA,
           pa_miss = NA,
           bye = NA) %>%
    select(player, team, position, source, pass_td, pass_yd, int, fum, rush_yd, rush_td, rec, rec_yd, rec_td,
           two_pt, fg_50, fg_40_49, fg_39, missed_fg_40, missed_fg_39_less, def_td, def_int, def_fum, def_block,
           def_safety, def_sack, def_0, def_1_6, def_7_13, def_14_20, def_21_27, def_28_34, def_35, pa, pa_miss, def_avg, bye) %>%
    mutate_if(is.logical, as.numeric) 
  
  Sys.sleep(2)
  print("razzball - DST")
  
  dst_link <- read_html("https://football.razzball.com/projections-teamdefense-restofseason")
  
  dst_table <- dst_link %>%
    html_nodes(xpath = '//*[@id="neorazzstatstable"]') %>%
    html_table()
  
  dst_table <- dst_table[[1]]
  dst_table <- dst_table[,c(2,6,7,9,10,11,18)]
  colnames(dst_table) <- c("team", "def_sack", "def_int", "def_fum", "def_safety", "def_td", "points_against")
  
  num_cols <- colnames(dst_table)
  num_cols <- num_cols[c(-1)]
  
  dst_table[num_cols] <- sapply(dst_table[num_cols],as.numeric)
  
  dst_table <- dst_table %>%
    mutate(player = team,
           player = str_remove_all(player, "'"),
           player = str_remove_all(player, " III"),
           player = str_remove_all(player, "\\."),
           player = tolower(player),
           player = str_remove_all(player, " jr"),
           player = case_when(
             player == "ari" ~ "arizona",
             player == "atl" ~ "atlanta",
             player == "bal" ~ "baltimore",
             player == "buf" ~ "buffalo",
             player == "car" ~ "carolina",
             player == "chi" ~ "chicago",
             player == "cin" ~ "cincinnati",
             player == "cle" ~ "cleveland",
             player == "dal" ~ "dallas",
             player == "den" ~ "denver",
             player == "det" ~ "detroit",
             player == "gb" ~ "green bay",
             player == "hou" ~ "houston",
             player == "ind" ~ "indianapolis",
             player == "jac" ~ "jacksonville",
             player == "kc" ~ "kansas city",
             player == "lv" ~ "las vegas",
             player == "lac" ~ "la chargers",
             player == "lar" ~ "la rams",
             player == "mia" ~ "miami",
             player == "min" ~ "minnesota",
             player == "ne" ~ "new england",
             player == "no" ~ "new orleans",
             player == "nyg" ~ "ny giants",
             player == "nyj" ~ "ny jets",
             player == "phi" ~ "philadelphia",
             player == "pit" ~ "pittsburgh",
             player == "sf" ~ "san francisco",
             player == "sea" ~ "seattle",
             player == "tb" ~ "tampa bay",
             player == "ten" ~ "tennessee",
             player == "was" ~ "washington",
             TRUE ~ player
           ),
           def_avg = points_against/17,
           position = "DST",
           source = "Razzball",
           rush_yd = NA, 
           rush_td = NA, 
           pass_td = NA, 
           pass_yd = NA,
           fum = NA,
           rec = NA, 
           rec_yd = NA, 
           rec_td = NA,
           int = NA,
           two_pt = NA,
           fg_50 = NA,
           fg_40_49 = NA,
           fg_39 = NA,
           missed_fg_40 = NA,
           missed_fg_39_less = NA,
           def_block = NA,
           def_0 = NA,
           def_1_6 = NA,
           def_7_13 = NA,
           def_14_20 = NA,
           def_21_27 = NA,
           def_28_34 = NA,
           def_35 = NA,
           pa = NA,
           pa_miss = NA,
           bye = NA) %>%
    select(player, team, position, source, pass_td, pass_yd, int, fum, rush_yd, rush_td, rec, rec_yd, rec_td,
           two_pt, fg_50, fg_40_49, fg_39, missed_fg_40, missed_fg_39_less, def_td, def_int, def_fum, def_block,
           def_safety, def_sack, def_0, def_1_6, def_7_13, def_14_20, def_21_27, def_28_34, def_35, pa, pa_miss, def_avg, bye) %>%
    mutate_if(is.logical, as.numeric) 
  
  Sys.sleep(2)
  print("razzball - K")
  
  k_link <- read_html("https://football.razzball.com/projections-pk-restofseason")
  
  k_table <- k_link %>%
    html_nodes(xpath = '//*[@id="neorazzstatstable"]') %>%
    html_table()
  
  k_table <- k_table[[1]]
  k_table <- k_table[,c(2,3,7,8,9,10,11,12,13)]
  colnames(k_table) <- c("player", "team", "pa", "pa_a", "fg_10", "fg_20", "fg_30", "fg_40", "fg_50")
  
  num_cols <- colnames(k_table)
  num_cols <- num_cols[c(-1, -2)]
  
  k_table[num_cols] <- sapply(k_table[num_cols],as.numeric)
  
  k_table <- k_table %>%
    mutate(player = str_remove(player, "'"),
           player = str_remove_all(player, "\\."),
           player = str_remove_all(player, " Jr"),
           player = str_remove_all(player, " IV"),
           player = str_remove_all(player, " III"),
           player = str_remove_all(player, " II"),
           player = tolower(player),
           source = "Razzball",
           position = "K",
           rush_yd = NA, 
           rush_td = NA, 
           rec = NA, 
           rec_yd = NA, 
           rec_td = NA,
           pass_td = NA, 
           pass_yd = NA, 
           int = NA,
           fum = NA,
           two_pt = NA,
           fg_40_49 = fg_40,
           fg_39 = fg_10+fg_20+fg_30,
           missed_fg_40 = NA,
           missed_fg_39_less = NA,
           def_td = NA,
           def_int = NA,
           def_fum = NA,
           def_block = NA,
           def_safety = NA,
           def_sack = NA,
           def_0 = NA,
           def_1_6 = NA,
           def_7_13 = NA,
           def_14_20 = NA,
           def_21_27 = NA,
           def_28_34 = NA,
           def_35 = NA,
           def_avg = NA,
           pa_miss = pa_a-pa,
           bye = NA) %>%
    select(player, team, position, source, pass_td, pass_yd, int, fum, rush_yd, rush_td, rec, rec_yd, rec_td,
           two_pt, fg_50, fg_40_49, fg_39, missed_fg_40, missed_fg_39_less, def_td, def_int, def_fum, def_block,
           def_safety, def_sack, def_0, def_1_6, def_7_13, def_14_20, def_21_27, def_28_34, def_35, pa, pa_miss, def_avg, bye) %>%
    mutate_if(is.logical, as.numeric) %>%
    filter(team != "DAL")
  
  razzball_scrape <- rbind(qb_table, flex_table, k_table, dst_table)
  
  return(razzball_scrape)
}
razzball_scrape <- scrape_razzball()

#### Fantasy Footballers ####
scrape_fantasy_footballers <- function(){
  print("Fantasy Footballers - QB")
  
  qb_mike <- read_csv("Fantasy Footballers/UDK - Mikes Projections - Fantasy Footballers Podcast - QB.csv") %>%
    mutate(source = "Fantasy Footballers Mike")
  qb_andy <- read_csv("Fantasy Footballers/UDK - Andys Projections - Fantasy Footballers Podcast - QB.csv") %>%
    mutate(source = "Fantasy Footballers Andy")
  qb_jason <- read_csv("Fantasy Footballers/UDK - Jasons Projections - Fantasy Footballers Podcast - QB.csv") %>%
    mutate(source = "Fantasy Footballers Jason")
  
  colnames(qb_mike) <- c("player", "team", "bye", "rank", "fpts", "pass_yd", "pass_td", "rush_yd", "rush_td", "int", "fum", "source")
  colnames(qb_andy) <- c("player", "team", "bye", "rank", "fpts", "pass_yd", "pass_td", "rush_yd", "rush_td", "int", "fum", "source")
  colnames(qb_jason) <- c("player", "team", "bye", "rank", "fpts", "pass_yd", "pass_td", "rush_yd", "rush_td", "int", "fum", "source")
  
  qb_table <- qb_mike %>%
    rbind(qb_andy) %>%
    rbind(qb_jason) %>%
    mutate(player = str_remove(player, "'"),
           player = str_remove_all(player, "\\."),
           player = str_remove_all(player, " Jr"),
           player = str_remove_all(player, " IV"),
           player = str_remove_all(player, " III"),
           player = str_remove_all(player, " II"),
           player = tolower(player),
           position = "QB",
           rec = NA,
           rec_yd = NA,
           rec_td = NA,
           two_pt = NA,
           fg_50 = NA,
           fg_40_49 = NA,
           fg_39 = NA,
           missed_fg_40 = NA,
           missed_fg_39_less = NA,
           def_td = NA,
           def_int = NA,
           def_fum = NA,
           def_block = NA,
           def_safety = NA,
           def_sack = NA,
           def_0 = NA,
           def_1_6 = NA,
           def_7_13 = NA,
           def_14_20 = NA,
           def_21_27 = NA,
           def_28_34 = NA,
           def_35 = NA,
           def_avg = NA,
           pa = NA,
           pa_miss = NA) %>%
    select(player, team, position, source, pass_td, pass_yd, int, fum, rush_yd, rush_td, rec, rec_yd, rec_td,
           two_pt, fg_50, fg_40_49, fg_39, missed_fg_40, missed_fg_39_less, def_td, def_int, def_fum, def_block,
           def_safety, def_sack, def_0, def_1_6, def_7_13, def_14_20, def_21_27, def_28_34, def_35, pa, pa_miss, def_avg, bye) %>%
    mutate_if(is.logical, as.numeric) 
  
  print("Fantasy Footballers - RB")
  
  rb_mike <- read_csv("Fantasy Footballers/UDK - Mikes Projections - Fantasy Footballers Podcast - RB.csv") %>%
    mutate(source = "Fantasy Footballers Mike")
  rb_andy <- read_csv("Fantasy Footballers/UDK - Andys Projections - Fantasy Footballers Podcast - RB.csv") %>%
    mutate(source = "Fantasy Footballers Andy")
  rb_jason <- read_csv("Fantasy Footballers/UDK - Jasons Projections - Fantasy Footballers Podcast - RB.csv") %>%
    mutate(source = "Fantasy Footballers Jason")
  
  colnames(rb_mike) <- c("player", "team", "bye", "rank", "fpts", "rush_att", "rush_yd", "rush_td", "rec", "rec_yd", "rec_td", "fum", "source")
  colnames(rb_andy) <- c("player", "team", "bye", "rank", "fpts", "rush_att", "rush_yd", "rush_td", "rec", "rec_yd", "rec_td", "fum", "source")
  colnames(rb_jason) <- c("player", "team", "bye", "rank", "fpts", "rush_att", "rush_yd", "rush_td", "rec", "rec_yd", "rec_td", "fum", "source")
  
  rb_table <- rb_mike %>%
    rbind(rb_andy) %>%
    rbind(rb_jason) %>%
    mutate(player = str_remove(player, "'"),
           player = str_remove_all(player, "\\."),
           player = str_remove_all(player, " Jr"),
           player = str_remove_all(player, " IV"),
           player = str_remove_all(player, " III"),
           player = str_remove_all(player, " II"),
           player = tolower(player),
           position = "RB",
           pass_td = NA, 
           pass_yd = NA, 
           int = NA,
           two_pt = NA,
           fg_50 = NA,
           fg_40_49 = NA,
           fg_39 = NA,
           missed_fg_40 = NA,
           missed_fg_39_less = NA,
           def_td = NA,
           def_int = NA,
           def_fum = NA,
           def_block = NA,
           def_safety = NA,
           def_sack = NA,
           def_0 = NA,
           def_1_6 = NA,
           def_7_13 = NA,
           def_14_20 = NA,
           def_21_27 = NA,
           def_28_34 = NA,
           def_35 = NA,
           def_avg = NA,
           pa = NA,
           pa_miss = NA) %>%
    select(player, team, position, source, pass_td, pass_yd, int, fum, rush_yd, rush_td, rec, rec_yd, rec_td,
           two_pt, fg_50, fg_40_49, fg_39, missed_fg_40, missed_fg_39_less, def_td, def_int, def_fum, def_block,
           def_safety, def_sack, def_0, def_1_6, def_7_13, def_14_20, def_21_27, def_28_34, def_35, pa, pa_miss, def_avg, bye) %>%
    mutate_if(is.logical, as.numeric) 
  
  print("Fantasy Footballers - WR")
  
  wr_mike <- read_csv("Fantasy Footballers/UDK - Mikes Projections - Fantasy Footballers Podcast - WR.csv") %>%
    mutate(source = "Fantasy Footballers Mike")
  wr_andy <- read_csv("Fantasy Footballers/UDK - Andys Projections - Fantasy Footballers Podcast - WR.csv") %>%
    mutate(source = "Fantasy Footballers Andy")
  wr_jason <- read_csv("Fantasy Footballers/UDK - Jasons Projections - Fantasy Footballers Podcast - WR.csv") %>%
    mutate(source = "Fantasy Footballers Jason")
  
  colnames(wr_mike) <- c("player", "team", "bye", "rank", "fpts", "rec", "rec_yd", "rec_td", "rush_att", "rush_yd", "rush_td", "fum", "source")
  colnames(wr_andy) <- c("player", "team", "bye", "rank", "fpts", "rec", "rec_yd", "rec_td", "rush_att", "rush_yd", "rush_td", "fum", "source")
  colnames(wr_jason) <- c("player", "team", "bye", "rank", "fpts", "rec", "rec_yd", "rec_td", "rush_att", "rush_yd", "rush_td", "fum", "source")
  
  wr_table <- wr_mike %>%
    rbind(wr_andy) %>%
    rbind(wr_jason) %>%
    mutate(player = str_remove(player, "'"),
           player = str_remove_all(player, "\\."),
           player = str_remove_all(player, " Jr"),
           player = str_remove_all(player, " IV"),
           player = str_remove_all(player, " III"),
           player = str_remove_all(player, " II"),
           player = tolower(player),
           position = "WR",
           pass_td = NA, 
           pass_yd = NA, 
           int = NA,
           two_pt = NA,
           fg_50 = NA,
           fg_40_49 = NA,
           fg_39 = NA,
           missed_fg_40 = NA,
           missed_fg_39_less = NA,
           def_td = NA,
           def_int = NA,
           def_fum = NA,
           def_block = NA,
           def_safety = NA,
           def_sack = NA,
           def_0 = NA,
           def_1_6 = NA,
           def_7_13 = NA,
           def_14_20 = NA,
           def_21_27 = NA,
           def_28_34 = NA,
           def_35 = NA,
           def_avg = NA,
           pa = NA,
           pa_miss = NA) %>%
    select(player, team, position, source, pass_td, pass_yd, int, fum, rush_yd, rush_td, rec, rec_yd, rec_td,
           two_pt, fg_50, fg_40_49, fg_39, missed_fg_40, missed_fg_39_less, def_td, def_int, def_fum, def_block,
           def_safety, def_sack, def_0, def_1_6, def_7_13, def_14_20, def_21_27, def_28_34, def_35, pa, pa_miss, def_avg, bye) %>%
    mutate_if(is.logical, as.numeric) 
  
  
  print("Fantasy Footballers - TE")
  
  te_mike <- read_csv("Fantasy Footballers/UDK - Mikes Projections - Fantasy Footballers Podcast - TE.csv") %>%
    mutate(source = "Fantasy Footballers Mike")
  te_andy <- read_csv("Fantasy Footballers/UDK - Andys Projections - Fantasy Footballers Podcast - TE.csv") %>%
    mutate(source = "Fantasy Footballers Andy")
  te_jason <- read_csv("Fantasy Footballers/UDK - Jasons Projections - Fantasy Footballers Podcast - TE.csv") %>%
    mutate(source = "Fantasy Footballers Jason")
  
  colnames(te_mike) <- c("player", "team", "bye", "rank", "fpts", "rec", "rec_yd", "rec_td", "fum", "source")
  colnames(te_andy) <- c("player", "team", "bye", "rank", "fpts", "rec", "rec_yd", "rec_td", "fum", "source")
  colnames(te_jason) <- c("player", "team", "bye", "rank", "fpts", "rec", "rec_yd", "rec_td", "fum", "source")
  
  te_table <- te_mike %>%
    rbind(te_andy) %>%
    rbind(te_jason) %>%
    mutate(player = str_remove(player, "'"),
           player = str_remove_all(player, "\\."),
           player = str_remove_all(player, " Jr"),
           player = str_remove_all(player, " IV"),
           player = str_remove_all(player, " III"),
           player = str_remove_all(player, " II"),
           player = tolower(player),
           position = "TE",
           rush_yd = NA, 
           rush_td = NA,
           pass_td = NA, 
           pass_yd = NA, 
           int = NA,
           two_pt = NA,
           fg_50 = NA,
           fg_40_49 = NA,
           fg_39 = NA,
           missed_fg_40 = NA,
           missed_fg_39_less = NA,
           def_td = NA,
           def_int = NA,
           def_fum = NA,
           def_block = NA,
           def_safety = NA,
           def_sack = NA,
           def_0 = NA,
           def_1_6 = NA,
           def_7_13 = NA,
           def_14_20 = NA,
           def_21_27 = NA,
           def_28_34 = NA,
           def_35 = NA,
           def_avg = NA,
           pa = NA,
           pa_miss = NA) %>%
    select(player, team, position, source, pass_td, pass_yd, int, fum, rush_yd, rush_td, rec, rec_yd, rec_td,
           two_pt, fg_50, fg_40_49, fg_39, missed_fg_40, missed_fg_39_less, def_td, def_int, def_fum, def_block,
           def_safety, def_sack, def_0, def_1_6, def_7_13, def_14_20, def_21_27, def_28_34, def_35, pa, pa_miss, def_avg, bye) %>%
    mutate_if(is.logical, as.numeric) 
  
  fantasy_footballers_scrape <- rbind(qb_table, rb_table, wr_table, te_table)
  
  return(fantasy_footballers_scrape)
  
}
fantasy_footballers_scrape <- scrape_fantasy_footballers()

#### 4for4 ####
scrape_4for4 <- function(){
  print("4for4")
  
  qb_scrape <- read_csv("4for4/4for4_projections_player.csv")
  qb_scrape <- qb_scrape[,c(2,3,4,9,10,11,13,14,15,16,17,21)]
  
  colnames(qb_scrape) <- c("player", "position", "team", "pass_yd", "pass_td", "int", "rush_yd", "rush_td", "rec", "rec_yd", "rec_td", "fum")
  
  player_table <- qb_scrape %>%
    mutate(player = str_remove(player, "'"),
           player = str_remove_all(player, "\\."),
           player = str_remove_all(player, " Jr"),
           player = str_remove_all(player, " IV"),
           player = str_remove_all(player, " III"),
           player = str_remove_all(player, " II"),
           player = tolower(player),
           source = "4for4",
           two_pt = NA,
           fg_50 = NA,
           fg_40_49 = NA,
           fg_39 = NA,
           missed_fg_40 = NA,
           missed_fg_39_less = NA,
           def_td = NA,
           def_int = NA,
           def_fum = NA,
           def_block = NA,
           def_safety = NA,
           def_sack = NA,
           def_0 = NA,
           def_1_6 = NA,
           def_7_13 = NA,
           def_14_20 = NA,
           def_21_27 = NA,
           def_28_34 = NA,
           def_35 = NA,
           def_avg = NA,
           pa = NA,
           pa_miss = NA,
           bye = NA) %>%
    select(player, team, position, source, pass_td, pass_yd, int, fum, rush_yd, rush_td, rec, rec_yd, rec_td,
           two_pt, fg_50, fg_40_49, fg_39, missed_fg_40, missed_fg_39_less, def_td, def_int, def_fum, def_block,
           def_safety, def_sack, def_0, def_1_6, def_7_13, def_14_20, def_21_27, def_28_34, def_35, pa, pa_miss, def_avg, bye) %>%
    mutate_if(is.logical, as.numeric) 
  
  for4_scrape <- player_table %>%
    filter(!position == "K")
  
  return(for4_scrape)
}
for4_scrape <- scrape_4for4()

#### Number Fire ####
scrape_number_fire <- function(){
  
  
  rD <- rsDriver(browser="firefox", port=4545L, verbose=F)
  remDr <- rD[["client"]]
  
  remDr$open()
  remDr$navigate("https://www.numberfire.com/")
  remDr$screenshot(TRUE)
  
  webElem15 <- remDr$findElement(using = 'xpath', '/html/body/nav[1]/div/ul[2]/li[3]/a')
  webElem15$clickElement()
  webElem15$sendKeysToElement(list(key = "enter"))
  
  Sys.sleep(2)
  
  webElem16 <- remDr$findElement(using = 'xpath', '/html/body/div[5]/div/span/a')
  webElem16$clickElement()
  
  Sys.sleep(2)
  
  webElem16.5 <- remDr$findElement(using = "xpath", "/html/body/div[4]/div/ul/li[4]/a")
  webElem16.5$clickElement()
  
  Sys.sleep(5)
  
  webElem17 <- remDr$findElement(using = 'xpath', '//*[@id="identifierId"]')
  webElem17$sendKeysToElement(list("ENTER YOUR EMAIL"))
  
  Sys.sleep(3)
  
  webElem18 <- remDr$findElement(using = 'xpath', '//*[@id="identifierNext"]')
  webElem18$clickElement()
  
  Sys.sleep(3)
  
  webElem19 <- remDr$findElement(using = 'xpath', '/html/body/div[1]/div[1]/div[2]/div/div[2]/div/div/div[2]/div/div[1]/div/form/span/section/div/div/div[1]/div[1]/div/div/div/div/div[1]/div/div[1]/input')
  webElem19$sendKeysToElement(list("ENTER YOUR PASSWORD"))
  
  Sys.sleep(3)
  
  webElem20 <- remDr$findElement(using = 'xpath', '//*[@id="passwordNext"]')
  webElem20$clickElement()
  
  Sys.sleep(3)
  
  remDr$navigate("https://www.numberfire.com/nfl/fantasy/remaining-projections")
  
  nfHTML <- remDr$getPageSource("outerHTML")[[1]] %>%
    read_html()
  
  nfPlayer <- nfHTML %>%
    html_nodes(".full") %>%
    html_text()
  
  nfPosition <- nfHTML %>%
    html_nodes(".player") %>%
    html_text() %>%
    str_remove("\n                        \n                            ") %>%
    str_remove("\n                            ") %>%
    str_remove("\n                        ") %>%
    str_remove("\n                    ") %>%
    substr(nchar(.)-8, nchar(.)) %>%
    str_remove_all(" ") %>%
    str_remove(",") %>%
    substr(2,3)
  
  nfPlayerTeam <- nfHTML %>%
    html_nodes(".player") %>%
    html_text() %>%
    str_remove("\n                        \n                            ") %>%
    str_remove("\n                            ") %>%
    str_remove("\n                        ") %>%
    str_remove("\n                    ") %>%
    substr(nchar(.)-8, nchar(.)) %>%
    str_remove_all(" ") %>%
    str_remove(",") %>%
    substr(4, nchar(.)-1)
  
  nfTable <- nfHTML %>%
    html_nodes(xpath = '/html/body/main/section[1]/div/div[3]/div[1]/table') %>%
    html_table() %>%
    .[[1]] %>%
    header.true()
  
  nfTable$player <- nfPlayer
  nfTable$position <- nfPosition
  nfTable$team <- nfPlayerTeam
  
  nfTable <- nfTable[,c(16,17,18,6,7,8,10,11,12,14,15)]
  colnames(nfTable) <- c("player", "position", "team", "pass_yd", "pass_td", "int", "rush_yd", "rush_td", "rec", "rec_yd", "rec_td")
  
  num_cols <- colnames(nfTable)
  num_cols <- num_cols[c(-1, -2, -3)]
  
  nfTable[num_cols] <- sapply(nfTable[num_cols],as.numeric)
  
  
  nfTable_player <- nfTable %>%
    mutate(player = str_remove(player, "'"),
           player = str_remove_all(player, "\\."),
           player = str_remove_all(player, " Jr"),
           player = str_remove_all(player, " IV"),
           player = str_remove_all(player, " III"),
           player = str_remove_all(player, " II"),
           player = tolower(player),
           source = "Number Fire",
           fum = NA,
           bye = NA,
           two_pt = NA,
           fg_50 = NA,
           fg_40_49 = NA,
           fg_39 = NA,
           missed_fg_40 = NA,
           missed_fg_39_less = NA,
           def_td = NA,
           def_int = NA,
           def_fum = NA,
           def_block = NA,
           def_safety = NA,
           def_sack = NA,
           def_0 = NA,
           def_1_6 = NA,
           def_7_13 = NA,
           def_14_20 = NA,
           def_21_27 = NA,
           def_28_34 = NA,
           def_35 = NA,
           def_avg = NA,
           pa = NA,
           pa_miss = NA) %>%
    select(player, team, position, source, pass_td, pass_yd, int, fum, rush_yd, rush_td, rec, rec_yd, rec_td,
           two_pt, fg_50, fg_40_49, fg_39, missed_fg_40, missed_fg_39_less, def_td, def_int, def_fum, def_block,
           def_safety, def_sack, def_0, def_1_6, def_7_13, def_14_20, def_21_27, def_28_34, def_35, pa, pa_miss, def_avg, bye) %>%
    mutate_if(is.logical, as.numeric)
  
  
  Sys.sleep(2)
  
  print("Number Fire - Defense")
  
  remDr$navigate("https://www.numberfire.com/nfl/fantasy/remaining-projections/d")
  
  nfHTML <- remDr$getPageSource("outerHTML")[[1]] %>%
    read_html()
  
  nfPlayer <- nfHTML %>%
    html_nodes(".full") %>%
    html_text()
  
  nfPlayerTeam <- nfHTML %>%
    html_nodes(".player") %>%
    html_text() %>%
    str_remove("\n                        \n                            ") %>%
    str_remove("\n                            ") %>%
    str_remove("\n                        ") %>%
    str_remove("\n                    ") %>%
    substr(nchar(.)-8, nchar(.)) %>%
    substr(nchar(.)-3, nchar(.)-1) %>%
    str_remove_all(" ")
  
  nfTable <- nfHTML %>%
    html_nodes(xpath = '/html/body/main/section[1]/div/div[3]/div[1]/table') %>%
    html_table() %>%
    .[[1]] %>%
    header.true()
  
  nfTable$player <- nfPlayer
  nfTable$team <- nfPlayerTeam
  
  nfTable <- nfTable[,c(10,11,4,6,7,8,9)]
  colnames(nfTable) <- c("player", "team", "pa", "def_sack", "def_int", "def_fum", "def_td")
  
  num_cols <- colnames(nfTable)
  num_cols <- num_cols[c(-1, -2)]
  
  nfTable[num_cols] <- sapply(nfTable[num_cols],as.numeric)
  
  
  nfTable_defense <- nfTable %>%
    mutate(position = "DST",
           player = str_remove_all(player, "'"),
           player = str_remove_all(player, " III"),
           player = str_remove_all(player, "\\."),
           player = tolower(player),
           player = str_remove_all(player, " jr"),
           player = str_remove_all(player, " d/st"),
           player = str_replace(player, "los angeles", "la"),
           player = str_replace_all(player, "new york", "ny"),
           source = "Number Fire",
           def_avg = pa/17,
           bye = NA,
           rush_yd = NA, 
           rush_td = NA, 
           pass_td = NA, 
           pass_yd = NA,
           int = NA,
           fum = NA,
           rec = NA, 
           rec_yd = NA, 
           rec_td = NA,
           two_pt = NA,
           fg_50 = NA,
           fg_40_49 = NA,
           fg_39 = NA,
           missed_fg_40 = NA,
           missed_fg_39_less = NA,
           def_block = NA,
           def_safety = NA,
           def_0 = NA,
           def_1_6 = NA,
           def_7_13 = NA,
           def_14_20 = NA,
           def_21_27 = NA,
           def_28_34 = NA,
           def_35 = NA,
           pa = NA,
           pa_miss = NA) %>%
    select(player, team, position, source, pass_td, pass_yd, int, fum, rush_yd, rush_td, rec, rec_yd, rec_td,
           two_pt, fg_50, fg_40_49, fg_39, missed_fg_40, missed_fg_39_less, def_td, def_int, def_fum, def_block,
           def_safety, def_sack, def_0, def_1_6, def_7_13, def_14_20, def_21_27, def_28_34, def_35, pa, pa_miss, def_avg, bye) %>%
    mutate_if(is.logical, as.numeric)
  
  
  Sys.sleep(3)
  
  print("Number Fire - Kicker")
  
  remDr$navigate("https://www.numberfire.com/nfl/fantasy/remaining-projections/k")
  
  nfHTML <- remDr$getPageSource("outerHTML")[[1]] %>%
    read_html()
  
  nfPlayer <- nfHTML %>%
    html_nodes(".full") %>%
    html_text()
  
  nfPlayerTeam <- nfHTML %>%
    html_nodes(".player") %>%
    html_text() %>%
    str_remove("\n                        \n                            ") %>%
    str_remove("\n                            ") %>%
    str_remove("\n                        ") %>%
    str_remove("\n                    ") %>%
    substr(nchar(.)-8, nchar(.)) %>%
    substr(nchar(.)-3, nchar(.)-1) %>%
    str_remove_all(" ")
  
  nfTable <- nfHTML %>%
    html_nodes(xpath = '/html/body/main/section[1]/div/div[3]/div[1]/table') %>%
    html_table() %>%
    .[[1]] %>%
    header.true()
  
  nfTable$player <- nfPlayer
  nfTable$team <- nfPlayerTeam
  
  nfTable <- nfTable[,c(12,13,4,7,8,9,10,11)]
  colnames(nfTable) <- c("player", "team", "pa", "fg_0", "fg_20", "fg_30", "fg_40", "fg_50")
  
  num_cols <- colnames(nfTable)
  num_cols <- num_cols[c(-1, -2)]
  
  nfTable[num_cols] <- sapply(nfTable[num_cols],as.numeric)
  
  
  nfTable_kicker <- nfTable %>%
    mutate(player = str_remove(player, "'"),
           player = str_remove_all(player, "\\."),
           player = str_remove_all(player, " Jr"),
           player = str_remove_all(player, " IV"),
           player = str_remove_all(player, " III"),
           player = str_remove_all(player, " II"),
           player = tolower(player),
           source = "Number Fire",
           position = "K",
           pass_td = NA, 
           pass_yd = NA,
           rush_yd = NA, 
           rush_td = NA, 
           rec = NA, 
           rec_yd = NA, 
           rec_td = NA,
           fum = NA,
           bye = NA,
           int = NA,
           two_pt = NA,
           fg_40_49 =fg_40,
           fg_39 = fg_0+fg_20+fg_30,
           missed_fg_40 = NA,
           missed_fg_39_less = NA,
           def_td = NA,
           def_int = NA,
           def_fum = NA,
           def_block = NA,
           def_safety = NA,
           def_sack = NA,
           def_0 = NA,
           def_1_6 = NA,
           def_7_13 = NA,
           def_14_20 = NA,
           def_21_27 = NA,
           def_28_34 = NA,
           def_35 = NA,
           def_avg = NA,
           pa_miss = NA) %>%
    select(player, team, position, source, pass_td, pass_yd, int, fum, rush_yd, rush_td, rec, rec_yd, rec_td,
           two_pt, fg_50, fg_40_49, fg_39, missed_fg_40, missed_fg_39_less, def_td, def_int, def_fum, def_block,
           def_safety, def_sack, def_0, def_1_6, def_7_13, def_14_20, def_21_27, def_28_34, def_35, pa, pa_miss, def_avg, bye) %>%
    mutate_if(is.logical, as.numeric)
  
  
  number_fire_scrape <- rbind(nfTable_defense, nfTable_player, nfTable_kicker)
  
  
  Sys.sleep(2)
  
  
  
  return(number_fire_scrape)
}
number_fire_scrape <- scrape_number_fire()

#### ESPN (doesn't have DST) ####
scrape_espn <- function(){
  
  rD <- rsDriver(browser="firefox", port=4545L, verbose=F)
  remDr <- rD[["client"]]
  
  remDr$open()
  remDr$navigate("https://fantasy.espn.com/football/players/projections")
  remDr$screenshot(TRUE)
  
  Sys.sleep(5)
  
  webElem1 <- remDr$findElement(using = "xpath", "/html/body/div[1]/div[1]/div/div/div[5]/div[2]/div[2]/div[1]/div/div[2]/div[2]/div/button[1]")
  webElem1$clickElement()
  
  print("ESPN - QB")
  
  Sys.sleep(1)
  
  webElem2 <- remDr$findElement(using = "xpath", "/html/body/div[1]/div[1]/div/div/div[5]/div[2]/div[2]/div[1]/div/div[2]/div[3]/div/div/label[2]")
  webElem2$clickElement()
  
  Sys.sleep(.1)
  
  qb_link <- remDr$getPageSource("outerHTML")[[1]] %>%
    read_html()
  
  qb_player <- qb_link %>%
    html_nodes(".truncate:nth-child(1) .pointer") %>%
    html_text()
  
  qb_team <- qb_link %>%
    html_nodes(".playerinfo__playerteam") %>%
    html_text()
  
  qb_table <- qb_link %>%
    html_nodes(".Table__Scroller > table:nth-child(1)") %>%
    html_table()
  
  qb_table <- qb_table[[1]] %>%
    header.true()
  
  qb_table <- qb_table[,c(-1)]
  
  qb_table$player <- qb_player
  
  qb_table$team <- qb_team
  
  webElem3 <- remDr$findElement(using = "xpath", "/html/body/div[1]/div[1]/div/div/div[5]/div[2]/div[3]/div/div/div/div/nav/button[2]")
  
  for(i in 1:2) {
    
    webElem3$clickElement()
    
    Sys.sleep(1)
    
    qb_link_temp <- remDr$getPageSource("outerHTML")[[1]] %>%
      read_html()
    
    qb_player_temp <- qb_link_temp %>%
      html_nodes(".truncate:nth-child(1) .pointer") %>%
      html_text()
    
    
    qb_team_temp <- qb_link_temp %>%
      html_nodes(".playerinfo__playerteam") %>%
      html_text()
    
    qb_table_temp <- qb_link_temp %>%
      html_nodes(".Table__Scroller > table:nth-child(1)") %>%
      html_table()
    
    qb_table_temp <- qb_table_temp[[1]] %>%
      header.true()
    
    qb_table_temp <- qb_table_temp[,c(-1)]
    
    qb_table_temp$player <- qb_player_temp
    
    qb_table_temp$team <- qb_team_temp
    
    qb_table <- rbind(qb_table, qb_table_temp)
  }
  
  qb_table <- qb_table[,c(13,14,1,2,3,5,6)]
  colnames(qb_table) <- c("player", "team", "pass_yd", "pass_td", "int", "rush_yd", "rush_td")
  
  num_cols <- colnames(qb_table)
  num_cols <- num_cols[-c(1,2)]
  
  qb_table[num_cols] <- sapply(qb_table[num_cols],as.numeric)
  
  qb_table[num_cols] <- sapply(qb_table[num_cols],replace_na, 0)
  
  qb_table <- qb_table %>%
    mutate(player = str_remove(player, "'"),
           player = str_remove_all(player, "\\."),
           player = str_remove_all(player, " Jr"),
           player = str_remove_all(player, " IV"),
           player = str_remove_all(player, " III"),
           player = str_remove_all(player, " II"),
           player = tolower(player),
           team = toupper(team),
           position = "QB",
           source = "ESPN",
           rec = NA,
           rec_yd = NA,
           fum = NA,
           rec_td = NA,
           two_pt = NA,
           fg_50 = NA,
           fg_40_49 = NA,
           fg_39 = NA,
           missed_fg_40 = NA,
           missed_fg_39_less = NA,
           def_td = NA,
           def_int = NA,
           def_fum = NA,
           def_block = NA,
           def_safety = NA,
           def_sack = NA,
           def_0 = NA,
           def_1_6 = NA,
           def_7_13 = NA,
           def_14_20 = NA,
           def_21_27 = NA,
           def_28_34 = NA,
           def_35 = NA,
           def_avg = NA,
           pa = NA,
           pa_miss = NA,
           bye = NA) %>%
    select(player, team, position, source, pass_td, pass_yd, int, fum, rush_yd, rush_td, rec, rec_yd, rec_td,
           two_pt, fg_50, fg_40_49, fg_39, missed_fg_40, missed_fg_39_less, def_td, def_int, def_fum, def_block,
           def_safety, def_sack, def_0, def_1_6, def_7_13, def_14_20, def_21_27, def_28_34, def_35, pa, pa_miss, def_avg, bye) %>%
    mutate_if(is.logical, as.numeric) 
  
  print("ESPN - FLEX")
  
  Sys.sleep(1)
  
  webElem2 <- remDr$findElement(using = "xpath", "/html/body/div[1]/div[1]/div/div/div[5]/div[2]/div[2]/div[1]/div/div[2]/div[3]/div/div/label[6]")
  webElem2$clickElement()
  
  Sys.sleep(1)
  
  flex_link <- remDr$getPageSource("outerHTML")[[1]] %>%
    read_html()
  
  flex_player <- flex_link %>%
    html_nodes(".truncate:nth-child(1) .pointer") %>%
    html_text()
  
  flex_table <- flex_link %>%
    html_nodes(".Table__Scroller > table:nth-child(1)") %>%
    html_table()
  
  flex_team <- flex_link %>%
    html_nodes(".playerinfo__playerteam") %>%
    html_text()
  
  flex_position <- flex_link %>%
    html_nodes(".ttu") %>%
    html_text()
  
  flex_table <- flex_table[[1]] %>%
    header.true()
  
  flex_table <- flex_table[,c(-1)]
  
  flex_table$player <- flex_player
  flex_table$position <- flex_position
  flex_table$team <- flex_team
  
  for(i in 1:17) {
    
    webElem3$clickElement()
    
    Sys.sleep(1)
    
    flex_link_temp <- remDr$getPageSource("outerHTML")[[1]] %>%
      read_html()
    
    flex_player_temp <- flex_link_temp %>%
      html_nodes(".truncate:nth-child(1) .pointer") %>%
      html_text()
    
    flex_team_temp <- flex_link_temp %>%
      html_nodes(".playerinfo__playerteam") %>%
      html_text()
    
    flex_position_temp <- flex_link_temp %>%
      html_nodes(".ttu") %>%
      html_text()
    
    flex_table_temp <- flex_link_temp %>%
      html_nodes(".Table__Scroller > table:nth-child(1)") %>%
      html_table()
    
    flex_table_temp <- flex_table_temp[[1]] %>%
      header.true()
    
    flex_table_temp <- flex_table_temp[,c(-1)]
    
    flex_table_temp$player <- flex_player_temp
    flex_table_temp$position <- flex_position_temp
    flex_table_temp$team <- flex_team_temp
    
    flex_table <- rbind(flex_table, flex_table_temp)
  }
  
  flex_table <- flex_table[,c(13,14,15,5,6,7,8,9)]
  colnames(flex_table) <- c("player", "position", "team", "rush_yd", "rush_td", "rec", "rec_yd", "rec_td")
  
  num_cols <- colnames(flex_table)
  num_cols <- num_cols[-c(1,2,3)]
  
  flex_table[num_cols] <- sapply(flex_table[num_cols],as.numeric)
  
  flex_table[num_cols] <- sapply(flex_table[num_cols],replace_na, 0)
  
  flex_table <- flex_table %>%
    mutate(player = str_remove(player, "'"),
           player = str_remove_all(player, "\\."),
           player = str_remove_all(player, " Jr"),
           player = str_remove_all(player, " IV"),
           player = str_remove_all(player, " III"),
           player = str_remove_all(player, " II"),
           player = tolower(player),
           team = toupper(team),
           source = "ESPN",
           pass_td = NA, 
           pass_yd = NA, 
           int = NA,
           fum = NA,
           two_pt = NA,
           fg_50 = NA,
           fg_40_49 = NA,
           fg_39 = NA,
           missed_fg_40 = NA,
           missed_fg_39_less = NA,
           def_td = NA,
           def_int = NA,
           def_fum = NA,
           def_block = NA,
           def_safety = NA,
           def_sack = NA,
           def_0 = NA,
           def_1_6 = NA,
           def_7_13 = NA,
           def_14_20 = NA,
           def_21_27 = NA,
           def_28_34 = NA,
           def_35 = NA,
           def_avg = NA,
           pa = NA,
           pa_miss = NA,
           bye = NA) %>%
    select(player, team, position, source, pass_td, pass_yd, int, fum, rush_yd, rush_td, rec, rec_yd, rec_td,
           two_pt, fg_50, fg_40_49, fg_39, missed_fg_40, missed_fg_39_less, def_td, def_int, def_fum, def_block,
           def_safety, def_sack, def_0, def_1_6, def_7_13, def_14_20, def_21_27, def_28_34, def_35, pa, pa_miss, def_avg, bye) %>%
    mutate_if(is.logical, as.numeric) 
  
  print("ESPN - K")
  
  Sys.sleep(1)
  
  webElem2 <- remDr$findElement(using = "xpath", "/html/body/div[1]/div[1]/div/div/div[5]/div[2]/div[2]/div[1]/div/div[2]/div[3]/div/div/label[8]")
  webElem2$clickElement()
  
  Sys.sleep(1)
  
  k_link <- remDr$getPageSource("outerHTML")[[1]] %>%
    read_html()
  
  k_player <- k_link %>%
    html_nodes(".truncate:nth-child(1) .pointer") %>%
    html_text()
  
  k_team <- k_link %>%
    html_nodes(".playerinfo__playerteam") %>%
    html_text()
  
  k_table <- k_link %>%
    html_nodes(".Table__Scroller > table:nth-child(1)") %>%
    html_table()
  
  k_table <- k_table[[1]] %>%
    header.true()
  
  k_table$player <- k_player
  k_table$team <- k_team
  
  k_table <- k_table %>%
    mutate(fg_39 = sapply(strsplit(`FG39/FGA39`, "/"), `[`, 1),
           fg_39_a = sapply(strsplit(`FG39/FGA39`, "/"), `[`, 2),
           fg_40_49 = sapply(strsplit(`FG49/FGA49`, "/"), `[`, 1),
           fg_40_49_a = sapply(strsplit(`FG49/FGA49`, "/"), `[`, 2),
           fg_50 = sapply(strsplit(`FG50+/FGA50+`, "/"), `[`, 1),
           fg_50_a = sapply(strsplit(`FG50+/FGA50+`, "/"), `[`, 2),
           pa = sapply(strsplit(`XP/XPA`, "/"), `[`, 1),
           pa_a = sapply(strsplit(`XP/XPA`, "/"), `[`, 2)) %>%
    select(player, team, fg_39, fg_39_a, fg_40_49, fg_40_49_a, fg_50, fg_50_a, pa, pa_a)
  
  num_cols <- colnames(k_table)
  num_cols <- num_cols[-c(1,2)]
  
  k_table[num_cols] <- sapply(k_table[num_cols],as.numeric)
  
  k_table[num_cols] <- sapply(k_table[num_cols],replace_na, 0)
  
  k_table <- k_table %>%
    mutate(player = str_remove(player, "'"),
           player = str_remove_all(player, "\\."),
           player = str_remove_all(player, " Jr"),
           player = str_remove_all(player, " IV"),
           player = str_remove_all(player, " III"),
           player = str_remove_all(player, " II"),
           player = tolower(player),
           team = toupper(team),
           position = "K",
           source = "ESPN",
           pass_td = NA, 
           pass_yd = NA, 
           int = NA,
           rec = NA,
           rec_yd = NA,
           rush_yd = NA,
           rush_td = NA,
           fum = NA,
           rec_td = NA,
           two_pt = NA,
           missed_fg_40 = (fg_40_49_a+fg_50_a)-(fg_40_49+fg_50),
           missed_fg_39_less = fg_39_a-fg_39,
           def_td = NA,
           def_int = NA,
           def_fum = NA,
           def_block = NA,
           def_safety = NA,
           def_sack = NA,
           def_0 = NA,
           def_1_6 = NA,
           def_7_13 = NA,
           def_14_20 = NA,
           def_21_27 = NA,
           def_28_34 = NA,
           def_35 = NA,
           def_avg = NA,
           pa_miss = pa_a-pa,
           bye = NA) %>%
    select(player, team, position, source, pass_td, pass_yd, int, fum, rush_yd, rush_td, rec, rec_yd, rec_td,
           two_pt, fg_50, fg_40_49, fg_39, missed_fg_40, missed_fg_39_less, def_td, def_int, def_fum, def_block,
           def_safety, def_sack, def_0, def_1_6, def_7_13, def_14_20, def_21_27, def_28_34, def_35, pa, pa_miss, def_avg, bye) %>%
    mutate_if(is.logical, as.numeric) 
  
  espn_scrape <- rbind(qb_table, flex_table, k_table)
  
  return(espn_scrape)
}
espn_scrape <- scrape_espn()

#### Combine Data ####


#### Images ####
image_link <- data.frame("team" = (pff_scrape %>%
                                     arrange(team) %>%
                                     group_by() %>%
                                     distinct(team))$team) %>%
  mutate(team_image = paste0(team, ".png"))

#### Combine ####
final_scrape_initial <- rbind(pff_scrape, fantasy_pros_scrape, fantasy_sharks_scrape,
                             nfl_scrape, cbs_scrape, razzball_scrape, number_fire_scrape, espn_scrape,
                             fantasy_footballers_scrape, for4_scrape) %>%
  mutate(team = case_when(
    team == "ARZ" ~ "ARI",
    team == "BLT" ~ "BAL",
    team == "CLV" ~ "CLE",
    team == "GBP" ~ "GB",
    team == "HST" ~ "HOU",
    team == "JAC" ~ "JAX",
    team == "KCC" ~ "KC",
    team == "LA" ~ "LAR",
    team == "LVR" ~ "LV",
    team == "NEP" ~ "NE",
    team == "NOS" ~ "NO",
    team == "SFO" ~ "SF",
    team == "TBB" ~ "TB",
    team == "WSH" ~ "WAS",
    TRUE ~ team
  )) %>%
  filter(!team %in% c("RB", "QB", "TE","K","WR","FA")) %>%
  filter(!source == "NFL")

final_scrape_dst_team <- final_scrape_initial %>%
  filter(position == "DST",
         !is.na(team)) %>%
  distinct(team, player) %>%
  rename(team_backup = team)

final_scrape <- final_scrape_initial %>%
  left_join(final_scrape_dst_team, by = c("player")) %>%
  mutate(team = ifelse(is.na(team), team_backup, team)) %>%
  select(-team_backup) %>%
  rowwise() %>%
  mutate(def_avg_fpts = case_when(
    def_avg <= 20 ~ 17,
    def_avg > 20 & def_avg <= 27 ~ 0,
    def_avg > 27 ~ -17,
    TRUE ~ 0
  ),
         fantasy_points = sum(pass_td*4,pass_yd*.04, int*-1, fum*-2, rush_yd*.1, rush_td*6, rec_yd*.1, rec_td*6,
                              rec*.5, two_pt*2,fg_50*5, fg_40_49*4, fg_39*3,def_td*6,def_int*2,def_fum*2, def_safety*2,
                              def_sack*1, def_0*10, def_1_6*7, def_7_13*4, def_14_20*1, def_21_27*0, def_28_34*-1, def_35*-4,
                              pa*1, def_avg_fpts, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(position = case_when(
    player == "cordarrelle patterson" ~ "WR",
    TRUE ~ position
  ),
  player = case_when(
    player == "ja’marr chase" ~ "jamarr chase",
    player == "d’andre swift" ~ "dandre swift",
    player == "ka’imi fairbairn" ~ "kaimi fairbairn",
    player == "james o’shaughnessy" ~ "james oshaughnessy",
    player == "michael badgley" ~ "mike badgley",
    player == "joshua palmer" ~ "josh palmer",
    player == "william fuller v" ~ "will fuller",
    player == "will fuller v" ~ "will fuller",
    player == "n’keal harry" ~ "nkeal harry",
    player == "tre’quan smith" ~ "trequan smith",
    player == "samuel ficken" ~ "sam ficken",
    player == "keelan cole sr" ~ "keelan cole",
    player == "d’wayne eskridge" ~ "dwayne eskridge",
    player == "scott miller" ~ "scotty miller",
    TRUE ~ player
  )
  ) %>%
  filter(!player == "")

write_csv(final_scrape, "2021_nfl_season_projections.csv")



