library(ggtext)
library(tidyverse)
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(shinycssloaders) 
library(DT)
library(DBI)
library(RPostgres)
library(RPostgreSQL)
library(dbplyr)
library(lubridate)
library(plotly)
library(RSQLite)
library(odbc)   
library(pool) 
library(tibbletime)
library(ggiraph)
library(ggplot2)
library(scales)
library(quantmod)
library(reactable)

# the following are the tidyverse packages used in mcoo
# library(purrr)
# library(stringr)
# library(tidyr)
# library(dplyr)

# setwd("C:/Users/BradMiller/Documents/FantasyDataExploration/Fantasy Football/Drafting App")


source('splash_colors_functions_prod.R')

final_scrape <- read_csv("2021_nfl_season_projections.csv") %>%
  mutate(team = case_when(
    team == "CLE" ~ "CLV",
    team == "ARI" ~ "ARZ",
    team == "BAL" ~ "BLT",
    team == "LAR" ~ "LA",
    team == "HOU" ~ "HST",
    TRUE ~ team
  )) %>%
  mutate(team = paste0(team,".png"))
# final_scrape <- read_csv("final_scrape.csv") %>%
#   mutate(team = paste0(team,".png"))

# UI ------
ui <- fluidPage(theme = "splash_theme.css",
                #favicon of splash logo
                # tags$head(tags$link(rel="shortcut icon", href="splash-fav-32.ico")),
                dashboardPage(title = "FF Drafting Tool",
                              dashboardHeader(title = tags$div(class = "app_title_header",
                                                               tags$img(src = 'fantasy_football.png', height = 50, width =80)
                                                               #h3(class = "app_title", "Part Hub")
                              )
                              ), 
                              dashboardSidebar( 
                                sidebarMenu(id = 'sidebar_menu',
                                            menuItem('Draft Settings', tabName = "draft_settings", selected = T),
                                            menuItem('Drafting Tool', tabName = "drafting_tab")
                                )
                              ),
                              dashboardBody(  
                                tabItems(
                                  #### Tab 1: Settings ####
                                  tabItem(
                                    tabName = "draft_settings",
                                    fluidRow(
                                      column(
                                        width = 12,
                                        tags$div(
                                          class = "ggplot_box",
                                          h1("Description"),
                                          p(
                                            "To properly set up the tool and optimize it for your specific draft, it is important that the league 
                                            settings and scoring rules are adjusted. The most important things to adjust are the League Settings, points per reception, and points per passing TD. The League Settings will help calculate the proper 
                                            VOR and Rank Comparison for your specific draft so make sure that you have the proper drafting site and 
                                            league format. The rest of the scoring rules besides receptions are pretty standard across most leagues but it 
                                            definitely doesn't hurt to check in case your league has any unique rules."
                                          )
                                        ),
                                      )
                                    ),
                                    column(
                                      width = 3,
                                      tags$div(class = "ggplot_box",
                                               # h2("Campaign Trends"),
                                               h1("League Setup"),
                                               selectInput("league_source", "Drafting Site: ", c("ESPN", "Yahoo", "CBS"), selected = "ESPN"),
                                               numericInput("league_total_teams", "Teams in League: ", 12, min = 2, max = 30, step = 1),
                                               numericInput("league_total_roster", "Total Players on Team: ", 15, min = 10, max = 20, step = 1),
                                               numericInput("league_total_starters", "Total Starting Slots: ", 9, min = 0, max = 20, step = 1),
                                               numericInput("league_total_qb", "Starting QB's: ", 1, min = 1, max = 3, step = 1),
                                               numericInput("league_total_rb", "Starting RB's: ", 2, min = 1, max = 3, step = 1),
                                               numericInput("league_total_wr", "Starting WR's: ", 2, min = 1, max = 3, step = 1),
                                               numericInput("league_total_te", "Starting TE's: ", 1, min = 1, max = 3, step = 1),
                                               numericInput("league_total_dst", "Starting DST's: ", 1, min = 1, max = 2, step = 1),
                                               numericInput("league_total_k", "Starting K's: ", 1, min = 1, max = 2, step = 1),
                                               numericInput("league_total_flex", "Starting FLEX's: ", 1, min = 1, max = 3, step = 1))
                                    ),
                                    column(
                                      width = 3,
                                      tags$div(class = "ggplot_box",
                                               # h2("Campaign Trends"),
                                               h1("Offensive Scoring"),
                                               numericInput("rec_score", "Reception: ", .5, min = 0, max = 2, step = .5),
                                               numericInput("pass_td_score", "Pass TD: ", 4, min = 0, max = 12, step = 1),
                                               numericInput("pass_yd_score", "Pass Yd: ", .04, min = 0, max = 1, step = .01),
                                               numericInput("int_score", "Int: ", -1, min = -10, max = 0, step = 1),
                                               numericInput("rush_yd_score", "Rush Yd: ", .1, min = 0, max = 1, step = .05),
                                               numericInput("rush_td_score", "Rush TD: ", 6, min = 0, max = 12, step = 1),
                                               numericInput("fum_score", "Fum Lost: ", -2, min = -10, max = 0, step = 1),
                                               numericInput("rec_yd_score", "Rec Yd: ", .1, min = 0, max = 2, step = .1),
                                               numericInput("rec_td_score", "Rec TD: ", 6, min = 0, max = 10, step = 1),
                                               numericInput("two_pt_score", "2pt Conversion: ", 2, min = 0, max = 10, step = 1))
                                    ),
                                    column(
                                      width = 3,
                                      tags$div(class = "ggplot_box",
                                               # h2("Campaign Trends"),
                                               h1("Kicking Scoring"),
                                               numericInput("pa_score", "PA: ", 1, min = 0, max = 10, step = 1),
                                               numericInput("fg_39_score", "FG <= 39: ", 3, min = 0, max = 10, step = 1),
                                               numericInput("fg_40_49_score", "FG 40-49: ", 4, min = 0, max = 10, step = 1),
                                               numericInput("fg_50_score", "FG 50+: ", 5, min = 0, max = 10, step = 1),
                                               numericInput("pa_miss_score", "PA Miss: ", 0, min = -10, max = 0, step = 1),
                                               numericInput("fg_39_miss_score", "FG <= 39 Miss: ", 0, min = -10, max = 0, step = 1),
                                               numericInput("fg_40_miss_score", "FG 40+ Miss: ", 0, min = -10, max = 0, step = 1))
                                    ),
                                    column(
                                      width = 3,
                                      tags$div(class = "ggplot_box",
                                               # h2("Campaign Trends"),
                                               h1("Defensive Scoring"),
                                               numericInput("def_td_score", "Def TD: ", 6, min = 0, max = 10, step = 1),
                                               numericInput("def_int_score", "INT: ", 2, min = 0, max = 10, step = 1),
                                               numericInput("def_fum_score", "Fum Rec: ", 2, min = 0, max = 10, step = 1),
                                               numericInput("def_block_score", "Block: ", 2, min = 0, max = 10, step = 1),
                                               numericInput("def_safety_score", "Safety: ", 2, min = 0, max = 10, step = 1),
                                               numericInput("def_sack_score", "Sack: ", 1, min = 0, max = 10, step = 1),
                                               numericInput("def_0_score", "0 PA: ", 10, min = 0, max = 20, step = 1),
                                               numericInput("def_1_6_score", "1-6 PA: ", 7, min = 0, max = 20, step = 1),
                                               numericInput("def_7_13_score", "7-13 PA: ", 4, min = -10, max = 15, step = 1),
                                               numericInput("def_14_20_score", "14-20 PA: ", 1, min = -10, max = 10, step = 1),
                                               numericInput("def_21_27_score", "21-27 PA: ", 0, min = -10, max = 10, step = 1),
                                               numericInput("def_28_34_score", "28-34 PA: ", -1, min = -10, max = 10, step = 1),
                                               numericInput("def_35_score", "35+ PA: ", -4, min = -10, max = 10, step = 1))
                                    ),
                                    fluidRow()
                                  ),
                                  #### Tab 6: Daily Sales Data ####
                                  tabItem(
                                    tabName = "drafting_tab",
                                    fluidRow(
                                      column(
                                        width = 12,
                                        tags$div(
                                          class = "ggplot_box",
                                          h1("Description"),
                                          h3(strong('How to use the tool')),
                                          p(strong('1. Add players to your team/remove drafted players:'),
                                            " Use the Remove/Add Players to Lineup in the top right corner. Clicking 
                                            remove player will remove a player from the draft board. Clicking Add player will add 
                                            them to your roster in the table underneath. If you accidentily remove or add a player to your team, you 
                                            can undo it with the undo remove and undo add buttons underneath your team.", br(),
                                            strong("2. Filtering the Drafting Table: "),
                                            "Underneath each column header is a box that you can use to search for specific players or positions. Filtering 
                                            is as simple as typing in the related box and removing the text to undo the filter.", br(),
                                          ),
                                          br(), h3(strong("Important Statistics Explained")),
                                          p(strong('1. VOR (Value over Replacement): '),
                                            "This statistic calculates the amount of points a player is expected to accumulate during 
                                            the season compared to the next best option you can pick up off the waiver wire. To calculate 
                                            this field, I've done a historical analysis on how many players of each position typically get drafted 
                                            or are rostered at any point in time. If you search for a VOR of -1, that is the best player that is 
                                            expected to start the season on waivers. This statistic helps calculate value in a draft because even though quarterbacks 
                                            might recieve the most points, they are also the easiest to replace from week to week versus skilled positions.", br(),
                                            strong('2. Rank Comparison: '),
                                            "This evaluates where the player is ranked in the aggregate model versus where they are ranked on the specific 
                                            site that you are using to draft. For example if If a player has a green +10, this means that they are ranked 10 spots 
                                            higher on the overall than on your drafting site. If a player has a green -15, that means the player is ranked 15 
                                            spots lower on the overall than on your drafting site. The purpose of this is to help decide which players to draft 
                                            if you're torn between 2. If one has a +20 green and the other has a -2 red, the logic would be to take the red player
                                            because the green one will likely still be there on your next pick due to being undervalued by the draft site.", br(),
                                          strong('3. Prediction: '),
                                          "This score is calculated by taking the average of the 10+ projection sites that are pulled in. The idea 
                                          is that the thought of the group of experts will always beat the thought of any individual. This idea has definitely 
                                          rung true while studying the projections from different sites from season to season and week to week. The average of the 
                                          sources consistently outperforms any one analyst/site.", br(),
                                          strong('4. Ceiling/Floor: '),
                                          "This is is simply looking and the highest and lowest individual projection from the sites pulled for each player.", br(),
                                          strong('5. Risk: '),
                                           "This looks at the range of predictions from the different websites. A low % means that all of the websites had a 
                                           very similar projection while a high % means that the different websites had drastically different projections.",
                                        )
                                      )
                                      )
                                      
                                    ),
                                    fluidRow(
                                      column(
                                        width = 9,
                                        tags$div(class = "ggplot_box",
                                                 # h2("Campaign Trends"),
                                                 h1("Draft Board"),
                                                 reactableOutput("drafting_rt_output") %>% shinycssloaders::withSpinner(type = 8, color = getOption("spinner.color", default = "#B4C5E4")))
                                      ),
                                      column(
                                        width = 3,
                                        tags$div(class = "ggplot_box",
                                          h3("Remove/Add Players to Lineup"),
                                          uiOutput("select_player_ui"),
                                          fluidRow(column(width = 1),
                                                   actionButton("remove_player", "Remove Player"),
                                                   actionButton("add_player", "Add Player"))
                                        ),
                                        tags$div(class = "ggplot_box",
                                                 h3("Current Lineup"),
                                                 reactableOutput("lineup_rt_output")  %>% shinycssloaders::withSpinner(type = 8, color = getOption("spinner.color", default = "#B4C5E4")),
                                                 fluidRow(column(width = 1),
                                                          actionButton("undo_remove", "Undo Remove"),
                                                          actionButton("undo_add", "Undo Add"))
                                        )
                                      )
                                    )
                                  )
                                ) # End Tab Items
                              ) #end Dashboard Body
                ) # end DashboardPage
) # end FluidPage (for theme css)

# Server ------------
server <- function(input, output, session) {
  

  #### Tab 6: Daily Sales File ####


  #### T6/R1/C2: Daily Sales by Campaign Trends ####
  rating_cols <- c("rank_difference")

  rating_column <- function(maxWidth = 100, ...) {
    colDef(maxWidth = maxWidth, align = "center", class = "cell number", ...)
  }

  make_color_pal <- function(colors, bias = 1) {
    get_color <- colorRamp(colors, bias = bias)
    function(x) rgb(get_color(x), maxColorValue = 255)
  }

  drafting_table_original_rdf <- reactive({
    df_full <- final_scrape %>%
      rowwise() %>%
      mutate(fantasy_points = sum(pass_td*input$pass_td_score,
                                  pass_yd*input$pass_yd_score,
                                  int*input$int_score,
                                  fum*input$fum_score,
                                  rush_yd*input$rush_yd_score,
                                  rush_td*input$rush_td_score,
                                  rec_yd*input$rec_yd_score,
                                  rec_td*input$rec_td_score,
                                  rec*input$rec_score,
                                  # rec*.5,
                                  two_pt*input$two_pt_score,
                                  fg_50*input$fg_50_score,
                                  fg_40_49*input$fg_40_49_score,
                                  fg_39*input$fg_39_score,
                                  def_td*input$def_td_score,
                                  def_int*input$def_int_score,
                                  def_fum*input$def_fum_score,
                                  def_safety*input$def_safety_score,
                                  def_sack*input$def_sack_score,
                                  def_0*input$def_0_score,
                                  def_1_6*input$def_1_6_score,
                                  def_7_13*input$def_7_13_score,
                                  def_14_20*input$def_14_20_score,
                                  def_21_27*input$def_21_27_score,
                                  def_28_34*input$def_28_34_score,
                                  def_35*input$def_35_score,
                                  pa*input$pa_score, 
                                  missed_fg_39_less*input$fg_39_miss_score,
                                  missed_fg_40*input$fg_40_miss_score,
                                  pa_miss*input$pa_miss_score,
                                  def_avg_fpts, na.rm = TRUE)) %>%
      ungroup() %>%
      group_by(player, team, position) %>%
      summarize(avg_pred = mean(fantasy_points, na.rm = TRUE),
                max_pred = max(fantasy_points, na.rm = TRUE),
                min_pred = min(fantasy_points, na.rm = TRUE),
                st_dev = sd(fantasy_points, na.rm = TRUE)) %>%
      ungroup() %>%
      mutate(st_dev_pct = st_dev/avg_pred)

    team_bye <- final_scrape %>%
      group_by(team) %>%
      summarize(bye = mean(bye, na.rm = TRUE)) %>%
      ungroup()

    teams <- input$league_total_teams
    qb <- input$league_total_qb
    wr <- input$league_total_wr
    te <- input$league_total_te
    rb <- input$league_total_rb
    k <- input$league_total_k
    dst <- input$league_total_dst
    req <- input$league_total_starters-input$league_total_flex
    total <- input$league_total_roster
    remaining_players <- total-req
    bench_qb <- .08
    bench_rb <- .33
    bench_wr <- .43
    bench_te <- .08
    bench_dst <- .06
    bench_k <- .03

    rem_qb <- remaining_players*bench_qb
    rem_rb <- remaining_players*bench_rb
    rem_wr <- remaining_players*bench_wr
    rem_te <- remaining_players*bench_te
    rem_dst <- remaining_players*bench_dst
    rem_k <- remaining_players*bench_k

    total_qb <- round((qb*teams)+(rem_qb*teams), digits = 0)
    total_wr <- round((wr*teams)+(rem_wr*teams), digits = 0)
    total_te <- round((te*teams)+(rem_te*teams), digits = 0)
    total_rb <- round((rb*teams)+(rem_rb*teams), digits = 0)
    total_k <- round((k*teams)+(rem_k*teams), digits = 0)
    total_dst <- round((dst*teams)+(rem_dst*teams), digits = 0)
    
    df_specific <- if(input$league_source == "CBS"){
      df_specific <- final_scrape %>%
        filter(source == "CBS") %>%
        rowwise() %>%
        mutate(fantasy_points = sum(pass_td*input$pass_td_score,
                                    pass_yd*input$pass_yd_score,
                                    int*input$int_score,
                                    fum*input$fum_score,
                                    rush_yd*input$rush_yd_score,
                                    rush_td*input$rush_td_score,
                                    rec_yd*input$rec_yd_score,
                                    rec_td*input$rec_td_score,
                                    rec*input$rec_score,
                                    # rec*.5,
                                    two_pt*input$two_pt_score,
                                    fg_50*input$fg_50_score,
                                    fg_40_49*input$fg_40_49_score,
                                    fg_39*input$fg_39_score,
                                    def_td*input$def_td_score,
                                    def_int*input$def_int_score,
                                    def_fum*input$def_fum_score,
                                    def_safety*input$def_safety_score,
                                    def_sack*input$def_sack_score,
                                    def_0*input$def_0_score,
                                    def_1_6*input$def_1_6_score,
                                    def_7_13*input$def_7_13_score,
                                    def_14_20*input$def_14_20_score,
                                    def_21_27*input$def_21_27_score,
                                    def_28_34*input$def_28_34_score,
                                    def_35*input$def_35_score,
                                    pa*input$pa_score, 
                                    missed_fg_39_less*input$fg_39_miss_score,
                                    missed_fg_40*input$fg_40_miss_score,
                                    pa_miss*input$pa_miss_score,
                                    def_avg_fpts, na.rm = TRUE)) %>%
        ungroup() %>%
        select(player, team, position, fantasy_points) %>%
        rename(avg_pred = fantasy_points)
    } else {
      if(input$league_source == "Yahoo") {
        df_specific <- final_scrape %>%
          filter(source == "Fantasy Pros/Yahoo") %>%
          rowwise() %>%
          mutate(fantasy_points = sum(pass_td*input$pass_td_score,
                                      pass_yd*input$pass_yd_score,
                                      int*input$int_score,
                                      fum*input$fum_score,
                                      rush_yd*input$rush_yd_score,
                                      rush_td*input$rush_td_score,
                                      rec_yd*input$rec_yd_score,
                                      rec_td*input$rec_td_score,
                                      rec*input$rec_score,
                                      # rec*.5,
                                      two_pt*input$two_pt_score,
                                      fg_50*input$fg_50_score,
                                      fg_40_49*input$fg_40_49_score,
                                      fg_39*input$fg_39_score,
                                      def_td*input$def_td_score,
                                      def_int*input$def_int_score,
                                      def_fum*input$def_fum_score,
                                      def_safety*input$def_safety_score,
                                      def_sack*input$def_sack_score,
                                      def_0*input$def_0_score,
                                      def_1_6*input$def_1_6_score,
                                      def_7_13*input$def_7_13_score,
                                      def_14_20*input$def_14_20_score,
                                      def_21_27*input$def_21_27_score,
                                      def_28_34*input$def_28_34_score,
                                      def_35*input$def_35_score,
                                      pa*input$pa_score, 
                                      missed_fg_39_less*input$fg_39_miss_score,
                                      missed_fg_40*input$fg_40_miss_score,
                                      pa_miss*input$pa_miss_score,
                                      def_avg_fpts, na.rm = TRUE)) %>%
          ungroup() %>%
          select(player, team, position, fantasy_points) %>%
          rename(avg_pred = fantasy_points)
      } else {
        df_specific <- final_scrape %>%
          filter(source == "ESPN") %>%
          rowwise() %>%
          mutate(fantasy_points = sum(pass_td*input$pass_td_score,
                                      pass_yd*input$pass_yd_score,
                                      int*input$int_score,
                                      fum*input$fum_score,
                                      rush_yd*input$rush_yd_score,
                                      rush_td*input$rush_td_score,
                                      rec_yd*input$rec_yd_score,
                                      rec_td*input$rec_td_score,
                                      rec*input$rec_score,
                                      # rec*.5,
                                      two_pt*input$two_pt_score,
                                      fg_50*input$fg_50_score,
                                      fg_40_49*input$fg_40_49_score,
                                      fg_39*input$fg_39_score,
                                      def_td*input$def_td_score,
                                      def_int*input$def_int_score,
                                      def_fum*input$def_fum_score,
                                      def_safety*input$def_safety_score,
                                      def_sack*input$def_sack_score,
                                      def_0*input$def_0_score,
                                      def_1_6*input$def_1_6_score,
                                      def_7_13*input$def_7_13_score,
                                      def_14_20*input$def_14_20_score,
                                      def_21_27*input$def_21_27_score,
                                      def_28_34*input$def_28_34_score,
                                      def_35*input$def_35_score,
                                      pa*input$pa_score, 
                                      missed_fg_39_less*input$fg_39_miss_score,
                                      missed_fg_40*input$fg_40_miss_score,
                                      pa_miss*input$pa_miss_score,
                                      def_avg_fpts, na.rm = TRUE)) %>%
          ungroup() %>%
          select(player, team, position, fantasy_points) %>%
          rename(avg_pred = fantasy_points)
      }
    }

    var_calculation_specific <- df_specific %>%
      group_by(position) %>%
      mutate(pos_rank = rank(desc(avg_pred))) %>%
      ungroup() %>%
      filter(case_when(
        position == "QB" ~ pos_rank >= total_qb,
        position == "RB" ~ pos_rank >= total_rb,
        position == "WR" ~ pos_rank >= total_wr,
        position == "TE" ~ pos_rank >= total_te,
        position == "DST" ~ pos_rank >= total_dst,
        position == "K" ~ pos_rank >= total_k
      )) %>%
      select(position, avg_pred) %>%
      group_by(position) %>%
      summarize(avg_pred = max(avg_pred, na.rm = TRUE)) %>%
      ungroup() %>%
      rename(replacement_points = avg_pred)

    df_specific <- df_specific %>%
      left_join(var_calculation_specific, by = c("position")) %>%
      mutate(value_over_repl = avg_pred-replacement_points,
             value_over_repl = ifelse(is.na(value_over_repl), 0, value_over_repl),
             overall_rank = rank(desc(value_over_repl))) %>%
      group_by(position) %>%
      mutate(pos_rank = rank(desc(value_over_repl))) %>%
      ungroup() %>%
      filter(avg_pred >= 40) %>%
      select(player, team, position, overall_rank) %>%
      rename(specific_rank = overall_rank)

    var_calculation <- df_full %>%
      group_by(position) %>%
      mutate(pos_rank = rank(desc(avg_pred))) %>%
      ungroup() %>%
      filter(case_when(
        position == "QB" ~ pos_rank >= total_qb,
        position == "RB" ~ pos_rank >= total_rb,
        position == "WR" ~ pos_rank >= total_wr,
        position == "TE" ~ pos_rank >= total_te,
        position == "DST" ~ pos_rank >= total_dst,
        position == "K" ~ pos_rank >= total_k
      )) %>%
      select(position, avg_pred) %>%
      group_by(position) %>%
      summarize(avg_pred = max(avg_pred, na.rm = TRUE)) %>%
      ungroup() %>%
      rename(replacement_points = avg_pred)


    df_full <- df_full %>%
      left_join(team_bye, by = c("team")) %>%
      left_join(var_calculation, by = "position") %>%
      mutate(value_over_repl = avg_pred-replacement_points,
             value_over_repl = ifelse(is.na(value_over_repl), 0, value_over_repl),
             overall_rank = rank(desc(value_over_repl))) %>%
      group_by(position) %>%
      mutate(pos_rank = rank(desc(value_over_repl))) %>%
      ungroup() %>%
      filter(avg_pred >= 50) %>%
      left_join(df_specific, by = c("player", "position", "team")) %>%
      mutate(rank_difference = specific_rank-overall_rank,
             rank_difference = ifelse(is.na(rank_difference), 0, rank_difference)) %>%
      rename(pos = position) %>%
      select(player, team, pos, bye, avg_pred, max_pred, min_pred, value_over_repl,
             st_dev_pct, pos_rank, overall_rank, rank_difference) %>%
      arrange(overall_rank)

    df_full


  })


  # drafting_table_rdf <- reactiveValues(df = drafting_table_original_rdf())
  drafting_table_rdf <- reactiveValues(df = NULL)
  
  # drafting_table_rdf$df <- drafting_table_original_rdf()
  observe({
    drafting_table_rdf$df <- drafting_table_original_rdf()
  })

  # values <- reactiveValues(dfWorking = df)
  #
  removed_rdf <- reactiveValues(df = data.frame("player" = NA,
                                                "team" = NA,
                                                "pos" = NA,
                                                "bye" = NA,
                                                "avg_pred" = NA,
                                                "max_pred" = NA,
                                                "min_pred" = NA,
                                                "value_over_repl" = NA,
                                                "st_dev_pct" = NA,
                                                "pos_rank" = NA,
                                                "overall_rank" = NA,
                                                "rank_difference" = NA)[-1,])

  observeEvent(input$remove_player,{

    if (input$select_player %in% drafting_table_rdf$df$player) {
      temp_player <- drafting_table_rdf$df %>%
        filter(player == input$select_player)

      removed_rdf$df <- rbind(removed_rdf$df, temp_player)

      drafting_table_rdf$df <- drafting_table_rdf$df %>%
        filter(!player == input$select_player)
    }
  })

  observeEvent(input$undo_remove,{
    temp_player <- removed_rdf$df[nrow(removed_rdf$df),]

    drafting_table_rdf$df <- rbind(temp_player, drafting_table_rdf$df)

    removed_rdf$df <- removed_rdf$df[-nrow(removed_rdf$df),]
  })

  output$drafting_rt_output <- renderReactable({
    df <- drafting_table_rdf$df

    off_rating_color <- make_color_pal(c("#ff2700", "#F8F8F8", "#44ab43"))

    table <- reactable(
      df,
      pagination = TRUE,
      defaultPageSize = 15,
      filterable = TRUE,
      defaultColGroup = colGroup(headerClass = "group-header"),
      defaultColDef = colDef(headerClass = "header"),
      columnGroups = list(
        colGroup(name = "Player Info", columns = c("player", "pos", "bye")),
        colGroup(name = "Player Projections", columns = c("avg_pred", "max_pred", "min_pred", "value_over_repl", "st_dev_pct")),
        colGroup(name = "Player Rankings", columns = c("pos_rank", "overall_rank", "rank_difference"))
      ),
      striped = TRUE,
      # onClick = "select",
      columns = list(
        player = colDef(
          name = "Player",
          align = "left",
          minWidth = 180,
          cell = function(value) {
            data <- df[df$player == value,]
            image <- img(src = sprintf("%s", data$team), height = "25px", width = "33px",alt = value)
            tagList(
              div(style = list(display = "inline-block", width = "45px"), image),
              value
            )
          }
        ),
        team = colDef(
          show = FALSE
        ),
        # team = colDef(
        #   name = "Team",
        #   align = "center"
        # ),
        # team = colDef(align = "center",
        #               name = "Team",
        #               cell = function(value) {
        #                 data <- df[df$player == value,]
        #   image <- img(src = sprintf("%s", value), height = "24px", alt = value)
        #   image
        #   # tagList(
        #   #   div(style = list(display = "inline-block", width = "45px"), image)
        #   # )
        # }),
        pos = colDef(
          name = "Position",
          align = "center",
          minWidth = 65
        ),
        bye = colDef(
          name = "Bye",
          align = "center",
          minWidth = 45
        ),
        avg_pred = colDef(
          name = "Prediction",
          align = "right",
          minWidth = 90,
          format = colFormat(
            digits = 0
          ),
          class = "border-left"
        ),
        max_pred = colDef(
          name = "Ceiling",
          align = "right",
          minWidth = 85,
          format = colFormat(
            digits = 0
          )
        ),
        min_pred = colDef(
          name = "Floor",
          align = "right",
          minWidth = 85,
          format = colFormat(
            digits = 0
          )
        ),
        value_over_repl = colDef(
          name = "VOR",
          align = "right",
          minWidth = 85,
          format = colFormat(
            digits = 0
          )
        ),
        st_dev_pct = colDef(
          name = "Risk",
          align = "right",
          minWidth = 85,
          format = colFormat(
            percent = TRUE,
            digits = 0
          )
        ),
        pos_rank = colDef(
          name = "Pos. Rank",
          align = "center",
          class = "border-left",
          minWidth = 80,
          format = colFormat(
            digits = 0
          )
        ),
        overall_rank = colDef(
          name = "Ovr. Rank",
          align = "center",
          minWidth = 80,
          format = colFormat(
            digits = 0
          )
        ),
        rank_difference = rating_column(
          name = "Rank Comparison",
          minWidth = 140,
          format = colFormat(
            percent = TRUE,
            digits = 0
          ),
          cell = function(value) {
            adjusted_value <- ifelse(value>= 12, 12, ifelse(value<= (-12), -12, value))
            scaled <- (adjusted_value - (-12)) / (12 - (-12))
            color <- off_rating_color(scaled)
            value <- paste0(value)
            div(class = "spi-rating", style = list(background = color), value)
          }
        )
      )
    )

    table
  })

  output$select_player_ui <- renderUI({
    selectizeInput("select_player", "Select Player", drafting_table_rdf$df$player)
  })


  lineup_rdf <- reactiveValues(df = data.frame("player" = NA,
                                               "team" = NA,
                                               "pos" = NA,
                                               "bye" = NA,
                                               "avg_pred" = NA,
                                               "max_pred" = NA,
                                               "min_pred" = NA,
                                               "value_over_repl" = NA,
                                               "st_dev_pct" = NA,
                                               "pos_rank" = NA,
                                               "overall_rank" = NA,
                                               "rank_difference" = NA)[-1,])

  observeEvent(input$add_player,{

    if (input$select_player %in% drafting_table_rdf$df$player) {

      test_df <- reactiveValues(df = drafting_table_rdf$df %>%
                                  filter(player == input$select_player))

      lineup_rdf$df <- rbind(lineup_rdf$df, test_df$df)

      drafting_table_rdf$df <- drafting_table_rdf$df %>%
        filter(!player == input$select_player)
    }
  })

  observeEvent(input$undo_add,{

    temp <- lineup_rdf$df[nrow(lineup_rdf$df),]

    drafting_table_rdf$df <- rbind(temp, drafting_table_rdf$df)

    lineup_rdf$df <- lineup_rdf$df[-nrow(lineup_rdf$df),]
  })

  output$lineup_rt_output <- renderReactable({
    df <- lineup_rdf$df

    off_rating_color <- make_color_pal(c("#ff2700", "#F8F8F8", "#44ab43"))

    table <- reactable(
      df,
      pagination = FALSE,
      striped = TRUE,
      # onClick = "select",
      columns = list(
        player = colDef(
          name = "Player",
          align = "left",
          minWidth = 180,
          cell = function(value) {
            data <- df[df$player == value,]
            image <- img(src = sprintf("%s", data$team), height = "25px", width = "33px",alt = value)
            tagList(
              div(style = list(display = "inline-block", width = "45px"), image),
              value
            )
          }
        ),
        team = colDef(
          show = FALSE
        ),
        # team = colDef(
        #   name = "Team",
        #   align = "center"
        # ),
        # team = colDef(align = "center",
        #               name = "Team",
        #               cell = function(value) {
        #                 data <- df[df$player == value,]
        #   image <- img(src = sprintf("%s", value), height = "24px", alt = value)
        #   image
        #   # tagList(
        #   #   div(style = list(display = "inline-block", width = "45px"), image)
        #   # )
        # }),
        pos = colDef(
          name = "Position",
          align = "center",
          minWidth = 65
        ),
        bye = colDef(
          name = "Bye",
          align = "center",
          minWidth = 45
        ),
        avg_pred = colDef(
          show = FALSE
        ),
        max_pred = colDef(
          show = FALSE
        ),
        min_pred = colDef(
          show = FALSE
        ),
        value_over_repl = colDef(
          show = FALSE
        ),
        st_dev_pct = colDef(
          show = FALSE
        ),
        pos_rank = colDef(
          show = FALSE
        ),
        overall_rank = colDef(
          show = FALSE
        ),
        rank_difference = colDef(
          show = FALSE
        )
      )
    )

    table
  })

  # player_selected <- reactive()
  #
  # test_df <- reactive()

  # observeEvent(input$add_player,{
  #   # test_df <- reactive({drafting_table_rdf() %>%
  #   #   filter(player == input$select_player) %>%
  #   #   select(player, team, pos, bye)})
  #
  #   selected_player <- reactive({input$select_player})
  #
  #   lineup_rdf(lineup_rdf() %>% filter(!player == selected_player()))
  # })


  # test_df <- reactive({
  #   drafting_table_rdf() %>%
  #     filter(player == input$select_player) %>%
  #     select(player, team, pos, bye)
  # })
  #
  # lineup_rdf <- eventReactive(input$add_player, {
  #   dm <<- rbind.data.frame(lineup_rdf(),test_df())
  # })
  
  
}

shinyApp(ui = ui, server = server)
