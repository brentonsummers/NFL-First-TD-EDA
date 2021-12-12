# Global variables can go here
library(tidyverse)
library(nflfastR)
library(shiny)
library(ggimage)
library(ggthemes)
library(readxl)
library(patchwork)
library(ggwordcloud)

# Read in NFL Data from nflfastR Library
pbp_2020 <- load_pbp(2020)
pbp_2021 <- load_pbp(2021)

# Grab NFL colors from nflfastR
nfl_colors <- teams_colors_logos %>% 
    select(team_abbr, team_color) %>% 
    rename(posteam=team_abbr)

# Get dataframe of first TDs scored by game
tds_2020 <- pbp_2020 %>% 
    filter(season_type == 'REG') %>% 
    mutate(a_td = ifelse(!is.na(td_player_name), 1,0)) %>% 
    group_by(game_id) %>% 
    mutate(td_counter = cumsum(a_td)) %>% 
    filter(td_counter == 1) %>% 
    filter(a_td == 1) %>% 
    select(game_id, game_seconds_remaining, posteam, defteam,play_type, interception, fumble, desc, td_player_name, posteam_score_post, defteam_score_post, home_team, away_team,td_team, spread_line) %>% 
    mutate(favored = ifelse((td_team == away_team & spread_line < 0) | (td_team == home_team & spread_line > 0), 1,0))

tds_2021 <- pbp_2021 %>% 
    filter(season_type == 'REG') %>% 
    mutate(a_td = ifelse(!is.na(td_player_name), 1,0)) %>% 
    group_by(game_id) %>% 
    mutate(td_counter = cumsum(a_td)) %>% 
    filter(td_counter == 1) %>% 
    filter(a_td == 1) %>% 
    select(game_id, game_seconds_remaining, posteam, defteam,play_type, interception, fumble, desc, td_player_name, posteam_score_post, defteam_score_post, home_team, away_team,td_team, spread_line) %>% 
    mutate(favored = ifelse((td_team == away_team & spread_line < 0) | (td_team == home_team & spread_line > 0), 1,0))

# Add in positional data - manual addition
match_2020 <- read_xlsx("./2020_match.xlsx")
match_2021 <- read_xlsx("./2021_match.xlsx")

# Join first TD scorers to positional data
match_2020 <- match_2020 %>% 
    group_by(td_player_name, position) %>% 
    distinct(td_player_name)

match_2021 <- match_2021 %>% 
    group_by(td_player_name, position) %>% 
    distinct(td_player_name)

# Add in team colors
tds_2020 <- left_join(tds_2020, nfl_colors)
tds_2020 <- left_join(tds_2020, match_2020)

tds_2021 <- left_join(tds_2021, nfl_colors)
tds_2021 <- left_join(tds_2021, match_2021)


# Wordcloud aggregation
td_agg_2020 <- tds_2020 %>% 
    group_by(td_player_name,posteam, team_color) %>% 
    summarize(count = n())

td_agg_2021 <- tds_2021 %>% 
    group_by(td_player_name,posteam, team_color) %>% 
    summarize(count = n())

seasonslist <- list("2020" = td_agg_2020,
                "2021" = td_agg_2021)

# Positional aggregation
position_agg_2020 <- tds_2020 %>% 
    group_by(position) %>% 
    summarize(count = n())

position_agg_2020$position <- factor(position_agg_2020$position, levels = c("QB", "RB", "WR", "TE", "DEF", "ST"))

position_agg_2021 <- tds_2021 %>% 
    group_by(position) %>% 
    summarize(count = n())

position_agg_2021$position <- factor(position_agg_2021$position, levels = c("QB", "RB", "WR", "TE", "DEF", "ST"))

## Frequency of Favored team scoring first
favored_2020 <- tds_2020 %>% 
    group_by(favored) %>% 
    summarize(count = n())

tds_2020$abs_spread_line <- abs(tds_2020$spread_line)

favored_2021 <- tds_2021 %>% 
    group_by(favored) %>% 
    summarize(count = n())

tds_2021$abs_spread_line <- abs(tds_2021$spread_line)

# play call splits
redzone_splits_2020 <- pbp_2020 %>% 
    filter(play_type == 'run' | play_type == 'pass') %>% 
    filter(yardline_100 <= 20) %>% 
    group_by(posteam) %>% 
    summarise(rz_pass_percent_20 = sum(pass)/n())

redzone_splits_2021 <- pbp_2021 %>% 
    filter(play_type == 'run' | play_type == 'pass') %>% 
    filter(yardline_100 <= 20) %>% 
    group_by(posteam) %>% 
    summarise(rz_pass_percent_21 = sum(pass)/n())

# Combine Redzone play split data
redzone20_21 <- left_join(redzone_splits_2020,redzone_splits_2021, by = 'posteam')

teams_colors_logos$posteam <- teams_colors_logos$team_abbr

redzone20_21 <- left_join(redzone20_21,teams_colors_logos)


# Define the UI
ui <- fluidPage(
    tabsetPanel(
        tabPanel("Player Cloud",
            plotOutput("cloud21"),
            plotOutput("cloud20")),
        
        tabPanel("Spread Data",
                 plotOutput("spread21"),
                 plotOutput("spread20")),
        
        tabPanel("Positional Data",
                 plotOutput("position21"),
                 plotOutput("position20")),

        
        tabPanel("Redzone Play Split - 2020 v 2021",
                 plotOutput("yearSplit"))
                 
    )
)



# Define the server code
server <- function(input, output) {
    
    output$cloud21 <- renderPlot({
        td_agg_2021 %>% 
            ggplot(aes(label = td_player_name, 
                       size = count, 
                       color = team_color)) +
            geom_text_wordcloud() +
            scale_size_area(max_size = 7) +
            scale_color_identity(aesthetics = c(tds_2021$posteam, 'color')) +
            theme_minimal() +
            ggtitle("2021 - First TD")
    })
    
    output$cloud20 <- renderPlot({
        td_agg_2020 %>% 
            ggplot(aes(label = td_player_name, 
                       size = count, 
                       color = team_color)) +
            geom_text_wordcloud() +
            scale_size_area(max_size = 7) +
            scale_color_identity(aesthetics = c(tds_2020$posteam, 'color')) +
            theme_minimal() +
            ggtitle("2020 - First TD")
    })
    
    output$spread21 <- renderPlot({
        tds_2021 %>% 
            ggplot(aes(abs_spread_line)) +
            geom_histogram(aes(fill=factor(favored)), bins=20) +
            scale_fill_brewer(palette="Paired") +
            theme_minimal() + 
            xlab("Point Spread") +
            ylab("Number of First TDs") +
            ggtitle("2021 - First TD Breakdown by Spread") +
            xlim(c(0,20)) +
            scale_x_continuous(n.breaks = 10) +
            ylim(c(0,60)) +
            labs(fill = "Favored: 1 = Yes")
    })
    
    output$spread20 <- renderPlot({
        tds_2020 %>% 
            ggplot(aes(abs_spread_line)) +
            geom_histogram(aes(fill=factor(favored)), bins=20) +
            scale_fill_brewer(palette="Paired") +
            theme_minimal() + 
            xlab("Point Spread") +
            ylab("Number of First TDs") +
            ggtitle("2020 - First TD Breakdown by Spread") +
            xlim(c(0,20)) +
            scale_x_continuous(n.breaks = 10) +
            ylim(c(0,60)) +
            labs(fill = "Favored: 1 = Yes")
    })
    
    output$position21 <- renderPlot({
        position_agg_2021 %>% 
            ggplot(aes(position, count, fill=position)) + 
            geom_bar(stat = 'identity') +
            theme_minimal() +
            xlab("Position") +
            ylab("Number of First Touchdowns") + 
            ylim(c(0,100)) +
            ggtitle("2021 - First Touchdowns by Position") +
            scale_fill_brewer(palette = 'Paired') +
            theme(legend.position = 'none')
    })
    
    output$position20 <- renderPlot({
        position_agg_2020 %>% 
            ggplot(aes(position, count, fill=position)) + 
            geom_bar(stat = 'identity') +
            theme_minimal() +
            xlab("Position") +
            ylab("Number of First Touchdowns") + 
            ylim(c(0,100)) +
            ggtitle("2020 - First Touchdowns by Position") +
            scale_fill_brewer(palette = 'Paired') +
            theme(legend.position = 'none')
    })
    
    output$yearSplit <- renderPlot({
        redzone20_21 %>% 
            ggplot(aes(rz_pass_percent_20, rz_pass_percent_21)) + 
            geom_image(aes(rz_pass_percent_20, rz_pass_percent_21, image = team_logo_espn), asp=16/9, size=0.04) +
            geom_abline(slope=1, intercept = 0, lty='dotted') +
            xlab("Redzone Pass Percentage in 2020") +
            ylab("Redzone Pass Percentage in 2021") +
            xlim(c(0.3,0.7)) +
            ylim(c(0.3,0.7)) +
            ggtitle("Redzone Pass Percentage 2020 v 2021") +
            theme_minimal()
    })
    
}

# Return a Shiny app object
shinyApp(ui = ui, server = server)