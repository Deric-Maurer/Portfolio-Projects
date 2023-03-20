library(tidyverse)
library(shiny)
library(Lahman)
library(shinyWidgets)
# Importing Data
mlb_pitches_21_22 = as_tibble(read_csv("./pitchdata.csv"))
# gives names to player_id
player_data = as_tibble(read.csv("./player.csv")) %>% 
  filter(is.na(death_year))
#Team and Team_id
team_data = as_tibble(read_csv("./team.csv")) %>% 
  filter(year == 2015)
#player_id to team id
pitcher_tag_data = as_tibble(read.csv("./pitching.csv")) 

#Merging Teams to Pitchers
merged = mlb_pitches_21_22 %>% 
  inner_join(player_data, by = c('last_name'='name_last', 'first_name'='name_first')) %>%
  inner_join(pitcher_tag_data, by = c('player_id.y'='player_id')) %>% 
  inner_join(team_data, by = c('team_id'='team_id')) %>% 
  select(1:24, 48:49, 77, 114) %>% 
  unique() %>% 
  drop_na(pitch_hand)



# Sorting Data
filtered_df = merged %>% 
  mutate(player_name = paste(first_name, last_name, sep = ' ')) %>% 
  pivot_longer(cols = 'ff_avg_speed': 'fastball_avg_spin', names_to = c('pitchType', 'category'), names_sep = '_avg_', values_to = "value") %>% 
  drop_na(c(pitch_hand, value)) %>% 
  pivot_wider(names_from = 'category', values_from = 'value') %>% 
  select(player_name, name, year.x, pitch_hand, pitchType, spin, speed, team_id, franchise_id)
filtered_df$name[filtered_df$year.x == 2022 & filtered_df$franchise_id == "CLE"] <- "Cleveland Guardians"

# Define UI for application that draws a histogram
ui <- fluidPage(
  fluidRow(column(width = 3, selectInput("pitcher1", "First Pitcher", sort(unique(filtered_df$player_name)))),
           column(width = 3, selectInput("pitcher2", "Second Pitcher", sort(unique(filtered_df$player_name)))),
           column(width = 3, awesomeCheckboxGroup("yearSelect", " Year Selections", sort(unique(filtered_df$year.x), decreasing = TRUE), inline = TRUE)),
           column(width = 2, selectInput("teamSelect", "Sort Pitchers by Team", c('All Teams', sort(unique(filtered_df$name)))))),
  fluidRow(column(width = 5, plotOutput('pitchSpeed')), column(width = 5, offset = 2, plotOutput('pitchSpinrate'))),
  fluidRow(column(width = 5, plotOutput("speedDif")), column(width = 5, offset = 2, plotOutput('spinDif')))
)

# Define server logic required to draw a histogram
server <- function(input, output) {

   
}

# Run the application 
shinyApp(ui = ui, server = server)
