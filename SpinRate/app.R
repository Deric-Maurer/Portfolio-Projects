library(tidyverse)
library(shiny)
library(Lahman)
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


# For testing
waino = filtered_df %>% 
  filter(player_name == "Adam Wainwright" | player_name == "Corey Kluber", year.x == 2022) 
klube = filtered_df %>% 
  filter(player_name == "Corey Kluber", year.x == 2022)
cards = filtered_df %>% 
  filter(name == "St. Louis Cardinals")

# Define UI for application that draws a histogram
ui <- fluidPage(
  fluidRow(column(width = 3, selectInput("pitcher1", "First Pitcher (Red)", sort(unique(filtered_df$player_name)))),
           column(width = 3, selectInput("pitcher2", "Second Pitcher (Blue)", c("NONE", sort(unique(filtered_df$player_name))))),
           column(width = 3, selectInput("yearSelect", " Year Selection", sort(unique(filtered_df$year.x), decreasing = TRUE))),
           column(width = 2, selectInput("teamSelect", "Team Averages and Deviations", sort(unique(filtered_df$name))))),
  fluidRow(column(width = 6, plotOutput('pitchSpeed1')), column(width = 6, offset = 0, plotOutput('pitchSpeed2'))),
  fluidRow(column(width = 6, plotOutput("pitchSpinrate1")), column(width = 6, offset = 0, plotOutput('pitchSpinrate2')))
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  #Reactive Elements
  
  # Year Reactive
  filtered_df_reactive = reactive({
    filtered_df %>% 
      filter(year.x == input$yearSelect)
  })
    
  observeEvent(
    eventExpr = input$yearSelect,
    handlerExpr = {
      updateSelectInput(inputId = "pitcher1", choices = sort(unique(filtered_df_reactive()$player_name)))
    }
  )
  
  observeEvent(
    eventExpr = input$yearSelect,
    handlerExpr = {
      updateSelectInput(inputId = "pitcher2", choices = c("NONE", sort(unique(filtered_df_reactive()$player_name))))
    }
  )
  
  observeEvent(
    eventExpr = input$yearSelect,
    handlerExpr = {
      updateSelectInput(inputId = "teamSelect", choices = sort(unique(filtered_df_reactive()$name)))
    }
  )
  #Pitcher and Team Reactive
  pitch1_reactive = reactive({
    filtered_df_reactive() %>% 
      filter(player_name == input$pitcher1)
  })
  
  pitch2_reactive = reactive({
    filtered_df_reactive() %>% 
      filter(player_name == input$pitcher2)
  })
  
  team_reactive = reactive({
    filtered_df_reactive() %>% 
      filter(name == input$teamSelect)
  })
  
   
  #Row 1
  
  #Speed Comparison Between Players
  output$pitchSpeed1 = renderPlot({ggplot(data = NULL, aes(x=pitchType, y=speed)) + scale_y_continuous(limits = c(60, 110)) + geom_point(data = pitch1_reactive(), color = "red2", size = 3) + geom_point(data=pitch2_reactive(), color = "blue2", size = 3)})
  
  #Team Speed Comparison
  output$pitchSpeed2 = renderPlot({ggplot(data = team_reactive(), aes(x=pitchType, y=speed)) + scale_y_continuous(limits = c(60, 110)) + geom_boxplot(outlier.colour = "green", fill = "red")})
  
  #Row 2
  
  #Spin Rate Between Players
  output$pitchSpinrate1 = renderPlot({ggplot(data = NULL, aes(x=pitchType, y=spin)) + scale_y_continuous(limits = c(1500, 3000)) + geom_point(data = pitch1_reactive(), color = "red2", size = 3) + geom_point(data=pitch2_reactive(), color = "blue2", size = 3)})
  
  #Team Spin Comparisons
  output$pitchSpinrate2 = renderPlot({ggplot(data = team_reactive(), aes(x=pitchType, y=spin)) + scale_y_continuous(limits = c(1500, 3000)) + geom_boxplot(outlier.colour = "green", fill = "blue")})
}

# Run the application 
shinyApp(ui = ui, server = server)
