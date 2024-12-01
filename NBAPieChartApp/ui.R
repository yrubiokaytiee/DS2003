library(tidyverse)
library(shiny)
library(shinythemes)

NBA <- read.csv("DS2003_NBA_Data.csv")

NBA$Season <- ifelse(month(NBA$GameDay) %in% c(8, 9, 10, 11, 12, 1, 2, 3, 4, 5, 6), 
                     year(NBA$GameDay), 
                     year(NBA$GameDay) - 1) 

fluidPage(
  theme = shinytheme("superhero"),
  titlePanel(div(style = "text-align: center;", "NBA Player Shot Selection Over Time")),
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId = "player", 
                  label = "Select Player:", 
                  choices = unique(NBA$Player)),
      sliderInput(inputId = "year", 
                  label = "Select Year:", 
                  min = 2000,  # Placeholder min; will be updated dynamically
                  max = 2023,  # Placeholder max; will be updated dynamically
                  value = 2000),  # Placeholder value; will be updated dynamically
      radioButtons(inputId = "attempted_or_made", 
                   label = "Select Statistic Type:", 
                   choices = c("Made", "Attempted")),
      uiOutput("description")
    ),
    
    mainPanel(
      plotly::plotlyOutput("player_graph")
    )
  )
)