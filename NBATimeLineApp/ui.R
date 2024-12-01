library(tidyverse)
library(shiny)

NBA<-read.csv("DS2003_NBA_Data.csv")

NBA$Season <- ifelse(month(NBA$GameDay) %in% c(8, 9, 10, 11, 12, 1, 2, 3, 4, 5, 6), 
                     year(NBA$GameDay), 
                     year(NBA$GameDay) - 1) 
#this part calculates the previous and following year to make sure the graph stays in numerical order, I found from a quick google search that the NBA season is from October to April of the following year.

Playoffs <- filter(NBA, Playoffs == "Y")
Reg_season <- filter(NBA, Playoffs == "0")

Regular_season_df <- Reg_season %>%
  group_by(Player, Season) %>%
  summarise(
    Avg_PTS = mean(PTS),
    Avg_AST = mean(AST),
    Avg_REB = mean(REB),
    Avg_STL = mean(STL),
    Avg_BLK = mean(BLK)
  )

Playoffs_df <- Playoffs %>%
  group_by(Player, Season) %>%
  summarise(
    Avg_PTS = mean(PTS),
    Avg_AST = mean(AST),
    Avg_REB = mean(REB),
    Avg_STL = mean(STL),
    Avg_BLK = mean(BLK)
  )

library(shiny)
library(shinythemes)
library(ggplot2)

fluidPage(
  theme = shinytheme("superhero"),
  titlePanel(div(style = "text-align: center;", 
                 "NBA Player Average Statistics")),
  sidebarLayout(
    sidebarPanel(
      selectizeInput(inputId = "player", 
                     label = "Select Player:", 
                     choices = unique(NBA$Player),
                     multiple = FALSE
      ),
      selectInput(inputId = "stat", 
                  label = "Select Statistic:", 
                  choices = c("Avg_PTS", "Avg_BLK", "Avg_AST", "Avg_REB", "Avg_STL")),
      radioButtons(inputId = "season_type", 
                   label = "Select Season Type:", 
                   choices = c("Regular Season", "Playoffs")),
      uiOutput("description")
      
    ),
    
    mainPanel(
      plotOutput("player_graph")
    )
  )
)