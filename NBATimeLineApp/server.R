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

function(input, output) {
  output$description <- renderUI({
    HTML("<b>This app displays the average NBA player statistics for the selected player and season type.</b>
    
     <div style='margin-top: 10px'>The data is based on the NBA player statistics from the regular season and playoffs. Choose a player of interest and a statistic you are interested in testing to see the player's performance per season over their career. Enjoy!</div>")
  })
  output$player_graph <- renderPlot({
    if(input$season_type == "Playoffs") {
      player_data <- Playoffs_df %>% 
        filter(Player == input$player)
    }else {
      player_data <- Regular_season_df %>%
        filter(Player == input$player)
    }
    
    
    
    ggplot(player_data, aes(x = Season, y = !!sym(input$stat)))+theme(plot.title = element_text(hjust = 0.5)) + 
      geom_line(color = "blue") + 
      geom_point() + 
      labs(x = "Season", y = input$stat, title = "Average NBA Player Statistics", caption = "Individual Player Statistics over Time") + 
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5), panel.border = element_rect(fill = NA, color = "red", size = 2)) + 
      scale_y_continuous(limits = c(0, max(player_data[, input$stat])))
  })
}