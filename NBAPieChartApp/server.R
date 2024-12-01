library(tidyverse)
library(shiny)
library(plotly)

NBA <- read.csv("DS2003_NBA_Data.csv")

NBA$Season <- ifelse(month(NBA$GameDay) %in% c(8, 9, 10, 11, 12, 1, 2, 3, 4, 5, 6), 
                     year(NBA$GameDay), 
                     year(NBA$GameDay) - 1) 

function(input, output, session) {
  output$description <- renderUI({
    HTML("<b>This app displays each player's attempted or made 3 pointers vs attempted or made 2 pointers by a full season that they were in the NBA.</b><div style='margin-top: 10px'>The data is based on the NBA player statistics from the regular season. Choose a player of interest and a type of shot (made, attempted) you are interested in, and see the player's shooting trends over time. Enjoy!</div>")
  })
  
  # Reactive expression to filter data based on the selected player
  filtered_data <- reactive({
    req(input$player)  # Ensure input$player is not NULL
    NBA %>% filter(Player == input$player)
  })
  
  # Observe changes in `player` input and update sliderInput
  observe({
    player_data <- filtered_data()
    req(nrow(player_data) > 0)  # Ensure there is data for the selected player
    
    min_year <- min(player_data$Season, na.rm = TRUE)
    max_year <- max(player_data$Season, na.rm = TRUE)
    
    updateSliderInput(session, "year",
                      min = min_year,
                      max = max_year,
                      value = min_year)  # Default to the earliest season
  })
  
  # Plot rendering
  output$player_graph <- plotly::renderPlotly({
    req(input$player, input$year)  # Ensure inputs are not NULL
    
    if (input$attempted_or_made == "Made") {
      player_data <- NBA %>%
        filter(Player == input$player, Season == input$year) %>%
        group_by(Player, Season) %>%
        summarize(
          Total_3FM = sum(`X3FM`, na.rm = TRUE),  # Total 3-point FGM
          Total_2FM = sum(FGM, na.rm = TRUE) - sum(`X3FM`, na.rm = TRUE),  # Total 2-point FGM
          .groups = "drop"
        ) %>%
        pivot_longer(cols = c(Total_3FM, Total_2FM), names_to = "Type", values_to = "Value") %>%
        mutate(
          TypeLabel = ifelse(Type == "Total_3FM", "3-Point Field Goals Made", "2-Point Field Goals Made")  # Custom labels
        )
    } else {
      player_data <- NBA %>%
        filter(Player == input$player, Season == input$year) %>%
        group_by(Player, Season) %>%
        summarize(
          Total_3FA = sum(`X3FA`, na.rm = TRUE), 
          Total_2FA = sum(FGA, na.rm = TRUE) - sum(`X3FA`, na.rm = TRUE), 
          .groups = "drop"
        ) %>%
        pivot_longer(cols = c(Total_3FA, Total_2FA), names_to = "Type", values_to = "Value") %>%
        mutate(
          TypeLabel = ifelse(Type == "Total_3FA", "3-Point Field Goals Attempted", "2-Point Field Goals Attempted")  # Custom labels
        )
    }
    
    req(nrow(player_data) > 0)  # Ensure there's data to plot
    
    # Dynamic plot title
    plot_title <- paste(input$player, ifelse(input$attempted_or_made == "Made", ": 3FM vs 2FM (", ": 3FA vs 2FA ("), input$year, " Season)", sep = "")
    
    # Create interactive pie chart using plotly
    plotly::plot_ly(
      player_data, 
      labels = ~TypeLabel,  # Use the new custom labels column
      values = ~Value, 
      type = 'pie', 
      textinfo = 'percent',  # Show percentage on hover
      hoverinfo = 'text',    # Customize hover text
      text = ~paste(TypeLabel, ": ", Value),  # Define custom text to show on hover
      marker = list(colors = c("skyblue", "orange"))
    ) %>%
      plotly::layout(
        title = list(
          text = plot_title,       # Set the dynamic title text
          x = 0.5,                 # Center the title horizontally
          xanchor = "center",      # Anchor title in the center
          y = 0.95,                # Position the title towards the top
          yanchor = "top",         # Anchor title at the top
          pad = list(t = 20)       # Add padding to the top of the title (20 px)
        ),
        margin = list(
          t = 80,  # Top margin, gives space for the title
          b = 50,  # Bottom margin
          l = 50,  # Left margin
          r = 50   # Right margin
        )
      )
  })
  
}