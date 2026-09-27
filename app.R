#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(tidyverse)

spotify <- read.csv("SpotifyFeatures.csv")

spotify_clean <- spotify |>
  select(track_name, artist_name, genre, popularity,
         danceability, energy, valence, acousticness,
         instrumentalness, loudness, tempo, duration_ms) |>
  filter(popularity > 0) |>
  mutate(duration_min = duration_ms / 60000)

ui <- fluidPage(
  titlePanel("Exploring Spotify Song Popularity"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("genre_choice", "Choose a genre:",
                  choices = sort(unique(spotify_clean$genre))),
      
      selectInput("feature_choice", "Choose a song feature:",
                  choices = c("danceability", "energy", "valence",
                              "acousticness", "instrumentalness",
                              "loudness", "tempo", "duration_min")),
      
      sliderInput("length_choice", "Maximum song length in minutes:",
                  min = 1, max = 10, value = 10)
    ),
    
    mainPanel(
      h3("Feature vs Popularity"),
      plotOutput("scatter_plot"),
      
      h3("Correlation"),
      textOutput("correlation_text"),
      
      h3("Top Songs Based on Your Filters"),
      tableOutput("song_table")
    )
  )
)

server <- function(input, output) {
  
  filtered_data <- reactive({
    spotify_clean |>
      filter(
        genre == input$genre_choice,
        duration_min <= input$length_choice
      )
  })
  
  output$scatter_plot <- renderPlot({
    ggplot(filtered_data(), aes_string(x = input$feature_choice, y = "popularity")) +
      geom_point(color = "steelblue", alpha = 0.3) +
      geom_smooth(method = "lm", se = FALSE) +
      labs(
        x = input$feature_choice,
        y = "Popularity",
        title = "Relationship Between Song Feature and Popularity"
      )
  })
  
  output$correlation_text <- renderText({
    cor_value <- cor(
      filtered_data()[[input$feature_choice]],
      filtered_data()$popularity
    )
    
    
    paste("The correlation between", input$feature_choice,
          "and popularity is", round(cor_value, 3))
          
  })
    
  
  output$song_table <- renderTable({
    filtered_data() |>
      select(track_name, artist_name, genre, popularity, duration_min) |>
      arrange(desc(popularity)) |>
      head(10)
  })
}

shinyApp(ui = ui, server = server)