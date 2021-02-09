library(tidyverse)
library(stringr)
library(data.table)
library(ggplot2)
library(leaflet)
library(rvest)
library(geojsonio)
library(ggrepel)
library(rsconnect)
library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(
  titlePanel("Pokemon"),
  
  sidebarLayout(
    sidebarPanel(
      
      textInput("text1", "What Pokemon Do you want to learn about?", "Enter pokemon here"), 
      actionButton("button1", "Click Me")
    ),
    
    mainPanel(
      
      h3(textOutput("text1", container = span))
    )
  )
)
   
   

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  ###getting a  short description of pokemon
  url <- str_c("https://pokemon.fandom.com/wiki/", text1)
  
  pokemon_text <- url %>%
    read_html() %>%
    html_nodes(xpath = '//*[@id="mw-content-text"]/p[1]') %>%
    html_text() 
  
  text <- eventReactive(input$button1, {
    input$text1
  })
  output$text1 <- renderText({
    url <- str_c("https://pokemon.fandom.com/wiki/", text1)
    pokemon_text <- url %>%
      read_html() %>%
      html_nodes(xpath = '//*[@id="mw-content-text"]/p[1]') %>%
      html_text() 
  })
   
}

# Run the application 
shinyApp(ui = ui, server = server)

