library(tidyverse)
library(stringr)
library(data.table)
library(ggplot2)
library(leaflet)
library(rvest)
library(geojsonio)
library(ggrepel)
library(shiny)
library(rsconnect)

president.data <- read.csv("~/1976-2016-president.csv")

president.data <- president.data %>%
  mutate(percent.vote = (candidatevotes/totalvotes)*100) %>%
  filter(party == "democrat" | party == "republican" | party == "independent" | party == "libertarian") ##we filter by candidates that recieve over 1% of total votes

president.data.clean <- president.data[ ,-c(4,5,6,10,13,14)] #Gets only the essential columns


states.url <- "http://eric.clst.org/assets/wiki/uploads/Stuff/gz_2010_us_040_00_5m.json"

states <- geojson_read(states.url, what = "sp")
View(states@data)

map <- states %>%
  leaflet() %>%
  addTiles() %>%
  setView(-96, 37.8, 4)

bins <- c(0, 1000000, 3000000, 5000000, 7000000, 10000000, 15000000, Inf) 
colors <- colorBin(palette = "YlOrRd",
                   domain = c(0, 100)) #WHAT WE COLOR BASED ON


# Define UI for application that draws the map
ui <- fluidPage(
  sliderInput(inputId = "slider1",
              label = "Slide this slider",
              min = 1976,
              max = 2016,
              value = 2000,
              step = 4,
              sep = ""),
  
  leafletOutput(outputId = "map1"),
  leafletOutput(outputId = "party"),
  textOutput(outputId = "text1"),
  plotOutput(outputId = "plot1")
  
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  current.year <- reactive(president.data.clean %>%
                             filter(year == input$slider1))
  
  
  output$map1 <- renderLeaflet({
    map %>%
      addPolygons(fillColor = ~colors(current.year()$percent.vote),#plugs percent.vote into color function
                  weight = 2,
                  opacity = 1,
                  color = "white",
                  dashArray = "3",
                  fillOpacity = 0.7,
                  layerId = states@data$NAME,
                  popup = paste("State", input$map1_shape_click$id, "<br>",
                      "Party", input$party_shape_click$id, "<br>",
                      "Name", president.data.clean$candidate, "<br>",
                      "Percent Votes", president.data.clean$percent.vote, "<br>")
        )
  })
  
  colors <- c("red","blue","yellow","green")
  names(colors) = c("republican", "democrat", "independent", "libertarian")
  
  output$plot1 <- renderPlot({
    if(!is.null(input$map1_shape_click$id)) {
      president.data.clean %>%
        mutate(is.clicked.state = ifelse(state == input$map1_shape_click$id,
                                         "yes",
                                         "no")) %>%
        filter(is.clicked.state == "yes") %>%
        ggplot(aes(x = factor(year),#, -percent.vote)),
                   y = percent.vote)) +
        geom_bar(aes(fill = party),
                 stat = "identity",
                 position = position_dodge2(width = 0.9, preserve = "single")) +
        scale_fill_manual(values=c("democrat" = "blue", 
                                   "republican" = "red",
                                   "independent" = "yellow", 
                                   "libertarian" = "green")) +
        labs(title = is.clicked.state, xlab = "Year", ylab = "Percent Vote")
    }
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)