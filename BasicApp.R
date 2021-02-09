library(shiny)
library(tidyverse)

ui <- fluidPage(
  #I can place lots of *Input() here in my UI
  sliderInput(inputId = "slider1",
              label = "Slide this slider",
              min = 0,
              max = 100,
              value = 50),
  textInput(inputId = "text1",
            label = "Title your graph here!"),
  actionButton(inputId = "button1",
               label = "Click me!"),
  plotOutput(outputId = "plot1")
)

server <- function(input, output, session) {
  
  #have the slider change based on button click
  diamonds.data <- reactive(diamonds %>%
    sample_n(input$slider1))
  
  
  output$plot1 <- renderPlot({
    #inisde renderPLot, get the input from button1
    input$button1
    #then go on and make the plot
    diamonds.data() %>%
      ggplot(aes(x = carat,
                 y = price)) +
      geom_point() +
      ggtitle(isolate(input$text1))
  })
}

shinyApp(ui, server)