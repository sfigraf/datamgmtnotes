#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(nycflights13)

# Define UI for application that draws a histogram
ui <- fluidPage(
  selectInput(inputId = "choices", 
              label = "Select a carrier!",
              choices = unique(flights$carrier)), #uniquetakes only unique values for our choices
   plotOutput(outputId = "plot1")
  )

# Define server logic required to draw a histogram
server <- function(input, output) {
  output$plot1 <- renderPlot({
    flights %>%
      sample_n(1000) %>%
      #filter(carrier == input$choices) %>% If you want to only graph what the user selects 
      mutate(is.user.carrier = ifelse(carrier == input$choices,
                                      input$choices,
                                      paste(c("Not", input$choices)))) %>%
      ggplot(aes(x = dep_time,
                 y = arr_time)) +
      geom_point(aes(color = input$choices))
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

