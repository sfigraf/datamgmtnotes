#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   textOutput(outputId = "text1")
      )

# Define server logic required to draw a histogram
server <- function(input, output) {
   
  sb.url <- ("https://en.wikipedia.org/wiki/Super_Bowl_XLII")
  sb.text <- sb.url %>%
    read_html() %>%
    html_nodes("p") %>%
    html_text() #clears out html code and converts it to text
  
  random.number <- sample(1:length(sb.text), #length sb.text is the number of paragraphs inside 
                          size = 1)
  
  output$text1 = sb.text[random.number]
  
}

# Run the application 
shinyApp(ui = ui, server = server)

