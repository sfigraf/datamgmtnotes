library(shiny)

ui <- fluidPage(
  #I can place lots of *input() here in my UI, which is how the user inputs something
  #inputId to use the info the user gives us
  sliderInput(inputId = "slider1", #names the input value so we can store is, so slider1 is the name of this
              label = "slide this slider", #what the user sees
              min = 0,
              max = 100,
              value = 50),
  textInput(inputId = "text1",
            label = "title your graph here!",
            value = "Please enter title."),
  actionButton(inputId = "button1", 
               label = "Click me!"),
  plotOutput(outputId = "plot1") #taken from server
  
)
#ui is the display for the user

server <- function(input, output, session) {
  diamonds.data <- reactive(diamonds %>%
    sample_n(input$slider1)) #wantdata to be created outside; reactive() makes reactive data frames (reacts to inputs changing), and changes thisdiamonds.data to a function (so need paranthesis)
  output$plot1  <- renderPlot({
    #observeevent(input$button1, {
    #}
     #alternate method; don't need isolate, and will also then need to put 
    
     #if we put action button here, code will only rerun when we click the button
    input$button1
    
     #slider changes, diamonds data changes.     #input is replaced by whatever is put in using the slider; sample_n() randomly samples rows in the dataset, so sliding the slider selects x number of rows to randomly sample

    diamonds.data() %>% #diamonds.data is a function
      ggplot(aes(x = carat,
                 y = price)) +
      geom_point() + #saying make this plot, render it, then store it aas plot1 and run it in the ui
      ggtitle(isolate(input$text1)) #isolate function says that when input changes, please ignore this part when it chagned. Don't run all code
    }) 
}
#server does heavy lifting: functions, etc
shinyApp(ui, server)