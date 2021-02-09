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

#read in pokemon data
pokemon_sheet <- read.csv("~/Pokemon.csv")
#creates functions 'percentile' that gets the percentile of a column
percentileattack = ecdf(min(pokemon_sheet$Attack):max(pokemon_sheet$Attack))
percentilehp = ecdf(min(pokemon_sheet$HP):max(pokemon_sheet$HP))
percentiledefense = ecdf(min(pokemon_sheet$Defense):max(pokemon_sheet$Defense))
percentileSpatt = ecdf(min(pokemon_sheet$Sp..Atk):max(pokemon_sheet$Sp..Atk))
percentileaSp.def = ecdf(min(pokemon_sheet$Sp..Def):max(pokemon_sheet$Sp..Def))
percentileSpeed = ecdf(min(pokemon_sheet$Speed):max(pokemon_sheet$Speed))
#create new columns that have percentages of stats for the pokemons
new_cols <- pokemon_sheet %>%
  mutate(HP.percentile = percentilehp(HP),
         Attack.percentile = percentileattack(Attack),
         Defense.percentile = percentiledefense(Defense),
         Sp.Attackpercentile = percentileSpatt(Sp..Atk),
         Sp.Defensepercentile = percentileaSp.def(Sp..Def),
         Speed.percentile = percentileSpeed(Speed)) %>%
  select(Name,
         HP.percentile,
         Attack.percentile,
         Defense.percentile,
         Sp.Attackpercentile,
         Sp.Defensepercentile,
         Speed.percentile) 
# Define UI for application 
ui <- fluidPage(
  titlePanel("Pokemon"),
  textInput("text1", "What Pokemon Do you want to learn about?", "Enter pokemon here"),
  actionButton("Search", "Click here"),
  textOutput("text"),
  plotOutput(outputId = "plot1")
    )
   
   

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  
  pokemon_text <- eventReactive(input$Search, {
    
    str_c("https://pokemon.fandom.com/wiki/", input$text1) %>%
    read_html() %>%
    html_nodes(xpath = '//*[@id="mw-content-text"]/p[1]') %>%
    html_text() 
    
  })
  
  
  output$text <- renderText({
    pokemon_text()
    
    
  })
  
    
  
  output$plot1 <- renderPlot({
    input$Search
    row <- which(grepl(input$text1, new_cols$Name)) #searches name row and returns the row position where it's found
    x <- new_cols[row,] #makes data frame with just that pokemon's stats
    #gathers data to one row
    x.gathered <- x %>%
      gather(key ="Stat",
             value = "percentile1",
             -Name)
    #finds max value stat and returns all information of the row that it's in
    max.stat <- x.gathered[which.max(x.gathered$percentile1),]
    #gets name of the stat that is the highest percentile
    name.of.stat <- max.stat[,"Stat"]
    #need to make new dataframe only with that stat. make it from new cols, not new cols gathered
    colnames <- c("Name", name.of.stat)
    #creates new dataset with only the specific columns
    new.frame <- new_cols[colnames]
    row2 <- which(grepl(input$text1, new.frame$Name)) #searches name row and returns the row position where it's found
    
    #makes a plot of percentile 
    ggplot(new.frame,
           aes(x = reorder(Name, new.frame[,2]), y = new.frame[,2])) + 
      geom_bar(stat = "identity", aes(fill = ifelse(Name == input$text1,
                                                    "blue",
                                                    "gray"))) +
      ylab(name.of.stat) +
      xlab(NULL) +
      theme(legend.position="none"
            ) +
      ggtitle(isolate(input$text1))
  })
}    
    

# Run the application 
shinyApp(ui = ui, server = server)