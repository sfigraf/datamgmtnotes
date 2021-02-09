text1 <- "Charmander"
url <- "https://pokemon.fandom.com/wiki/"+text1
url <- str_c("https://pokemon.fandom.com/wiki/", text1)
url %>%
  read_html() %>%
  html_nodes(xpath = '//*[@id="mw-content-text"]/p[1]') %>%
  html_text()
install.packages("Rfast")
library(Rfast)

pokemon_sheet <- read.csv("~/Pokemon.csv")

percentileattack = ecdf(min(pokemon_sheet$Attack):max(pokemon_sheet$Attack))
percentilehp = ecdf(min(pokemon_sheet$HP):max(pokemon_sheet$HP))
percentiledefense = ecdf(min(pokemon_sheet$Defense):max(pokemon_sheet$Defense))
percentileSpatt = ecdf(min(pokemon_sheet$Sp..Atk):max(pokemon_sheet$Sp..Atk))
percentileaSp.def = ecdf(min(pokemon_sheet$Sp..Def):max(pokemon_sheet$Sp..Def))
percentileSpeed = ecdf(min(pokemon_sheet$Speed):max(pokemon_sheet$Speed))

?as.numeric
?ecdf
ecdf(min(pokemon_sheet$HP):max(pokemon_sheet$HP))
View(pokemon_sheet)
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

  

pokemon_name <- "SteelixMega Steelix"
row <- which(grepl(pokemon_name, new_cols$Name)) #searches name row and returns the row position where it's found
x <- new_cols[row,] #makes data frame with just that pokemon's stats
#gathers data to one row
x.gathered <- x %>%
  gather(key ="Stat",
       value = "percentile1",
       -Name)
#finds max value stat and returns all information of the row that it's in by subsetting
max.stat <- x.gathered[which.max(x.gathered$percentile1),]
#gets name of the stat that is the highest percentile
name.of.stat <- max.stat[,"Stat"]
#need to make new dataframe only with that stat. make it from new cols, not new cols gathered
colnames <- c("Name", name.of.stat)
#creates new dataset with only the specific columns
new.frame <- new_cols[colnames]
row2 <- which(grepl(pokemon_name, new.frame$Name)) #searches name row and returns the row position where it's found


#makes a plot of percentile 
ggplot(new.frame,
       aes(x = reorder(Name, new.frame[,2]), y = new.frame[,2])) + 
  geom_bar(stat = "identity", aes(fill = ifelse(Name == pokemon_name,
                                                "gray",
                                                "tomato"))) +
  ylab(name.of.stat) +
  xlab(NULL) +
  theme(legend.position="none")


#want to make it so that if the max stat is a certain name, then display this graph, but if it's a different stat, then display this graph




  

##Question 3
states.url <- "http://eric.clst.org/assets/wiki/uploads/Stuff/gz_2010_us_040_00_5m.json"

states <- geojson_read(states.url, what = "sp")
View(states@data)

#getting the data
map <- states %>%
  leaflet() %>%
  addTiles() %>%
  setView(-96, 37.8, 4)
#assigning the pokemon a random number
#make this random rumber it's own dataframe then addd to pokemon sheet
#each numebr corresponds to a state
length(pokemon_sheet)
?runif
?sample
pokemon_sheet.Rand <- pokemon_sheet %>%
  mutate(RandState = sample.int(51, nrow(pokemon_sheet), replace = TRUE))

states.clean <- states@data[-c(8),]
states.clean <- states.clean %>%
  mutate(RandState = sample.int(51, nrow(states.clean), replace = FALSE))

states.clean1 <- left_join(states.clean,
                         pokemon_sheet.Rand,
                         by = "RandState")
counts <- states.clean1 %>%
  group_by(NAME) %>%
  tally()
poke_state <- data.frame(states.clean1[states.clean1$Name == pokemon_name,])
poke_state1 <- poke_state$NAME[1]
poke_state1 == "North Carolina"
View(counts)
pokemon_name <- "Charizard"
poke_row <- which(grepl(states.clean1$Name == pokemon_name, states.clean1$Name)) #searches name row and returns the row position where it's found

x <- states.clean1[states.clean1$Name == pokemon_name,]
y <- states.clean1[n(states.clean1$Name),]
x$NAME
x$NAME==states.clean1$NAME
new.vector <- NULL
poke_names <- c(to_string(states.clean1$NAME))
poke_names
state = "Colorado"

for(i in 1:nrow(states.clean1)) {
  new.vector[i] <- c(states.clean1$Name)
}
View(new.vector)
for(i in 0:) {
  count.vector[i+1] <- sum(str_count(reddit$author,
                                     toString(i)))
  print(i)
} 



colors <-colorFactor(palette = "YlOrRd", #whats the gradient I want
                     domain = counts$n #this is the thing I'm basing my color gradient off of
) 

map %>%
  addPolygons(fillColor = ~colors(counts$n),
  weight = 2,
  opacity = 1,
  color = "white",
  dashArray = "3",
  fillOpacity = 0.7,
  layerId = states@data$NAME,
  popup = paste(states.clean1 %>%
                  filter(NAME == input$map_shape_click$id) %>%
                  select(Name)),
  highlight = 
    highlightOptions(
      weight = ifelse(states.clean1$Name == "Colorado",
                      5,
                      1
                      ),
      stroke = TRUE,
      color = "#666",
      dashArray = "",
      fillOpacity = 0.7,
      bringToFront = TRUE)
  
    
  )

View(states@polygons)
?highlightOptions
??highlight
