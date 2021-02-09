#store things, same things
x = 2 + 2
x <- 2 + 2

#learn how to use functions
#c function
#stands for concatenate (put all these things together in a vector basically)
#arguments are put in the parentheses, things i want to put in a vector

vector1 = c(1,2,9,15,1000)
vector1 <- 
#mean function takes mean
#trim takes off end values, 0-.5
     trim = .25)

#bring up help documentation
?mean

#installing new packages
#install.packages('tidyverse') 
#tidyverse is a lot of fucntions so that database mangement that is normally done in sql can be done in R
library(tidyverse) #loads all functions of tidyverse into R

#explore the diamonds dataset
#View function helps bring up dataset in more userfriendly way
View(diamonds)
?diamonds

### tidyverse "verbs"
#all these functions come from tidyverse, all are things done to a dataset
#filter() function: subsets data based on logical or matheamatical expression
?filter
filter(.data = diamonds) #not filtered
expensive.diamonds = filter(.data = diamonds,
       price > 12000) #only diamonds higher than 12000 in price

expensive.pretty.diamonds = filter(.data = expensive.diamonds,
       color == "D" | color == "E") #double equals signifies that I'm checking that color is equal to D (the best color)
#shift+backslash can add another color to that; like "or"
#technically don't even need .data = part if you put arguments in the right spot

#slightly more efficient way
filter(.data = diamonds,
       price > 12000,
       color == "D" | color == "E") #need parentheses 

#pipe operator %>%
#take the thing before and make it the first argument of the thing right after that
mean(vector1)

vector1 %>% 
  mean()
       
#most efficient way
#filtered in steps
ex1 = diamonds %>%
  filter(price > 12000) %>%
  filter(color == "D" | color == "E")


#select function: selects particular columns of interest
diamonds %>%
  select(carat,price)

diamonds %>%
  select(-price) #sees all cilumns except price

#arrange() function: sorts a data set
diamonds %>%
  arrange(carat) #says sort by carat in ascending order

#arrange in descending order
diamonds %>%
  arrange(-carat)

#class notes Feb 18
library(tidyverse)

View(diamonds)

diamonds %>%
  summarize(mean.price = mean(price)) #mean price of all the diamonds in the dataset; labels column on the new dataset

diamonds %>%
  summarize(max.price = max(price)) #max price

#groupby divides the data into groups based on categorical variable (ex: color
diamonds %>%
  group_by(color) %>%
  summarize(mean.price = mean(price)) #mean price of all the diamonds in the dataset; labels column on the new dataset

#mean price of diamonds by color and cut
diamonds %>%
  group_by(color,cut) %>%
  summarize(mean.price = mean(price),
            max.price = max(price),
            count = n()) #count gives number of diamonds in each 

#install.packages('nycflights13') #all flights that left airports in 2013
library(nycflights13) 
flights

?flights
#1: What is the average depareturre delay for flights into burlington airport vs logan airport?
View(flights)
flights %>%
  filter(dep_delay > 0) %>% #finding flights that were only delayed, not that weren't
  filter(dest == "BOS" | dest == "BTV") %>%
  group_by(dest) %>%
  summarize(mean.dep_delay = mean(dep_delay, na.rm = TRUE)) #SHOULD na VALUES BE REMOVED?





#2: What porportion of flights are delayed heading into Burlington airport vs Logan airport? (hint: ifelse())
?ifelse

flights %>%
  filter(dest == "BOS" | dest == "BTV") %>%
  mutate(is.delayed = ifelse(dep_delay >0, 1,0)) %>%
  group_by(dest) %>%
  summarize(prop.delayed = mean(is.delayed,
                                na.rm = TRUE)/n())

#anotherway
flights %>%
  filter(dest == "BOS" | dest == "BTV") %>%
  group_by(dest) %>%
  mutate(total_dest_flights = ifelse(delayyes >= 0, 1, 0)) %>% #removes flights that are NA
  summarize(sum.total_dest_flights = sum(total_dest_flights, na.rm = TRUE), #gets sums of those columns
            sum.delayyes = sum(delayyes, na.rm = TRUE))

delayed$new <- delayed$sum.delayyes / delayed$sum.total_dest_flights


?sum


View(delayed) 


#class notes feb 20
#homework due next wednesday

#make sgrapohs in R using ggplot2()
library(ggplot2)
#lets make scatterpolot of carat of diamonds vs price
diamonds %>%
  ggplot(mapping = aes(x = carat,
                       y = price)) + #aes changes asthetics of the graph
  geom_point() #scatterplot

#color the points
diamonds %>%
  ggplot(mapping = aes(x = carat,
                       y = price)) + #aes changes asthetics of the graph
  geom_point(aes(color = color)) #scatterplot; #"thistle" is a good color; can calso do #FFFFFF system of colors;also sclae_color_brewer 
#coloring diamonds based on color of diamonds; if it has to do with a variable, you have to put it in asthetics

?geom_point
diamonds %>%
  ggplot(mapping = aes(x = carat,
                       y = price)) + #aes changes asthetics of the graph
  geom_point(aes(color = color)) +
  #scale_color_brewer(palette = "Blues")#can change color of gradient
  scale_color_manual(values = c("red", 
                                "orange",
                                "yellow", 
                                "green",
                                "blue",
                                "purple",
                                "pink"))
#graphs
install.packages("nycflights13")
library(nycflights13) 
flights
#graph 1
flights %>%
  ggplot(mapping = aes(x = origin, 
                       y = sched_arr_time)) +
  geom_violin(aes(fill = origin)) +
  scale_color_manual(values = c("red",
                                "green",
                                "blue"))
#graph 2
flights %>%
  na.omit() %>% #also what works is filter(!is.na(dep_delay))
  mutate(is.delayed = ifelse(dep_delay > 0,
                             "delayed",
                             "not delayed")) %>%
  ggplot(mapping = aes(x = carrier)) + #don't necesiarily need the "mapping = " because aes is in the right place
  geom_bar(position = "stack", aes(fill = is.delayed)) #geom col is better with using spaces between bars, can move the columns around easier

#graph 3
flights %>%
  na.omit() %>% #also what works is filter(!is.na(dep_delay))
  mutate(is.delayed = ifelse(dep_delay > 0,
                             "delayed",
                             "not delayed")) %>%
  ggplot(mapping = aes(x = carrier)) + #don't necesiarily need the "mapping = " because aes is in the right place
  geom_bar(position = "fill", aes(fill = is.delayed)) #geom col is better with using spaces between bars, can move the columns around easier


#graoh 4
flights %>%
  group_by(origin) %>%
  summarize(avg.flight.time = mean(air_time,
                                   na.rm = TRUE)) %>%
  ggplot(aes(x = reorder(origin,
                         avg.flight.time),
             y = avg.flight.time)) +
  geom_bar(stat = "identity",
           aes(fill = origin)) +
  xlab("Origin")



#class notes 2/22/19
#investigate Reddit Users data set
install.packages("data.table")
library(data.table)
fread() #best thing to read things into R; doesn't matter the extension type either
reddit <- fread(file.choose()) #use file.choose and run it to find the file you want

reddit %>%
  head(5) #look at first five observations
#learn about tendencies of users 
#look at stringr package
#
library(stringr)
#which usernames contain the letter "e"?
toy.data <- reddit %>%
  head(5) #this dataset toy.data is the first 5 observations in the reddit dataset
str_detect(toy.data$author, "e") #first argument is the vector, or clumn, of strings. take the data set in reddit and extract the column "author". Second argument is what you're looking for
#str_detect doesn't amtch capitals
#how do I detect "e" or "E"?
str_detect(toy.data$author, "e|E") #detecting lower case e or upper. putting \\ in front of the symbol to actually find the | symbol. ie "e\\|E" 
#regular expressions
str_detect(toy.data$author,
           regex("e",
                 ignore_case = TRUE)) #other method of ignorign case 
#estimate age distributions on reddit
#how many of our data has digits in the username?
str_detect(toy.data$author,
           "[:digit:]")
#filter by str_detect()
toy.data %>%
  filter(str_detect(toy.data$author,
                    "[:digit:]")
  )

#full data: what porportion of usernames have a digit?
reddit %>%
  mutate(has.digit = ifelse(str_detect(reddit$author,
                                       "[:digit:]"),
                            1,
                            0)) %>% #if there's a digit in the username, make column that has a 1, if not, make 0.
  summarize(prop = sum(has.digit)/n()) 

#Feb 25 notes
#tutrioing is tuesday 7-9
#say i want to calculate the average number of digits per username
#use str_count
str_count(toy.data$author,
          "[:digit:]")

#fi we don't want to make a long computation of number of digitis for ecomputation, save the one long computation in a dataset
reddit.digits <- reddit %>%
  mutate(num.digits = str_count(author,
                                "[:digit:]"))
reddit.digits %>%
  summarize(average = mean(num.digits))

#what's the average number of digits used in reddit usernames that contain digits?
#of usernames with at least 1 digit, how many average digits does it have?
reddit.digits %>%
  filter(num.digits >=1) %>%
  summarize(average = mean(num.digits))

#lets plot the distribution of number of digits used
#binsize 
reddit.digits %>%
  filter(num.digits >= 1) %>%
  ggplot(aes(x = num.digits)) +
  geom_bar() # treats each data point as its own category, vs a histogram, which treats it all the same category

#how many usernames end with the numebr 99?
#lets say the username alex99 ends with 99, but alex999 doesn't
test.vector <- c("Alex99", "Alex999", "Bob8")

str_detect(test.vector,
           "[:alpha:]99$|^99|[:punct]99$") #detects the usernames that ends with 99 ("99$") and is preceded by letters ([:alpha]) because we want to exclude 999
#any line that ends in a letter followed by "99", or punctuation and 99 
?str_sub
"([:alpha:]|[:punct:])[:digit:][:digit:])|^[:digit:][:digit:]$|([:digit:]([:alpha:]|[:punct:])|[:digit:])$"
reddit %>%
  filter(str_detect(author,
                    "[:alpha:]99$|^99|[:punct]99$")) %>% #detects the usernames that ends with 99 ("99$") and is preceded by letters ([:alpha]) because we want to exclude 999))
  summarize(total = n()) #gives total number of these

#INTRODUCING FOR LOOPS IN R
new.vector <- NULL #vector is empty

for(i in 1:100){
  new.vector[i] <- 2*i
} #says that i starts at 1; make it equal to 2*1, which is 2. 
new.vector

#what is the distribution of digits in reddit usernames?

#How amny 0's are there?
sum(str_count(reddit$author,
          "0")) #sums up a vector of all the isntances of 0's in usernames
count.vector <- NULL
for(i in 0:9) {
  count.vector[i+1] <- sum(str_count(reddit$author,
                             toString(i)))
  print(i)
} #tostring takes i and converts it to a string
#for i, store the elements of the vector 1-10, 
#got to dataset, count up number of 0's, store it in the vector
#will get vector of total number of the 

count.vector 
##class notes 2/27/19
reddit <- fread(file.choose())
digits.data <- data.frame(counts = count.vector,
                          digit = 0:9)

digits.data %>%
  ggplot(aes(x = factor(digit), #factor treats digit as discrete/categorical, not quantitative/continuous
             y = counts)) + 
  geom_bar(stat = "identity") #don't want to count up the numbers (which is default), so use statr = identity. Otheriwse, you get error "stat count() not used as a y aesthetic
###what is the distribution of numbers that 
##prepend usernames? (come at the beginning of)

#for example, 27Alex, 27 prepends the username
#3Alex56, 3 prepends the username
#Alex has no prepending number
toy.vector <- c("27Alex", "3Alex56", "Alex")
#str_locate finction: 
#locates first instance of a given string

str_locate(toy.vector, "A")
# the pattern "A" starts and ends at the 3'd postion. 
str_locate(toy.vector, "Alex")

#Locate first example of a number followed by a non-number
str_locate(toy.vector,
           "[:digit:]([:alpha:]|[:punct:])") #pattern of digit followed by a letter or punctuation

#let's filter our data for only usernames beginning with digits
#takes care of case "What if there are no digits in username?
reddit %>%
  filter(str_detect(author,
                    "^[:digit:]"))

#str_sub() function
#subsets/extracts a string
location_of_last_digit <- str_locate(toy.vector,
           "[:digit:]([:alpha:]|[:punct:])")[,1] #pattern of digit followed by a letter or punctuation 
#[,1] says give me all rows, first column 

str_sub(toy.vector,
        start = 1, 
        end = location_of_last_digit)
#now for whole dataset
digit.reddit <- reddit %>%
  filter(str_detect(reddit$author,
                    "^[:digit:]"))

location_of_last_digit <- str_locate(digit.reddit$author,
                                     "[:digit:]([:alpha:]|[:punct:])")[,1] #pattern of digit followed by a letter or punctuation 
#[,1] says give me all rows, first column 

prepending_numbers <- str_sub(digit.reddit$author, #need to ID column to look at, otherwise you get :"argument is not an atomic vecotr: coercing
        start = 1, 
        end = ifelse(is.na(location_of_last_digit),
                      nchar(digit.reddit$author),
                     location_of_last_digit)) #if the location of the last digit is NA, it must mean their username is all numbers, so just give us the number of caracters in the username, since they're all numbers


prepending_numbers[1:10]
#visually tells us the number of usernames there are with a specific digits in front of the username

prepend.data <- data.frame(numbers = as.numeric(prepending_numbers))
prepend.data %>%
  ggplot(aes(x = numbers)) +
  geom_histogram() +
  xlim(0,1000) +
  ylim(0, 30000)

prepend.data %>%
  ggplot(aes(x = numbers)) +
  geom_histogram() +
  xlim(0,10)

prepend.data %>%
  ggplot(aes(x = numbers)) +
  geom_bar() +
  xlim(0,10)

#now do the same for append
toy.vector <- c("Alex56", "34Alex34", "23Alex", "Alex")
digit.reddit <- reddit %>%
  filter(str_detect(author,
                    "[:digit:]$"))
start.of.last.digits <- str_locate(digit.reddit$author,
           "([:alpha:]|[:punct:])[:digit:]") #pattern of digit followed by a letter or punctuation



appending_numbers <- str_sub(digit.reddit$author, #need to ID column to look at, otherwise you get :"argument is not an atomic vecotr: coercing
                              start = ifelse(is.na(start.of.last.digits),
                                             nchar(digit.reddit$author),
                                             start.of.last.digits)) 

#end of string

#class notes 3/1
library(data.table)
reddit <- fread(file.choose())
set.seed(10)
sample.reddit <- reddit %>%
  sample_n(100)
#identify usernames with number at the end
sample.reddit %>%
  filter(str_detect(author,
                    "[:digit:]{1,}$")) #says give me one or more digits to end a string
#find location of last letter
str_locate(sample.reddit$author,
           "([:alpha:]|[:punct:])[:digit:]{1,}$")[,1]

#Extract the numbers at the end of usernames
str_sub(sample.reddit$author,
        start = location,
        end = nchar(sample.reddit$author)
#graph is; will have to remove the NA's and cast the other things as numeric

#webscraping: something exists on the web, and I want it
library(rvest)
#say i want a table from tom brady's stats
brady.url <- "https://en.wikipedia.org/wiki/Tom_Brady"
)))
#scraping syntax looks like this
# apaprently this is the best way to find the css selector we need to extract our text from https://selectorgadget.com/
brady.table <- brady.url %>%
  read_html() %>%
  html_nodes("table") %>% #because we're scarping a table; says how many tables there are
  .[[6]] %>% #get 6'th element: found by matching up code with number in xml_nodeset
  html_table(fill = TRUE)
View(brady.table) 

new.brady <- brady.table #don't make changes to original data, make copy instead
#make new colnames for brady table
colnames(new.brady) <- new.brady[1,] #take colnames of my dataset and replace it with first row of bradty table
View(new.brady)
#need to make some columns to go away
nrow(new.brady)
ncol(new.brady)
new.brady <- new.brady[-c(1,21), -23] #takes away cols

new.brady %>%
  ggplot(aes(x = Year,
             y = Yds)) + 
  geom_point()

# we neeed to convert yards to numeric
#need to remove all commas
new.brady$Yds
str_replace_all(new.brady$Yds,
                ",", 
                "") #says look through all entries, find comma, replace with empty stringe ("")

new.brady$Yds <- as.numeric(str_replace_all(new.brady$Yds,
                           ",", 
                           "")) #overwrites Yds column with new improved version of itself
new.brady %>%
  ggplot(aes(x = Year,
             y = Yds)) + 
  geom_point()

#what about text analysis?
sb.url <- ("https://en.wikipedia.org/wiki/Super_Bowl_XLII")
sb.text <- sb.url %>%
  read_html() %>%
  html_nodes("p") %>%
  html_text() #clears out html code and converts it to text

#want to combine it all together
combined.text <- str_c(sb.text,
      sep = "AAA", #shows how it combines strings
      collapse = NULL)

###Class notes 3/4/19
#install.packages("shiny")
#for online web apps
#top left, new shiny web app,  save in appropriate plaece

library(shiny)
#class notes march 6
#random number



## Class Notes 3/8/19
install.packages("leaflet")
library(leaflet)


#Build a leaflet (map)
?addTiles #
?addMarkers #adds graphic layers
leaflet() %>%
  addTiles() %>%
  addMarkers(lng=-73.1755, lat = 44.01005, popup = "Warner")


#Scrape volcano data
library(rvest)
library(tidyverse)
volcano.url <- "https://www.ngdc.noaa.gov/nndc/struts/results?type_0=Like&query_0=&op_8=eq&v_8=&type_10=EXACT&query_10=None+Selected&le_2=&ge_3=&le_3=&ge_2=&op_5=eq&v_5=&op_6=eq&v_6=&op_7=eq&v_7=&t=102557&s=5&d=5"

volcano.data <- volcano.url %>%
  read_html() %>%
  html_nodes("table") %>% #says that we're looking for table in the url
  .[[3]] %>% #gets 3rd element found by matching up nodeset
  html_table()

View(volcano.data)



#Plot locations of all volcanos on my leaflet
volcano.data %>%
  leaflet() %>%
  addTiles() %>%
  addMarkers(clusterOptions = markerClusterOptions(),
             lat = ~Latitude,
             lng = ~Longitude,
             popup = paste("Region", volcano.data$Region, "<br>",
                           "Elevation", volcano.data$Elev, "<br>"),
             label = ~Country)
?addMarkers



#Add our own icons

tomBrady <- makeIcon(
  iconUrl = "https://fanatics.frgimages.com/FFImage/thumb.aspx?i=/productimages/_1347000/altimages/FF_1347038ALT2_full.jpg&w=900",
  iconHeight = 30,
  iconWidth = 30
)

peytonManning <- makeIcon(
  iconUrl = "https://en.wikipedia.org/wiki/Cookie#/media/File:2ChocolateChipCookies.jpg",
  iconHeight = 30,
  iconWidth = 30
)

volcano.data2 <- volcano.data %>%
  mutate(height = ifelse(Elev > 3000,
                         "tall",
                         "short"))

footballIcons <- iconList(
  tall = tomBrady,
  short = peytonManning
)

volcano.data2 %>%
  leaflet() %>%
  addTiles() %>%
  addMarkers(icon = ~footballIcons[height])
#March 11 Code
#no shiny on test
#give test on Wednesday, due friday

###Learning to make choropleths (colorful maps to show things)
###json file is a format readable by a lot of things
library(geojsonio)
install.packages("geojsonio")
install.packages("ggrepel")
states_url <- "http://eric.clst.org/assets/wiki/uploads/Stuff/gz_2010_us_040_00_5m.json"

states <- geojson_read(states_url, what = "sp") #states is spatial polygon dataframe

#Look at my data
View(states@data) #@symbol looks through files in a system
#Look at my map
library(tidyverse)
library(leaflet)
#draws map
map<-states %>%
  leaflet() %>%
  addTiles() %>%
  setView(-96, 37.8, 4) #US view
map
#only works if you've got a file to make thsee, like geojson

#Now lets color each state based on its size using the map we've drawn and columns in the spatial polygon datafram
library(ggplot2)
states@data %>%
  ggplot(aes(x = CENSUSAREA)) +
  geom_histogram(bins = 100) +
  xlim(0, 200000)
bins <- c(0, 25000, 50000, 75000, 100000, 125000, 150000, Inf)#what are the cutoffs for coloring these? anything between these ranges will be one color or another color
colors <-colorBin(palette = "YlOrRd", #whats the gradient I want
                  domain = states@data$CENSUSAREA, #this is the thing I'm basing my color gradient off of
                  bins = bins) 
map %>%
  addPolygons(fillColor = ~colors(CENSUSAREA),
              weight = 2, #make darks darker and lights lighter
              opacity = 1,
              color = "white", 
              dashArray = "3",
              fillOpacity = 0.7)

#want to color by population, but we don't have it, so we need to go get it
#scrape in state population data
library(rvest)
pop.url <- "https://en.wikipedia.org/wiki/List_of_states_and_territories_of_the_United_States_by_population"
#read it in
pop.data <- pop.url %>%
  read_html() %>%
  html_nodes("table") %>%   #looking for tables here
  .[[3]] %>% #get the third table down since that's the one we want
  html_table()

#need to get rid of notes
pop.data.clean <- pop.data[-c(66:nrow(pop.data))] #removes rows from 66 on
#remove all commas in numbers, replace with nothing
colnames(pop.data.clean)[3] <- "Population" #can't have columns start with numnbers in R so change name to population
pop.data.clean$Population <- str_replace_all(pop.data.clean$Population,
                                              ",",
                                              "")
#Convert these to numbers
pop.data.clean$Population <- as.numeric(pop.data.clean$Population)

#joining to Original data frame
#Rename column 1 to NAME, same as other one
colnames(pop.data.clean)[1] <- "NAME"

pop.data.clean <- pop.data.clean %>%
  select(NAME, Population)
#left join looks to join tables on similar rows, throwes out ros that aren't found on both datasets
#join
states@data <- left_join(states@data,
                         pop.data.clean,
                         by = "NAME") #match up columns by NAME
View(states@data)

#NOW COLOR by population
map<-states %>%
  leaflet() %>%
  addTiles() %>%
  setView(-96, 37.8, 4) #US view
#only works if you've got a file to make thsee, like geojson

#Now lets color each state based on its size using the map we've drawn and columns in the spatial polygon datafram
library(ggplot2)
states@data %>%
  ggplot(aes(x = CENSUSAREA)) +
  geom_histogram(bins = 100) +
  xlim(0, 200000)
bins <- c(0, 1000000, 3000000, 5000000, 15000000, Inf)#what are the cutoffs for coloring these? anything between these ranges will be one color or another color
colors <-colorBin(palette = "YlOrRd", #whats the gradient I want
                  domain = states@data$Population, #this is the thing I'm basing my color gradient off of
                  bins = bins) 
map %>%
  addPolygons(fillColor = ~colors(states@data$Population.y),
              weight = 2, #make darks darker and lights lighter
              opacity = 1,
              color = "white", 
              dashArray = "3",
              fillOpacity = 0.7)

### Class 3/18/19
library(tidyverse)
#learnignn how to publish shiny app
install.packages('rsconnect')
rsconnect::setAccountInfo(name='sfgraf',
                          token='84BD69A23CD6012146669EFA43234157',
                          secret='rHctlBVM5tm+wVbrJgvw2KQ+9Vq9QLYLhSfonNur')
#read in income datra
library(data.table)

life.data <- read_csv(file.choose()) #"life_expectancy_years.csv
#convert from wide format (years in each column) to ling format (years in one column)
#wide data is good for summary tables, but need long format in order to do analysis
#introduce the gather() function
#key is the column name for all the columns about to be put into one column
#value is the data in the table that you will put there
#-country is the column you wont gather but will still keep in the new dataframe
life.gathered <- life.data %>%
  gather(key ="year",
         value = "LifeExpectancy",
         -country) #key and value are column names of new dataset, but "country" is the name of the column that we don't want to gather
#lets graph life extantacny over time for all countries: geom_point
#can use geom_line for a time series too
life.gathered$year <- as.numeric(life.gathered$year)  #need to to do this so ggplot doesn't think year is a factor and doesn't clog up the x axis for viewing
life.gathered %>%
  filter(country == "Indonesia" | country == "Iran" | country =="Iraq") %>%
  ggplot(aes(x = year,
             y= LifeExpectancy)) +
  geom_line(aes(color = country),
            size = 3) #size is weight of line

#for income datra
income.data <- read_csv(file.choose()) #read_csv does the columns correctly if in fread, the columns are being read in as first row
income.gathered <- income.data %>%
  gather(key = "year",
         value = "gdp",
         -country)

#graph lifeexpectancy
income.gathered$year <- as.numeric(income.gathered$year)
income.gathered %>%
  filter(country == "Indonesia" | country == "Iran" | country =="Iraq") %>%
  ggplot(aes(x = year,
             y= gdp)) +
  geom_line(aes(color = country),
            size = 3) #size is weight of line


#let's combine datasets using a join
#left_join(): finds one column in a datset and another in another dataset and match them up
#left_join(): puts one left hand dataset and matches it up in right 
## Class Notes 3/20/19
library(data.table)

#Using left_join()

joined.data <- left_join(life.gathered,
                         income.gathered,
                         by = c("country", "year"))

# joined.data %>%
#   filter(is.na(lifeExpectancy))

View(joined.data)

#Let's recreate a single frame from the video on Day 1
#Specifically, let's look at the year 1991

joined.data %>%
  filter(year == 1991) %>%
  ggplot(aes(x = gdp,
             y = LifeExpectancy)) +
  geom_point() +
  scale_x_log10()


# What am I missing?

# 1) Color my points by continent/region               DONE!
# 2) Size my points by population (scraping pop data)  DONE!
# 3) Label/highlight countries in some way             DONE!
# 4) Change x-axis                                     DONE!




#Convert from wide format to long format
#Introduce the gather() function

pop.data <- read_csv(file.choose())

pop.gathered <- pop.data %>%
  gather(key = "year",
         value = "Population",
         -country)

#Let's graph the life expectancy over time for all countries

pop.gathered$year <- as.numeric(pop.gathered$year)


joined.data2 <- left_join(joined.data,
                          pop.gathered,
                          by = c("country", "year"))




joined.data2 %>%
  filter(year == 1991) %>%
  ggplot(aes(x = gdp,
             y = LifeExpectancy)) +
  geom_point(aes(size = Population)) +
  scale_x_log10()



#Scrape in region data
region.url <- "https://meta.wikimedia.org/wiki/List_of_countries_by_regional_classification"

region.data <- region.url %>%
  read_html() %>%
  html_nodes("table") %>%
  .[[1]] %>%
  html_table()

#Join more data

colnames(region.data)[1] <- "country"


joined.data3 <- left_join(joined.data2,
                          region.data)



#Make new graph
joined.data3 %>%
  filter(year == 1991) %>%
  filter(!is.na(Region)) %>%
  ggplot(aes(x = gdp,
             y = LifeExpectancy)) +
  geom_point(aes(size = Population,
                 color = Region)) +
  scale_x_log10() +
  geom_text(aes(label = country),
            check_overlap = TRUE)


#Investigate NA values
joined.data3 %>%
  filter(is.na(Region)) %>%
  select(country)

#Let's remember to check out the
library(ggrepel)

country.subset <- joined.data3 %>%
  filter(year == 1991) %>%
  filter(country == "United States" | country == "China" | country == "India")

#Make new graph
joined.data3 %>%
  filter(year == 1991) %>%
  filter(!is.na(Region)) %>%
  ggplot(aes(x = gdp,
             y = lifeExpectancy)) +
  geom_point(aes(size = Population,
                 color = Region)) +
  scale_x_log10() +
  geom_text_repel(data = country.subset,
                  mapping = aes(label = country))


#Save our joined.data3 dataset
write.csv(joined.data3,
          "Gapminder.csv")



#### PAUSE CURRENT NOTES




## 3/22/19


#Sentiment Analysis

trump.url <- "https://en.wikipedia.org/wiki/Donald_Trump"

trump.text <- trump.url %>%
  read_html() %>%
  html_nodes("p") %>%
  html_text()

trump.text <- str_replace_all(trump.text,
                              "\n",
                              "")

trump.data <- data.frame(text = trump.text,
                         paragraph = 1:188,
                         stringsAsFactors = FALSE)

#Now my goal is to calculate the sentiment score
#for each paragraph, and graph it.
library(tidytext)

trump.tokens <- trump.data %>%
  unnest_tokens("word", "text")

#Count up instances/uses of each word
trump.word.usage <- trump.tokens %>%
  count(word) %>%
  arrange(-n) %>% 
  anti_join(stop_words)#remove all stopwords

#Show a graph of the top 10 words
trump.word.usage %>%
  head(10) %>%
  ggplot(aes(x = reorder(word,n),
             y = n)) +
  geom_bar(stat = "identity") +
  coord_flip()


#Calculate sentiment
#3 sentiment dictionaries
#bing
#afinn
#nrc
get_sentiments("bing")
get_sentiments("afinn")
get_sentiments("nrc")

trump.word.usage <- trump.tokens %>%
  count(word, paragraph) %>%
  arrange(paragraph, -n) %>% 
  anti_join(stop_words)

#Count up total positive or negative sentiments per paragraph
g1 <- trump.word.usage %>%
  inner_join(get_sentiments("bing")) %>%
  count(sentiment, paragraph) %>%
  arrange(paragraph) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(total.sentiment = positive - negative) %>%
  ggplot(aes(x = paragraph,
             y = total.sentiment)) +
  geom_bar(stat = "identity")

#Count up total positive or negative sentiments per paragraph
#Let's use afinn
g2 <- trump.word.usage %>%
  inner_join(get_sentiments("afinn")) %>%
  group_by(paragraph) %>%
  summarize(total.sentiment = sum(n*score)) %>%
  ggplot(aes(x = paragraph,
             y = total.sentiment)) +
  geom_bar(stat = "identity")

#use grid.arrange() to put plots side by side
library(gridExtra)

grid.arrange(g1,g2)

#Find paragraph with very negative sentiment
trump.word.usage %>%
  inner_join(get_sentiments("afinn")) %>%
  group_by(paragraph) %>%
  summarize(total.sentiment = sum(n*score)) %>%
  arrange(total.sentiment)

trump.text[72]


get_sentiments("bing") %>% filter(word == "trump")
get_sentiments("afinn") %>% filter(word == "trump")

# %in%

names <- c("Alex", "Bill", "Bob")
names2 <- c("Alex", "Jill", "Bob")

names %in% names2

library(tidyverse)

wanted.diamonds <- c("D", "F", "G")

diamonds %>%
  filter(color %in% wanted.diamonds)


#Introducing the get() function

color <- "red"
get("color")


#Introduce plotly
install.packages("plotly")
library(plotly)

#joined data starts on line 688 

gg1 <- joined.data3 %>%
  filter(year == 1991) %>%
  filter(!is.na(Region)) %>%
  ggplot(aes(x = gdp,
             y = lifeExpectancy,
             text = country)) +
  geom_point(aes(size = Population,
                 color = Region)) +
  scale_x_log10() +
  scale_size_continuous(range = c(1,20)) +
  theme_bw()

#use ggplotly() to make a plotly from a ggplot
ggplotly(gg1, tooltip = c("x", "y", "country"))

## Class notes 4/5/19
#repeat function
#making a data frame
?data.frame
wealth.data <- data.frame(Gender = rep(c("M", "F"), each = 3),
                          Type = rep(c("Inherited", "Inherited/Self-made", "Self-made"),
                                     times = 2),
                          Value = c(.079, .301, .620, .535, .296, .169))


wealth.data %>%
  ggplot(aes(x = Gender,
             y = Value)) + 
  geom_bar(aes(fill = Type),
           stat = "identity")
#error stat count must not be used with a y aesthetic; need to put stat at identity
# n = 2000
#1800 men
#200 women


#Introducing a Mosaic Plot to help show sample sizes; want to use width to represent number of people in each group
#takes contingency table with the number of counts, so can't use a table with just proportion

#get data into R
#want them as a matrix. Matrix elements have to be same type (ex: numeric). Matrices can also have row names easier than dataframes can
wealth.matrix <- matrix(c(142.2, 107, 541.8, 59.2,
                           1116,
                           33.8),
                         nrow = 2,
                         ncol = 3) #by default, matrix function reads data in column-wise
rownames(wealth.matrix) <- c("Male", "Female")
colnames(wealth.matrix) <- c("Inherited", "Inherited/Self-made", "Self-made")

#wealth matrix gets put inside mosiac plot
#area of rectangle represents number of people in that subcategory
mosaicplot(wealth.matrix,
           shade = TRUE)
##shade shows where you would expect more or less value; chi squared


###Exam due Sunday
###April 8 notes
#will have to publish app on shiny.io
library(tidyverse)
library(rvest)
#better version to webscape paragraph; gets directly at the thing (inspect, right click on thing we want, copy, copy xpath)
"https://en.wikipedia.org/wiki/Food#Shelf-stable_food" %>%
  read_html() %>%
  html_nodes(xpath = '//*[@id="mw-content-text"]/div/p[59]') %>%
  html_text()

#nested ifelse() functions

#supose alex and bob get As on test, Tim and tom get B's, and John gets an F
#how to get dataset that shows this?
test.data <- data.frame(names = c("Alex", "Bob", "Tim", "Tom", "John"))

test.data %>%
  mutate(grade = ifelse(names %in% c("Alex", "Bob"), 
                      "A",
                      ifelse(names %in% c("Tim", "Tom"),
                             "B",
                             "F")))

library(geojsonio)
library(leaflet)
#displaying the map
states.url <- "http://eric.clst.org/assets/wiki/uploads/Stuff/gz_2010_us_040_00_5m.json"

states <- geojson_read(states.url, what = "sp")
View(states@data)

#getting the data
map <- states %>%
  leaflet() %>%
  addTiles() %>%
  setView(-96, 37.8, 4)         
x<-"https://disa.com/map-of-marijuana-legality-by-state" %>%
  read_html() %>%
  html_nodes(xpath = '//*[@id="Content"]/div[2]/div[2]/div/div/div[3]/table') %>%
  html_table() %>%
  .[[1]] #need this because it's getting the dataframe from a list of 1 to an actual dataframe
View(x)

bins <- c(0, 25000, 50000, 75000, 100000, 125000, 150000, Inf)#what are the cutoffs for coloring these? anything between these ranges will be one color or another color
colors <-colorFactor(palette = "YlOrRd", #whats the gradient I want
                  domain = x$`Legal Status` #this is the thing I'm basing my color gradient off of
                  ) 
map %>%
  addPolygons(fillColor = ~colors(x$`Legal Status`),
              weight = 2, #make darks darker and lights lighter
              opacity = 1,
              color = "white", 
              dashArray = "3",
              fillOpacity = 0.7,
              popup = paste("State", x$State, "<br>",
                            x$`Legal Status`)
              
## Class Notes 4/10/19

bball <- read_csv("Basketball.csv")
View(bball)              

#graph of blocks vs sagarin.Rating
bball %>%
  ggplot(aes(x = Blocks,
             y = Sagarin.Rating))+
  geom_point()

#make  agraph of possessions per game 
#versus opponents possessions per game
bball %>%
  ggplot(aes(x = PossessionsPerGame,
             y = `O-PossessionsPerGame`))+
  geom_point()

bball %>%
  ggplot(aes(x = FGM,
             y = AssistsPerGame))+
  geom_point()
#need to tchange this dataset
bball %>%
  ggplot(aes(x = OREB,
             y = DREB))+
  geom_point()
#one way:
#replace Syrcause DREB value with 920
bball[194, "DREB"] <- 920 #take datset and row 194

bball %>%
  ggplot(aes(x = FTM,
             y = FTA))+
  geom_point() +
  geom_text(aes(label = Team))

###April 15
install.packages("rtweet")
library(rtweet)

#scraping tweets
#have 30,000 tweets scrape limit
tweets1 <- search_tweets("#notredame",
                         n = 1000,
                         include_rts = FALSE) #searches through all of twitter for something, 1000 is the max tweest allowed, ony originial tweets
#lets say we want to live stream tweets
live.notre.dame.tweets <- stream_tweets("#notredame",
                                        timeout = 30,
                                        file_name = "notredame.json") #want to save as json file so that we don't accidently overwrite our data. use json allows you to code later to geojson. 
#saved in my current working directory
#read back in
nd.tweets <- parse_stream("notredame.json")
View(nd.tweets)

#what clients are people tweeting from?
#number of posts by platform
nd.tweets %>%
  group_by(source) %>%
  summarize(count = n())
#numebr of tweets by user
nd.tweets %>%
  group_by(screen_name) %>%
  summarize(count = n()) %>%
  arrange(-count)

#let's invesitgate the user "A_de_St_Germain"
nd.tweets %>%
  filter(screen_name == "A_de_St_Germain") %>%
  select(text) %>%
  View()

#let's look at #RussellWestbrook
rw.tweets <- search_tweets("#RussellWestbrook",
                           n = 1000,
                           include_rts = FALSE) #stop after getting the last 1000 tweets to use this hashtag. Not case sensitive
#Ttrack numbers of tweets over time
#whose tweets to track?
#TigerWoods, Elon Musk, Pope Francis

tweets3 <- get_timelines(c("TigerWoods",
                           "ElonMusk",
                           "Pontifex"),
                         n = 500)
#How many tweets from each handle did I actually scrape?
tweets3 %>%
  group_by(screen_name) %>%
  summarize(count = n())

#Let's amke a time series plot of tweets
#for these three paeople
#for this month
tweets3 %>%
  filter(created_at >= "2019-04-01") %>% #converted to a datetime object, sothis syntax works
  group_by(screen_name) %>%
  ts_plot("days") + #time-series plot: x axis time, y axis number of tweets. time unit = days.
  geom_point() +
  theme_bw() #black and white theme

#let's find all fo the people that our president is following
#@realdonaldtrump
trump_friends <- get_friends("realdonaldtrump") #how many people is this person following?
#convert from user_id into real screen name
#gives their most recent tweet as well
trump_friends <- lookup_users(trump_friends$user_id) 

#looking at ian lummis
ian_friends <- get_friends("ian_lummis")
ian_friends <- lookup_users(ian_friends$user_id) 

#scraping his tweets
tweets4 <- get_timelines(c("ian_lummis",
                           "theRealJakeBas",
                           "gTRON3",
                           "SamwiseGrafee"),
                           n = 1500)
tweets4 %>%
  group_by(screen_name) %>%
  summarize(count = n())

tweets4 %>%
  group_by(screen_name) %>%
  ts_plot("days") + #time-series plot: x axis time, y axis number of tweets. time unit = days.
  geom_point() +
  theme_bw() #black and white theme

?search_tweets


## create lat/lng variables using all available tweet and profile geo-location data
rt <- lat_lng(rt)

## plot state boundaries
par(mar = c(0, 0, 0, 0))
maps::map("state", lwd = .25)

## plot lat and lng points onto state map
with(rt, points(lng, lat, pch = 20, cex = .75, col = rgb(0, .3, .7, .75)))


###Class notes 4/19
#introduction to linear regression
#residual value is point observed - predicted
#best line is "least squares line"; want to minimize absolute value of residuals
house <- read_csv("train.csv")

house %>%
  ggplot(aes(x = SalePrice)) +
  geom_histogram()

#alwayss do some exporatory analysis before constructing regression model
house %>%
  ggplot(aes(x = LotArea,
             y = SalePrice)) +
  geom_point()
#don't adjust data at all, make a regression model
#contrl+I gets code to tab correctly
questionable.model <- lm(SalePrice ~ LotArea, #sale price as a function of lotArea (dependent ~independent)
                         data = house)
questionable.model

#gives slopes and intercept for linear model
#plot our model on top of our data
house %>%
  ggplot(aes(x = LotArea,
             y = SalePrice)) +
  geom_point() +
  geom_abline(slope = 2.1, 
              intercept = 158836.2,
              color = "red",
              size = 3)


questionable.model$coefficients
house %>%
  ggplot(aes(x = LotArea,
             y = SalePrice)) +
  geom_point() +
  geom_abline(slope = questionable.model$coefficients[2], 
              intercept = questionable.model$coefficients[1],
              color = "red",
              size = 3)
#First we should look only at lots <50000
house.subset <- house %>%
  filter(LotArea <= 50000)
#just removed 11 houses
house.subset %>%
  ggplot(aes(x = LotArea,
             y = SalePrice)) +
  geom_point()
#fit model with new data
house.subset.model <- lm(SalePrice ~ LotArea,
                         data = house.subset)
house.subset.model
house.subset %>%
  ggplot(aes(x = LotArea,
             y = SalePrice)) +
  geom_point() +
  geom_abline(slope = house.subset.model$coefficients[2], 
              intercept = house.subset.model$coefficients[1],
              color = "red",
              size = 3)
#here's hwo we can make predictions using our model
#Say the lot area of a house is 10000 sq feet
house.subset.model$coefficients
predict.lm(house.subset.model,
           data.frame(LotArea = 10000)) #takes a model and a dataframe that you want to make predictors for

#April 22
View(house.subset)
house.model2 <- lm(SalePrice ~ LotArea + BedroomAbvGr + Id,
                   data = house.subset)
summary(house.model2)
#intercept is 97460
#lotarea coefficiant = 6.136
#Bedroom coefficient = 7731
#so Saleprice = 97360 + 6.136(Area) + 7731(Bedroom)
#so for each additional sq foot, price increases $6 and for each additional bedroom, the rpcie will increase 7731
#p valuep values tell relationship between variable and sale price: is there a strong relationship? low p value means yes

#making garbage data to demonstrate randomness of p values with a variabel like "Id"
#basically, if ou take enough random data, p value will be .05 or lower 5% of the time; so sometimes something is found significant when it's actually not

set.seed(28) #decides where to start on the random number generator, based on hardware 
garbage.data <- data.frame(test = rnorm(1449))
for(i in 1:100){
  garbage.data <- cbind(garbage.data, rnorm(1449))
}

garbage.data <- cbind(garbage.data, house.subset$SalePrice) #put house sale data into this garbage data

colnames(garbage.data)[1:101] <- 1:101
garbage.model <- lm(`house.subset$SalePrice` ~ ., #include all variables 
                    data = garbage.data)
summary(garbage.model)
#now how do i evaluate this? 
#two ways to compare and evaluate models
# R- squared: the percent variation in our response variable
#that's explained by our model
#Example: you can expain 15% of the variation with our house model given the three variables
summary(house.model2)$r.squared
#drawbacks:not great for comparing two models; can cheat Rsquared by adding a bunch of different dimensions to model

#AIC: lower values are better
#penalized for adding more variables to the model
AIC(house.model2)

#introcuing the step() function
#try all combinations of variablse in the model, use one with lowest AIC
step(house.model2)
#removing LotArea gives the highest AIC. so LotArea is the best predictor variable for this model

#Class notes 4/26
#unsupervised learning: have no idea what the answers are,
#or if theyre are answers at al.
# so we will clister or group our data into some level of groups to try and "infer" some level of data



#supervised learning: We know the "answer" for some treaining
#data, wnd we want to predict the "answers for new data.


#learn how to use k-means clustering
#says" i have lots of data and i want to understand the structure of them/how they're related

iris #data on types of irises, which are flowers
iris.subset <- iris %>% # get iris datasetr without species
  select(-Species)

iris.subset %>%
  ggplot(aes(x = Petal.Length,
             y = Petal.Width)) +
  geom_point()
#k means clutering
#k is ht enumber of groups that you think exists in your data
#in this case: 2 groups
#ex: pickks two random points, calculates which points are closest to which points, then re-shifts points to be the center of the cluster

#doing it
iris.kmeans <- kmeans(iris.subset,2) #first arg is dataset, 2nd is "k" (number of centers you want/#groups)

iris.kmeans$cluster

#making new column where it says which cluster the point is in
iris.subset %>%
  mutate(cluster = iris.kmeans$cluster) %>%
  ggplot(aes(x = Petal.Length,
             y = Petal.Width)) +
  geom_point(aes(color = factor(cluster)))
#calculating points based on 4 dimensions (4 variables) which is why some points seem to be in other cluster
#helpful in picking out interesting cases
data <- read_csv(file.choose())
comedy <- data %>%
  mutate(genre = ifelse(str_detect(genres, "Comedy") == TRUE,
                        "Yes",
                       "No"))
comedy1 <- comedy %>%
  select(popularity,
         budget,
         revenue,
         runtime) %>%
  na.omit()

comedy1 %>%
  ggplot(aes(x = budget,
             y = revenue)) +
  geom_point()

comedy1.kmeans <- kmeans(comedy1, 3)
comedy1 %>%
  mutate(cluster = comedy1.kmeans$cluster) %>%
  ggplot(aes(x = budget,
             y = revenue)) +
  geom_point(aes(color = factor(cluster)))

###class notes 4/25
library(data.table)
#called epl stats
soccer <- fread(file.choose())


#goal today: use hierarchical clustering
#to find commonlaities among players

#remove categorical vriables
soccer2 <- soccer %>%
  select(-contains("kickoff"), -name, -X1) #boolean stats (True or False) are ok to keep

#want to scale variables before doing euclidean distance
#becasue there's a large difference between someone playing 90 mintues vs 86, vs 5 goals vs 1

soccer.scaled <- scale(soccer2) #get's Z scores: number of standard deviationns away from the mean you are

#we can makea distance matrix based on these z scores
#matrix gives us the distances. pair-wise, between these two points 
#so in this case, the matrix is 46k by 46k
#but first take a subset for closer analysis  becuase other dataset is too large
set.seed(30)
soccer.sample <- soccer2 %>%
  sample_n(100)
soccer.scaled <- scale(soccer.sample) #get's Z scores: number of standard deviationns away from the mean you are


distance.matrix <- dist(soccer.scaled)
#visualize the results of our hierarchical clustering (see how far away each poin tis in relation to each other)
#using a dendrogram
clustering1 <- hclust(distance.matrix)
plot(clustering1)
#anyting that's close to each other on x axis represents how similar the players are: so close together means they're really similar
#y axis (height) shows that groups behave relative to each other
#in this case, most players have more in common than 100, 73, and 75. 
#22 and 94 are "relatively" close to each other, but 44 and 67 are really close to each other
#next step: why do certain players behave similarly? what makes them diferent?
#so w e Cut my dendrogram using cutree()
#install.packages("dendextend")
library(dendextend)
cutree(clustering1, 
       k = 4) #number of cuts in the tree

###aside: investigate player 75
View(soccer.sample[75,])

cluster.10.value <- cutree(clustering1,
       k = 10)
soccer.sample %>%
  mutate(cluster = cluster.10.value) %>%
  ggplot(aes(x = minutes,
             y = attempted_passes)) +
  geom_jitter(aes(color = factor(cluster)),
              size = 3) #start to see why people might be grouped together; choose minutes and passes because it mightbe a discrimiating factor
#redo analysis with n = 6 people to get a better visual representation
sample_players <- soccer %>%
  filter(name == "Joe_Hart" |
         name == "Eden_Hazard" |
         name == "Mohamed_Salah" |
         name == "Paul_Pogba" |
         name == "Sadio_Man?" |
         name == "David_de Gea") %>%
  group_by(name) %>%
  summarize(mean.goals = mean(goals_scored),
            mean.goals.against = mean(goals_conceded),
            mean.passes = mean(completed_passes),
            mean.minutes = mean(minutes))

###notes May 1
#these metrics aren't good in distunguishingbetween them, so here are bettwer ones. 
soccer <- read_csv(file.choose())

sample_players <- soccer %>%
  filter(name == "Joe_Hart" |
           name == "Eden_Hazard" |
           name == "Mohamed_Salah" |
           name == "Paul_Pogba" |
           name == "Sadio_Man?" |
           name == "David_de Gea") %>%
  group_by(name) %>%
  summarize(mean.attempt.pass = mean(attempted_passes),
            mean.goals = mean(goals_conceded),
            mean.comp.pass = mean(completed_passes),
            mean.dribbles = mean(dribbles),
            mean.fouls = mean(fouls),
            mean.assists = mean(assists))
#goal is now to cluster based on this
#do same analysis
#scale vairbales, take out "name"
soccer.scaled <- scale(sample_players %>%
                         select(-name)) #get's Z scores: number of standard deviationns away from the mean you are

#make dist matrix
distance.matrix <- dist(soccer.scaled)

hc1 <- hclust(distance.matrix)
plot(hc1)

d1 <- as.dendrogram(hc1)
labels <- sample_players$name[order.dendrogram(d1)] #try and get names of players
plot(d1)

#make three clusters
clusters <- cutree(hc1, k = 3)
#draw rectangles around clusters
rect.hclust(hc1,
            k = 3,
            border = 1:3)
#choosing colors
d1 <- color_branches(d1, k = 3)


## Class Notes 5/3/19

library(dendextend)
d1 %>%
  set("labels_col",
      c("Red", "Red", "Blue", "Green", "Green", "Green")) %>%
  plot()

#Make 3 clusters
clusters <- cutree(d1, k = 3)

#Choose my colors
colors <- c("Red", "Blue", "Green")

colors[clusters[labels(d1)]]

d1 %>%
  set("labels_col",
      colors[clusters[labels(d1)]]) %>%
  set("labels_cex", 2) %>%
  circlize_dendrogram()
#plot()

#Let's redo this using kmeans

soccer.k <- kmeans(soccer.scaled,
                   3)

#Investigate the compactness of my clusters
soccer.k$tot.withinss

soccer.k <- NULL

for(i in 1:4){
  s <- kmeans(soccer.scaled,
              i)
  soccer.k[i] <- s$tot.withinss
}

soccer.k

#Create an elbow plot
plot(soccer.k)


## class notes may 6
# Put many trees together into a forest (a random forest)
#an industry standard for machine learning
library(randomForest)
#drawback to decision trees: biased by data

forest1 <- randomForest(spam ~ .,
                        data = email) #spam as a function of everything, using email data
?randomForest
#tuning parameter: ntree (number of individual trees created); usually increasing it is better
#as dataset is bigger and bigger, it increases runtime linearly. 
#after 500 trees, diminishing returns on accuracy
#other tuning paramter: mtry: number of variables that's going to be selected at the split of each tree
forest1
#gets confusioun matrix: rows tell how much you're correct or wrong
#variable importance plot in order to interpret this
#asks "which variables are the msot important for detecting spam emails?
varImpPlot(forest1)
#x axis says "if we lose this variable, this is the information we're losing" (how much worse would this model be)
#tune our random forest: make it better
tuneRF(email %>% select(-spam),
       email$spam,
       mtryStart = 1) #initial variables: how many do i want to test at each split? #
#tell's us error rate for each amount of variables tried
#using this to try and predict what genre a song belongs to
##3class notes may 8
library(tidytext)
library(randomForest)
library(caret)
library(fread)
library(tidyverse)

lyrics <- read_csv(file.choose()) #MATH216B ML_ Predicting Song Genre 5_6_2019 - Sheet1 title name
lyrics2 <- lyrics %>%
  select(-X4)



#Check everyone's favorite songs
lyrics2 %>%
  count(Title) %>%
  arrange(-n)
#remove NA's
lyrics2 <- lyrics2 %>%
  na.omit() 

#Let's look at words in all songs
lyrics2 %>%
  select(Lyrics) %>%
  unnest_tokens(word, Lyrics) %>% #helps to get the individual words out of the lyrics column, calls each one "words" (i think)
  count(word,
        sort = TRUE) %>% #counts the number of words and sorts them 
  anti_join(stop_words) #removes simple stop words like "i, he, and, " etc

#document feature matrix tells us how many times each unique word occurs in each song (x column is word, y is songs); can then append genre column to matrix
#Do text mining stuff
library(quanteda)
dfm1 <- lyrics2 %>%
  corpus(text_field = "Lyrics") %>%
  dfm(stem = TRUE,
      remove = stopwords("english"),
      remove_punct = TRUE,
      ngrams = 1) %>%
  convert(to = "matrix")   


dfm1 %>%
  head() %>%
  View()

table(lyrics2$Genre)  %>% #makes frequency table of the number of times a genre appears
  View()

lyrics2 <- lyrics2 %>%
  mutate(rock = ifelse(str_detect(lyrics2$Genre,
                                  regex("rock", ignore_case = TRUE)),
                       "1",
                       "0")) ##if i detect any form of "rock" (ignoring case), make a 1, else call it a 0. Put in quotes so that they're factors

#need to convert document feature matrix to dataframe for rpart tree
dfm2 <- as.data.frame(dfm1)

#try and predict rock as a function of everything
#first get predictor variables together with if something is rock
dfm2 <- cbind(dfm2, lyrics2$rock) #column bind
colnames(dfm2)[3777] <- "genre.variable" #subsets and renames column 3777 to "genre.variable" (because you can see in the environment that there are 3777 variables)
table(lyrics2$rock) #shows how many songs are rock songs

library(rpart)
tree1 <- rpart(genre.variable ~ .,
               data = dfm2)

#visualizes the data
library(rattle)
fancyRpartPlot(tree1)
