#Here is how to add 2+2
2+2

#Store things (assign things)
x <- 2+2
y <- 6
x + y

#Learn how to use functions

#c() function: concatenate
vector1 <- c(1,2,9,15,1000)


#mean() function: takes a mean
mean(x = vector1,
     trim = .25)


#Bring up help documentation
?mean


#Installing new packages
#install.packages('tidyverse')
library(tidyverse)

#Explore the diamonds data set
diamonds
View(diamonds)
?diamonds


##### tidyverse "verbs"

#filter() function: subsets our data based
#on a logical or mathematical expression

expensive.diamonds <- filter(.data = diamonds,
                             price > 12000)

#Find color D or E diamonds
expensive.pretty.diamonds <- filter(.data = expensive.diamonds,
       color == "D" | color == "E")

#Slightly more efficient way
filter(.data = diamonds,
       price > 12000,
       color == "D" | color == "E")

#Introducing the pipe operator %>%
mean(vector1)

vector1 %>%
  mean()

#Most efficient way
ex1 <- diamonds %>%
  filter(price > 12000) %>%
  filter(color == "D" | color == "E")

#select() function: selects particular columns of interest
diamonds %>%
  select(carat, price)

diamonds %>%
  select(-price)


#arrange() function: sorts a data set
#arrange in ascending order
diamonds %>%
  arrange(carat)

#arrange in descending order
diamonds %>%
  arrange(-carat)


## Class Notes 2/18/19
diamonds %>%
  arrange(-carat) %>%
  head(5)

diamonds %>%
  arrange(carat, price) %>%
  head(10)

## Identify the most expensive diamond with greater than 2 carats
## That doesn't belong to the "worst" 2 colors.

diamonds %>%
  filter(carat > 2) %>%
  arrange(-price) %>%
  filter(color != "J" & color != "I") %>%
  head(1) 

# mutate() function: adds new column(s) to the data
sdkjfshksdkfhsfd <- diamonds %>%
  mutate(volume = x*y*z)

# group_by() divides the data into groups based on a
# a categorical variable
# summarize() applies a mathematical function to the data

diamonds %>%
  summarize(mean.price = mean(price))

diamonds %>%
  group_by(color) %>%
  summarize(mean.price = mean(price))


diamonds %>%
  group_by(color, cut) %>%
  summarize(mean.price = mean(price),
            max.price = max(price),
            count = n())

#install.packages('nycflights13')
library(nycflights13)
?flights


# 1) What is the average departure delay for flights into
# Burlington Airport vs Logan Airport (Boston)

# 2) What proportion of flights are delayed heading into
# Burlington Airport vs Logan Airport? (Hint: ifelse())


## Class Notes 2/20/19

#1)
flights %>%
  filter(dep_delay > 0) %>%
  filter(dest == "BOS" | dest == "BTV") %>%
  group_by(dest) %>%
  summarize(avg.delay = mean(dep_delay,
                             na.rm = TRUE))

#2)
flights %>%
  filter(dest == "BOS" | dest == "BTV") %>%
  mutate(is.delayed = ifelse(dep_delay > 0,
                             1,
                             0)) %>%
  group_by(dest) %>%
  summarize(prop.delayed = sum(is.delayed,
                               na.rm = TRUE)/n())

#Another way to approach this
flights %>%
  filter(dest == "BOS" | dest == "BTV") %>%
  mutate(is.delayed = ifelse(dep_delay > 0,
                             1,
                             0)) %>%
  group_by(dest) %>%
  summarize(prop.delayed = mean(is.delayed,
                                na.rm = TRUE))


#Make graphs in R using the  ggplot2() package
library(ggplot2)

#Let's make a scatterplot of the carat of a diamond
# versus its price

diamonds %>%
  ggplot(mapping = aes(x = carat,
                       y = price)) +
  geom_point()

#Say I want to color my points thistle
diamonds %>%
  ggplot(mapping = aes(x = carat,
                       y = price)) +
  geom_point(aes(color = color))

#Say I want a DIFFERENT color scheme
diamonds %>%
  ggplot(mapping = aes(x = carat,
                       y = price)) +
  geom_point(aes(color = color)) +
  scale_color_brewer(palette = "Blues")

diamonds %>%
  ggplot(mapping = aes(x = carat,
                       y = price)) +
  geom_point(aes(color = color)) +
  scale_color_manual(values = c("red",
                                "orange",
                                "yellow",
                                "green",
                                "blue",
                                "purple",
                                "pink"))

## Class Notes 2/22/19

# Graph 1
flights %>%
  ggplot(aes(x = origin,
             y = sched_arr_time)) +
  geom_violin(aes(fill = origin))

# Graph 2
flights %>%
  filter(!is.na(dep_delay)) %>%
  mutate(was.flight.delayed = ifelse(dep_delay > 0,
                                     "delayed",
                                     "not delayed")) %>%
  ggplot() +
  geom_bar(aes(x = carrier,
               fill = was.flight.delayed),
           position = "stack")
  
# Graph 3
flights %>%
  filter(!is.na(dep_delay)) %>%
  mutate(was.flight.delayed = ifelse(dep_delay > 0,
                                     "delayed",
                                     "not delayed")) %>%
  ggplot() +
  geom_bar(aes(x = carrier,
               fill = was.flight.delayed),
           position = "fill")

# Graph 4

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


#Investigate the Reddit Users data set
#install.packages('data.table')
library(data.table)
reddit <- fread(file.choose())

reddit %>%
  head(5)

#Looking at stringr package
library(stringr)

#Which usernames contain the letter "e"?
toy.data <- reddit %>%
  head(5)

str_detect(toy.data$author,
           "e")

#How do I detect e OR E?
str_detect(toy.data$author,
           "e|E")

str_detect(toy.data$author,
           regex("e",
                 ignore_case = TRUE))

#Are there any digits?
str_detect(toy.data$author,
           "[:digit:]")

#Filter by str_detect()
toy.data %>%
  filter(str_detect(toy.data$author,
                    "[:digit:]"))



#Full data
#What proportion of users have usernames with a digit?

reddit %>%
  mutate(has.digit = ifelse(str_detect(reddit$author,
                                       "[:digit:]"),
                            1,
                            0)) %>%
  summarize(prop = mean(has.digit))



## Class Notes 2/25/19



#Looking at the str_count() function
#Say I want calculate the AVERAGE number of digits per username

str_count(toy.data$author,
          "[:digit:]")

reddit.digits <- reddit %>%
  mutate(num.digits = str_count(author,
                                "[:digit:]")) 

reddit.digits %>%
  summarize(avg = mean(num.digits))

#What's the average number of digits used in reddit usernames
#THAT CONTAIN digits.

reddit.digits %>%
  filter(num.digits >= 1) %>%
  summarize(avg = mean(num.digits))


#Let's plot the DISTRIBUTION of number of digits used
reddit.digits %>%
  filter(num.digits >= 1) %>%
  ggplot(aes(x = num.digits)) +
  geom_bar()

#How many usernames END with the number 99?
#The username "Alex99" ends with 99.
#The username "Alex999" does NOT end with 99.
#The username "Alex899" does NOT end with 99.


test.vector <- c("Alex99", "Alex999", "Bob8")

str_detect(test.vector,
           "[:alpha:]99$|^99$|[:punct:]99$")

reddit %>%
  filter(str_detect(author,
                    "[:alpha:]99$|^99$|[:punct:]99$")) %>%
  summarize(total = n())

## Introducing for loops in R

new.vector <- NULL

for(i in 1:100){
  new.vector[i] <- 2*i
}

new.vector

#What is the distribution of digits it reddit usernames?

#How many 0s are there?
sum(str_count(reddit$author,
              "0"))

#How many 0s are there?
sum(str_count(reddit$author,
              "1"))

count.vector <- NULL

for(i in 0:9){
  count.vector[i+1] <- sum(str_count(reddit$author,
                                   toString(i)))
  print(i)
}



## Class Notes 2/27/19

count.vector

digits.data <- data.frame(counts = count.vector,
                          digit = 0:9)

digits.data %>%
  ggplot(aes(x = factor(digit), #treat digit as discrete, categorical
             y = counts)) +
  geom_bar(stat = "identity")


### What is the distribution of numbers that 
### PREPEND usernames?

# For example, 27Alex, 27 prepends the username
# For example, 3Alex56, 3 prepends the username
# For example, Alex has no prepending number

toy.vector <- c("27Alex", "3Alex56", "Alex", "Alex56Alex")


# str_locate() function which locates the first
# instance of a given string.

str_locate(toy.vector, "A")
str_locate(toy.vector, "Alex")

#Locate first example of a number followed by a non-number
str_locate(toy.vector,
           "[:digit:]([:alpha:]|[:punct:])")


#str_sub() function, which subsets a string
location.of.last.digit <- str_locate(toy.vector,
           "[:digit:]([:alpha:]|[:punct:])")[ ,1]

?str_sub

str_sub(toy.vector,
        start = 1,
        end = location.of.last.digit)

#Let's filter our data for only usernames beginning with
# digits
digit.reddit <- reddit %>%
  filter(str_detect(reddit$author,
                    "^[:digit:]"))

location.of.last.digit <- str_locate(digit.reddit$author,
                                     "[:digit:]([:alpha:]|[:punct:])")[ ,1]

prepending.numbers <- str_sub(digit.reddit$author,
                              start = 1,
                              end = ifelse(is.na(location.of.last.digit),
                                           nchar(digit.reddit$author),
                                           location.of.last.digit))

#Look at the first 10 elements of our prepending numbers
prepending.numbers[1:14]

#First, get our prepending numbers into a data set

prepend.data <- data.frame(numbers = as.numeric(prepending.numbers))

prepend.data %>%
  ggplot(aes(x = numbers)) +
  geom_histogram() +
  xlim(0,1000) +
  ylim(0,30000)


prepend.data %>%
  ggplot(aes(x = numbers)) +
  geom_bar() +
  xlim(0,10)

#Now calculate the distribution of numbers that
# APPEND (come at the end of) usernames.



## Class Notes 3/1/19

set.seed(10)
sample.reddit <- reddit %>%
  sample_n(100)

#Identify usernames with number at the end
sample.reddit %>%
  filter(str_detect(author,
                    "[:digit:]{1,}$"))

#Find location of first digit and the end of the username
location <- str_locate(sample.reddit$author,
           "([:alpha:]|[:punct:])[:digit:]{1,}$")[,1] + 1

#Extract the numbers at the end of usernames
str_sub(sample.reddit$author,
        start = location,
        end = nchar(sample.reddit$author))


#Now, on to web scraping!
#Web scraping: Something exists on the web, and I want it.
#install.packages('rvest')
library(rvest)

brady.url <- "https://en.wikipedia.org/wiki/Tom_Brady"

#Our scraping syntax looks like this:

brady.table <- brady.url %>%
  read_html() %>%
  html_nodes("table") %>%
  .[[6]] %>%
  html_table(fill = TRUE)

View(brady.table)

new.brady <- brady.table

#Make new column names for brady.table
colnames(new.brady) <- new.brady[1, ]
nrow(new.brady)
ncol(new.brady)

new.brady <- new.brady[-c(1,21), -23]

new.brady %>%
  ggplot(aes(x = Year,
             y = Yds)) +
  geom_point()

#We need to convert yards to numeric
#apply()
new.brady$Yds <- as.numeric(str_replace_all(new.brady$Yds,
                ",",
                ""))

new.brady %>%
  ggplot(aes(x = Year,
             y = Yds)) +
  geom_point()


#Let's try scraping text

sb.url <- "https://en.wikipedia.org/wiki/Super_Bowl_XLII"

sb.text <- sb.url %>%
  read_html() %>%
  html_nodes("p") %>%
  html_text()

combined.text <- str_c(sb.text,
      sep = "AAAAAAA",
      collapse = "")




### Class Notes 3/4/19

library(shiny)


## Class Notes 3/8/19
library(leaflet)


#Build a leaflet (map)
leaflet() %>%
  addTiles() %>%
  addMarkers(lng=-73.1755, lat = 44.01005, popup = "Warner")


#Scrape volcano data
volcano.url <- "https://www.ngdc.noaa.gov/nndc/struts/results?type_0=Like&query_0=&op_8=eq&v_8=&type_10=EXACT&query_10=None+Selected&le_2=&ge_3=&le_3=&ge_2=&op_5=eq&v_5=&op_6=eq&v_6=&op_7=eq&v_7=&t=102557&s=5&d=5"

volcano.data <- volcano.url %>%
  read_html() %>%
  html_nodes("table") %>%
  .[[3]] %>%
  html_table()



#Plot locations of all volcanos on my leaflet
volcano.data %>%
  leaflet() %>%
  addTiles() %>%
  addMarkers(clusterOptions = markerClusterOptions(),
             lat = ~Latitude,
             lng = ~Longitude,
             popup = ~Country,
             label = ~Region)



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






### Class Notes 3/11/19

#Learning to make choropleths
library(geojsonio)

states.url <- "http://eric.clst.org/assets/wiki/uploads/Stuff/gz_2010_us_040_00_5m.json"

states <- geojson_read(states.url, what = "sp")

#Look at my data
View(states@data)

#Look at my map

map <- states %>%
  leaflet() %>%
  addTiles() %>%
  setView(-96, 37.8, 4)

#Now, let's color each state based on its size

states@data %>%
  ggplot(aes(x = CENSUSAREA)) +
  geom_histogram(bins = 100) +
  xlim(0,200000)

bins <- c(0, 25000, 50000, 75000, 100000, 125000, 150000, Inf)
colors <- colorBin(palette = "YlOrRd",
                   domain = states@data$CENSUSAREA,
                   bins = bins)

#Now, color our states!

map %>%
  addPolygons(fillColor = ~colors(CENSUSAREA),
              weight = 2,
              opacity = 1,
              color = "white",
              dashArray = "3",
              fillOpacity = 0.7)

#Scrape in state pop data

pop.url <- "https://en.wikipedia.org/wiki/List_of_states_and_territories_of_the_United_States_by_population"

pop.data <- pop.url %>%
  read_html() %>%
  html_nodes("table") %>%
  .[[3]] %>%
  html_table()

#Remove rows at end of data
pop.data.clean <- pop.data[-c(66:nrow(pop.data)), ]

#Remove all commas in numbers
colnames(pop.data.clean)[3] <- "Population"
pop.data.clean$Population <- str_replace_all(pop.data.clean$Population,
                                             ",",
                                             "")

#Convert these to numbers
pop.data.clean$Population <- as.numeric(pop.data.clean$Population)
  
#Rename column 1
colnames(pop.data.clean)[1] <- "NAME"

pop.data.clean <- pop.data.clean[ , c(1,3)]


#Join the data sets

states@data <- left_join(states@data,
                         pop.data.clean,
                         by = "NAME")
View(states@data)

#Color by population
bins <- c(0, 1000000, 3000000, 5000000, 15000000, Inf)
colors <- colorBin(palette = "YlOrRd",
                   domain = states@data$Population,
                   bins = bins)

#Now, color our states!

map %>%
  addPolygons(fillColor = ~colors(states@data$Population),
              weight = 2,
              opacity = 1,
              color = "white",
              dashArray = "3",
              fillOpacity = 0.7)





## Class 3/18/19

peppermint <- fread(file.choose())

peppermint %>%
  ggplot(aes(x = factor(Peppermint),
             y = Score)) +
  geom_boxplot(aes(fill = factor(Test)))

peppermint %>%
  ggplot(aes(x = factor(Test),
             y = Score)) +
  geom_boxplot(aes(fill = factor(Peppermint)))

peppermint %>%
  ggplot(aes(x = factor(StudentID),
             y = Score)) +
  geom_point(aes(color = factor(Peppermint)),
             size = 5)

#Read in life expectancy data
life.data <- read_csv(file.choose())

#Convert from wide format to long format
#Introduce the gather() function

life.gathered <- life.data %>%
  gather(key = "year",
         value = "lifeExpectancy",
         -country)


#Let's graph the life expectancy over time for all countries

life.gathered$year <- as.numeric(life.gathered$year)


life.gathered %>%
  filter(country == "Indonesia" | country == "Iran" | country == "Iraq") %>%
  ggplot(aes(x = year,
             y = lifeExpectancy)) +
  geom_line(aes(color = country),
            size = 3)



#Read in income data
income.data <- read_csv(file.choose())

#Convert from wide format to long format
#Introduce the gather() function

income.gathered <- income.data %>%
  gather(key = "year",
         value = "gdp",
         -country)

#Let's graph the life expectancy over time for all countries

income.gathered$year <- as.numeric(income.gathered$year)

income.gathered %>%
  filter(country == "Indonesia" | country == "Iran" | country == "Iraq") %>%
  ggplot(aes(x = year,
             y = gdp)) +
  geom_line(aes(color = country),
            size = 3)


#Combine our data sets using a join
#left_join()





## Class Notes 3/20/19


#Using left_join()

joined.data <- left_join(life.gathered,
                         income.gathered,
                         by = c("country", "year"))

# joined.data %>%
#   filter(is.na(lifeExpectancy))



#Let's recreate a single frame from the video on Day 1
#Specifically, let's look at the year 1991

joined.data %>%
  filter(year == 1991) %>%
  ggplot(aes(x = gdp,
             y = lifeExpectancy)) +
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
             y = lifeExpectancy)) +
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
             y = lifeExpectancy)) +
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


