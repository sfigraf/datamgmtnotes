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



  

