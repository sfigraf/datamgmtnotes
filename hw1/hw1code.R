install.packages("openintro")
library(ggplot2)
library(tidyverse)
library(openintro)
marioKart
colnames(marioKart)
#what is the least expensive mariokart game that included at least 1 steering wheel in the ebay auction?
data <- marioKart %>%
  mutate(has.wheels = ifelse(wheels > 0, 
                              1,
                              0)) %>%
  filter(has.wheels >0) %>%
  arrange(totalPr)

View(data)  
  
#Q2
#which shipping speed/method had the largest range of shipping prices, and which had the smallest range?
colnames(marioKart)
data <- marioKart %>%
  group_by(shipSp) %>%
  summarize(max.price = max(shipPr),
            min.price = min(shipPr)) %>%
  mutate(range = max.price - min.price)

data

#q3
data <- marioKart %>%
  group_by(cond) %>%
  summarize(mean.price = mean(totalPr),
             median.price = median(totalPr))
data

#q4

data <- marioKart %>%
  group_by(cond) %>%
  summarize(max.price = max(totalPr),
            median.price = median(totalPr))
data
marioKart %>%
  ggplot(mapping = aes(x = cond,
             y = totalPr)) +
  geom_boxplot(aes(fill = cond))

#could use a box and whisker plot for this, or double bar ocmparing medians to the maxes.  
#q5
data <-marioKart %>%
  arrange(-totalPr) %>%
  filter(totalPr < 76) %>%
  group_by(cond) %>%
  summarize(max.price = max(totalPr),
            median.price = median(totalPr))
  
data

#Q6
#median total price for each combination of game condition and whether or not the auction contanied any number of steering wheels
data <- marioKart %>%
  mutate(has.wheels = ifelse(wheels > 0, 
                             1,
                             0)) %>%
  group_by(has.wheels, cond) %>%
  summarize(median.price = median(totalPr))
data

#q7
#porportion of new and used games I'd be able to purchase with $50
data <- marioKart %>%
  group_by(cond) %>%
  mutate(below_50 = ifelse(totalPr <50, 
                           1,
                           0)) %>%
  mutate(totalnumbers = ifelse(totalPr >= 0, 1,0)) %>%
  summarize(sum.below_50 = sum(below_50), #gets sums of those columns
            sum.totalnumbers = sum(totalnumbers))

data$porps <- data$sum.below_50 / data$sum.totalnumbers
data

#this method isn't working...why?
data <- marioKart %>%
  mutate(below_50 = ifelse(totalPr <50, 
                           1,
                           0)) %>%
  group_by(cond) %>%
  summarize(prop.afford = mean(below_50)/n())
data

#q8
marioKart
marioKart %>%
  ggplot(aes(x= startPr,
             y = totalPr)) +
  geom_point(aes(color = cond, shape = stockPhoto, size = 2))+
  facet_grid(vars(cond), vars(stockPhoto))
?facet_grid
?geom_point

#q9
#condition vs price
marioKart %>%
  group_by(cond) %>%
  summarize(median.total.price = median(totalPr)) %>%
  ggplot(aes(x= cond, median.total.price,
             
             y = median.total.price)) +
  geom_col(aes(fill = cond)) #geom_col not geom bar; "stat_count() must not be used with a y aesthetic." error is gotten from not using geom_col
#shipping speed vs price
marioKart %>%
  group_by(shipSp) %>%
  summarize(median.total.price = median(totalPr)) %>%
  ggplot(aes(x= shipSp, median.total.price,
             
             y = median.total.price)) +
  geom_col(aes(fill = shipSp))

#shipping cost vs price
data <- marioKart[-20,] #makes a copy of mariokart without row 20 (the large outlier)
data
View(data)
data %>%
  
  ggplot(aes(x= shipPr,
             y = totalPr)) +
  geom_point()
#number of wheels
marioKart %>%
  group_by(wheels) %>%
  summarize(median.total.price = median(totalPr)) %>%
  ggplot(aes(x= wheels, median.total.price,
             
             y = median.total.price)) +
  geom_col(aes(fill = wheels))
#nBids vs price
marioKart %>%
  
  ggplot(aes(x= nBids,
             y = totalPr)) +
  geom_point() 
  #xlim(0, 10000)
#stock photo
marioKart %>%
  group_by(stockPhoto) %>%
  summarize(median.total.price = median(totalPr)) %>%
  ggplot(aes(x= stockPhoto, median.total.price,
             
             y = median.total.price)) +
  geom_col(aes(fill = stockPhoto))

