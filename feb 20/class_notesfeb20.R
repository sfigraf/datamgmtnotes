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

  
