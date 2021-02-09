library(tidyverse)
library(dplyr)
library(data.table)
library(leaflet)
library(rvest)
library(geojsonio)


president.data <- fread(file.choose())
View(president.data)
x <- president.data %>%
  filter(year == "2016")
View(x)

states_url <- "http://eric.clst.org/assets/wiki/uploads/Stuff/gz_2010_us_040_00_5m.json"

states <- geojson_read(states_url, what = "sp")

map<-president.data %>%
  leaflet() %>%
  addTiles() %>%
  setView(-96, 37.8, 4) %>%
  addMarkers(clusterOptions = markerClusterOptions(),
             popup = ~state,
             label = ~candidate)
?addMarkers


president.data <- read.csv("~/1976-2016-president.csv")

president.data <- president.data %>%
  mutate(percent.vote = (candidatevotes/totalvotes)*100) %>%
  filter(percent.vote > 1) ##we filter by candidates that recieve over 1% of total votes

president.data.clean <- president.data[ ,-c(4,5,6,10,13,14)]
View(president.data)

for i in 0:nrow(state) {
  [i] == [i + 1]
}
x <- president.data %>%
  group_by(state, year) %>%
  summarize(won = ifelse(percent.vote == max(percent.vote),
            1,
            0))
View(x)
lats_longs <- fread(file.choose())
View(lats_longs)
View(president.data.clean)
joined.data <- left_join(president.data.clean,
                         lats_longs,
                         by = c("state", "year"))
?left_join
