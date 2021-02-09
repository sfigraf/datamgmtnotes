library(tidyverse)
library(stringr)
library(data.table)
library(ggplot2)
library(leaflet)
library(rvest)

data <- read_csv("moviedata.csv")
#genres
genres <- data %>%
  mutate(is.comedy = str_detect(genres, "Comedy"),
         is.documentary = str_detect(genres, "Documentary"),
         is.thriller = str_detect(genres, "Thriller"),
         is.action = str_detect(genres, "Action"),
         is.western = str_detect(genres, "Western"),
         is.horror = str_detect(genres, "Horror"),
         is.drama = str_detect(genres, "Drama"),
         is.animation = str_detect(genres, "Animation"),
         is.crime = str_detect(genres, "Crime"),
         is.adventure = str_detect(genres, "Adventure"),
         is.music = str_detect(genres, "Music"),
         is.romance = str_detect(genres, "Romance"),
         is.fantasy = str_detect(genres, "Fantasy"),
         is.mystery = str_detect(genres, "Mystery"),
         is.sci.fi = str_detect(genres, "Science Fiction"),
         is.war = str_detect(genres, "War"),
         is.history = str_detect(genres, "History"))
#adding production countries
production_countries <- genres %>%
  mutate(in.us = str_detect(production_countries, "United States of America"),
         in.uk = str_detect(production_countries, "United Kingdom"),
         in.japan = str_detect(production_countries, "Japan"),
         in.sweden = str_detect(production_countries, "Sweden"),
         in.ireland = str_detect(production_countries, "Ireland"),
         in.canada = str_detect(production_countries, "Canada"),
         in.iran = str_detect(production_countries, "Iran"),
         in.germany = str_detect(production_countries, "Germany"),
         in.india = str_detect(production_countries, "India"),
         in.france = str_detect(production_countries, "France"),
         in.china = str_detect(production_countries, "China"),
         in.australia = str_detect(production_countries, "Australia"),
         in.spain = str_detect(production_countries, "Spain"),
         in.russia = str_detect(production_countries, "Russia"),
         in.iceland = str_detect(production_countries, "Iceland"),
         in.austria = str_detect(production_countries, "Austria"),
         in.italy = str_detect(production_countries, "Italy"),
         in.greece = str_detect(production_countries, "Greece"),
         in.phillipines = str_detect(production_countries, "Philippines"),
         in.denmark = str_detect(production_countries, "Denmark"),
         in.Chile = str_detect(production_countries, "Chile"),
         in.brazil = str_detect(production_countries, "Brazil"),
         in.mongolia = str_detect(production_countries, "Mongolia"),
         in.mexico = str_detect(production_countries, "Mexico"),
         in.argentina = str_detect(production_countries, "Argentina"))

         
#keeps relevant columns         
columns <- production_countries %>%
  select(-id,
         -belongs_to_collection,
         -genres,
         -homepage,
         -imdb_id,
         -original_title,
         -overview,
         -poster_path,
         -production_companies,
         -production_countries,
         -spoken_languages,
         -status,
         -tagline,
         -title,
         -Keywords,
         -cast,
         -crew) %>%
  na.omit()

columns %>%
  ggplot(aes(x = budget,
             y = revenue)) +
  geom_point()



columns %>%
  ggplot(aes(x = budget,
             y = revenue)) +
  geom_point()

columns.kmeans <- kmeans(na.omit(columns, 3))
columns %>%
  mutate(cluster = columns.kmeans$cluster) %>%
  ggplot(aes(x = budget,
             y = revenue)) +
  geom_point(aes(color = factor(cluster)))

#test
testcols <- columns %>%
  select(-original_language,
         -release_date)
test.scaled <- scale(testcols)
dist.matrix <- dist(test.scaled)
clustering1 <- hclust(dist.matrix)
plot(clustering1)

sum.test <- testcols %>%
  group_by(is.comedy) %>%
  summarize(mean.revenue = mean(revenue))

sum.test %>%
  ggplot(aes_all(x = sum.test$mean.revenue,
                 y = mean.revenue)) +
  geom_point()
library(tidyverse)
