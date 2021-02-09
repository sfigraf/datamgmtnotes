library(tidyverse)

install.packages("babynames")
library(babynames)
library(stringr)
library(data.table)
?babynames
View(babynames)
#what are the ten most common babynames for us babues
names <-babynames
colnames(babynames)
x <- str_count(babynames$name, "Joe")
sum(x)
table
?str_count
#q1
pop_names <- babynames %>%
  group_by(name) %>%
  summarize(x = sum(n)) %>%
  arrange(-x)
pop_names[1:10,]
#q2
babynames %>%
  group_by(year) %>%
  summarize(births = sum(n)) %>%
  ggplot(aes(x = year,
         y = births)) +
  geom_col()

#q3
a <- str_detect(babynames$name, "a")

A_names <- babynames %>%
  mutate(a = (str_detect(babynames$name, "a|A"))) %>%
  mutate(a1 = ifelse(a == TRUE, 
         1,
         0)) %>%
  summarize(sumA = sum(a1))

E_names <- babynames %>%
  mutate(E = (str_detect(babynames$name, "e|E"))) %>%
  mutate(e1 = ifelse(E == TRUE, 
                     1,
                     0)) %>%
  summarize(sume = sum(e1))

I_names <- babynames %>%
  mutate(I = (str_detect(babynames$name, "i|I"))) %>%
  mutate(i1 = ifelse(I == TRUE, 
                     1,
                     0)) %>%
  summarize(sumi = sum(i1))

O_names <- babynames %>%
  mutate(o = (str_detect(babynames$name, "o|O"))) %>%
  mutate(o1 = ifelse(o == TRUE, 
                     1,
                     0)) %>%
  summarize(sumo = sum(o1))

U_names <- babynames %>%
  mutate(u = (str_detect(babynames$name, "u|U"))) %>%
  mutate(u1 = ifelse(u == TRUE, 
                     1,
                     0)) %>%
  summarize(sumu = sum(u1))
A_names[[1,1]]
data = c(A_names[[1,1]], E_names[[1,1]], I_names[[1,1]], O_names[[1,1]], U_names[[1,1]])
df <- data.frame("vowel" = c("A", "E", "I", "O", "U"), "count" = data)
df %>%
  ggplot(aes(x = vowel,
             y = count)) +
  geom_col()
  
#q4
test.vector <- c("Pete", "Gerald", "Juliet")
str_length(test.vector)

babynames %>%
  group_by(name) %>%
  summarize(number_of_names = sum(n)) %>%
  filter(number_of_names >1000) %>%
  mutate(name_length = str_length(name)) %>%
  arrange(-name_length)
  
  
#q5
babynames %>%
  mutate(length = str_length(babynames$name)) %>%
  group_by(year) %>%
  summarize(total_people = sum(n), total_lengths = sum(length * n)) %>%
  mutate(avg.length = total_lengths / total_people) %>%
  ggplot(aes(x = year,
             y = avg.length)) +
  geom_line()

#q6
toy.vector <- c("Asa", "Sam", "Noah", "Evan", "asa", "LUlga")
babynames %>%
  mutate(begins_with_vowel = str_detect(name, "^A|^E|^I|^O|^U")) %>%
  mutate(begins.vowel = ifelse(begins_with_vowel == TRUE,
                               n,
                               0)
  ) %>%
  mutate(ends_with_vowel = str_detect(name, "a$|e$|i$|o$|u$")) %>%
  mutate(ends.vowel = ifelse(ends_with_vowel == TRUE,
                               n,
                               0)) %>%
  group_by(year) %>%
  summarize(births = sum(n), vowel.start = sum(begins.vowel), vowel.end = sum(ends.vowel)) %>%
  mutate(proportion.with.vowelstart = vowel.start / births, proportion.with.vowelend = vowel.end / births)%>%
  gather(proportion.with.vowelstart, proportion.with.vowelend, key = vowelLoc, value = proportion) %>%
  ggplot(aes(x = year,
             y = proportion)) +
  geom_line(aes(color = vowelLoc))



  
