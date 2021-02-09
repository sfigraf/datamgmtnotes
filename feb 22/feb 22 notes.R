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
  
