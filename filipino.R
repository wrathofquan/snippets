library(tidyverse)
library(readxl)
library(refinr)
library(ggthemes)
library(magrittr)

data <- read_excel("LiladharFilipinoAnalysis04272019.xlsx")


data$Year <- gsub("\\[|\\]|?", "", data$Year)

data$Subject <- n_gram_merge(data$Subject)


data$Year <- as.numeric(data$Year)
data$Language <- as_factor(data$Language)
data$Subject <- as.character(data$Subject)
data$Copies <- as_factor(data$Copies)



data %>%  group_by(Language, Subject, Copies) %>% summarize(n = n()) %>%  na.omit() %>%  
  ggplot(aes(x = reorder(Language, n), y = n, fill = Copies)) +
  geom_bar(stat = "identity") + coord_flip()  
  


# 1. How are the publications distributed by the years of publication? Do you think, a 10 year or a 25-year interval might be one way to look at all these years? I mean collapsing the data points for years?

data %>% filter(Year != "NA") %>% mutate(decade = (floor(Year/10)*10)) %>% 
  group_by(decade) %>% tally() %>%  
  ggplot(aes(x = decade, y = n)) + geom_line() +
  ggthemes::theme_fivethirtyeight() + ggtitle("Publications by Year 1593 - 1749") +
  geom_label(aes(label = n)) + geom_text(aes(x = 1730, y = 16, label = "Some annotation"), family = "Helvetica") 




# 2. Data visualization using North America as a place of holdings? I have created a separate column for the libraries of North America. 
#Out of 118, there are 22 items that are held in our country and Mexico. 
# 
data %>%  group_by(`Copies in North America`, Language) %>% summarize(n = n()) %>%  na.omit() %>%  
  ggplot(aes(x = reorder(`Copies in North America`, n), y = n)) +
  geom_bar(stat = "identity") + coord_flip()  + ggthemes::theme_fivethirtyeight()


northAmerica <- data %>%  
  group_by(`Copies in North America`, Language) %>% 
  summarize(n = n()) %>%  
  na.omit() 

library(ggmap)
library(maps)

register_google(key = 'AIzaSyAM32dJF8yS2qzbERIzGYZphL5mpVmYUIc')

locations <- northAmerica$`Copies in North America`

world <- map_data("world")

locations %>% geocode()


mutate_geocode(northAmerica, `Copies in North America` )



# 3. Representing the non-Spanish languages of publication by the years of publication?

data %>% filter(Year != "NA" ) %>% filter(str_detect(Language, "Spanish", negate = TRUE)) %>% 
 mutate(decade = (floor(Year/10)*10)) %>% 
  group_by(Language, decade) %>% tally() %>%  
  ggplot(aes(x = decade, y = n) ) + geom_line() +
  ggthemes::theme_fivethirtyeight() + ggtitle("Non-Spanish Publications 1593 - 1749")
  

data %>% filter(Language != "NA") %>% group_by(Year, Language) %>% summarize(n = n()) %>% 
  mutate(decade = (floor(Year/10)*10)) %>% 
  ggplot(aes(x = decade, y = n, by = Language)) + geom_point() +
  ggthemes::theme_fivethirtyeight() + ggtitle("Non-Spanish Publications 1593 - 1749")
  
#  4. And lastly, visualizing the subjects of these publications?

library(ggwordcloud)


data %>%  group_by(Subject) %>% summarize(n = n()) %>% head(n = 15) %>% 
  ggplot(aes(x = reorder(Subject, n), y = n)) +
  geom_col() + coord_flip() + xlab("blah")


data %>% group_by(Subject) %>% summarize(n = n()) %>% head(n = 15) %>% set.seed(42) %>%  
  ggplot(aes(label = Subject, size = n)) + 
  geom_text_wordcloud() +
  scale_size_area(max_size = 20) +
  theme_minimal()



