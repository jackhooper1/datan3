# Week 10. Text data
# AB
# 29 March 2021

library(tidyverse)
library(textdata)

df <- dataset_ag_news()

df

# stringr is a package for working with strings that is part of tidyverse 

# select all news titles containing the word "oil"

df %>%
  filter(str_detect(title, "oil"))

# compare the proportion of news titles containing the word "oil" across classes

df %>%
  count(class)

df %>%
  mutate(oil = str_detect(title, "oil")) %>%
  group_by(class) %>%
  summarise(propOil = mean(oil))

# count the average number of words in news titles

df <- df %>%
  mutate(n = str_count(title, "\\S+")) %>%
  mutate(n1 = str_count(title, " ") + 1)

df

# count the average number of characters in news titles

df %>%
  mutate(nCharacters = str_length(title))

