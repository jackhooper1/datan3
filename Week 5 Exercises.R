## Week 5 Exercises
library(tidyverse)
library(ggplot2)
## 12.2.1
#1 table1 - Each row = country/yr for cases and population
#  table2 - each row =country/yr/variable and then value of count of variable
#  table3 - Each row = Country/yr for rate of cases-population
#  table 4a - Each row = Country then column for cases in each year
#  table4b - Each row = Country then column for population in each year

table1
table2
table3
table4a
table4b

#2

tb2 <- table2 %>%
  pivot_wider(names_from=type, values_from=count) %>%
  mutate(rate = cases*10000/population) %>%
  pivot_wider(names_from=year, values_from = c("cases","population","rate"))
tb2

tb4 <- table4a %>%
  full_join(table4b, by = "country") %>%
  mutate(rate.1999 = `1999.x`*10000/`1999.y`) %>%
  mutate(rate.2000 = `2000.x`*10000/`2000.y`)
tb4  

#3 
tb2plot <- table2 %>%
  pivot_wider(names_from=type, values_from=count) %>%
  mutate(rate = cases*10000/population)

ggplot(tb2plot, aes(year, cases)) + 
  geom_line(aes(group = country), colour = "grey50") + 
  geom_point(aes(colour = country))

#OR

table2 %>%
  filter(type == "cases") %>%
  ggplot(aes(year, count)) +
  geom_line(aes(group = country), colour = "grey50") +
  geom_point(aes(colour = country)) +
  scale_x_continuous(breaks = unique(table2$year)) +
  ylab("cases")

#12.3.3
# 1
#wider uses the key
# if the key is not a unique identifier e.g. people with the same name, it is 
# difficult. Could add: mutate(obs=row_number())

#2 Because the columns have this ``

#12.4.3
# arguments within separate

# extra = what to do if too many pieces of data:
# default = drop extra bits
# "merge" = put both values in same column

# fill = what to do if there aren't enough pieces of data:
# default is add NA
# can choose from the left or right
