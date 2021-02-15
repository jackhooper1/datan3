# Week 6. Visualising data
# AB
# 15 Feb 2021

library(tidyverse)
library(vroom)

# ggplot2 is the package for data visualisation

# open the file with time constant individual characteristics

df <- vroom("UKDA-6614-tab/tab/ukhls_wx/xwavedat.tab")

# Read ch.3 from R for Data Science: https://r4ds.had.co.nz/data-visualisation.html

# Also see R Graphics Cookbook: https://r-graphics.org

# Bar chart: one categorical variable (factor).

# ukborn: place of birth

df %>% count(ukborn)

df <- df %>%
    mutate(ukbornLabelled = case_when(
      ukborn == 1 ~ "England",
      ukborn == 2 ~ "Scotland",
      ukborn == 3 ~ "Wales",
      ukborn == 4 ~ "N.Ireland",
      ukborn == 5 ~ "not UK"
    )) 

df %>% count(ukbornLabelled)

df %>%
  ggplot(aes(x = ukbornLabelled)) +
  geom_bar()

# Nicer design.

df %>%
  filter(!is.na(ukbornLabelled)) %>%
  ggplot(aes(x = fct_infreq(ukbornLabelled))) +
  geom_bar() +
  xlab("Country of birth") +
  ylab("count")

# Percentages instead of counts.

df %>%
  filter(!is.na(ukbornLabelled)) %>%
  ggplot(aes(x = fct_infreq(ukbornLabelled))) +
  geom_bar(aes(y = (..count..) / sum(..count..) * 100)) +
  xlab("Country of birth") +
  ylab("Percent")

# another way is summarising the data first.

df %>%
  filter(!is.na(ukbornLabelled)) %>%
  count(ukbornLabelled) %>%
  mutate(perc = n / sum(n) * 100) %>%
  ggplot(aes(x = fct_reorder(ukbornLabelled, perc), y = perc)) +
  geom_col()

# other geoms

# a scatter plot: two quantitative variables.

# wave 10 data

w10 <- vroom("UKDA-6614-tab/tab/ukhls_w10/j_indresp.tab")

w10 %>% count(j_age_dv)
w10 %>% count(j_fimnnet_dv)

w10 %>%
  # take a sample of 100 individuals
  slice_sample(n = 100) %>%
  ggplot(aes(x = j_age_dv, y = j_fimnnet_dv)) +
  geom_point()

# add a smooth spline

w10 %>%
  slice_sample(n = 100) %>%
  ggplot(aes(x = j_age_dv, y = j_fimnnet_dv)) +
  geom_point() +
  geom_smooth() +
  geom_smooth(aes(colour="red"), method = "lm")

# Faceted charts

# age/income relationship by region

# region

w10 %>% count(j_gor_dv)

w10 %>%
  filter(j_age_dv > 16) %>%
  ggplot(aes(x = j_age_dv, y = j_fimnnet_dv)) +
  geom_smooth()

w10 %>%
  filter(j_age_dv > 16) %>%
  filter(j_gor_dv > 0) %>%
  ggplot(aes(x = j_age_dv, y = j_fimnnet_dv)) +
  geom_smooth() +
  facet_wrap(~ j_gor_dv)









