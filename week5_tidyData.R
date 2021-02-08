# Week 5. Tidy data (reshaping)
# AB
# 8 Feb 2021

# Read ch.12 from R for Data Science (Tidy data): https://r4ds.had.co.nz/tidy-data.html

library(tidyverse)
library(vroom)

# simple example

df <- tibble(
  names = c("Jack", "Mary"),
  age1 = c(18, 25),
  age2 = c(19, 26),
  age3 = c(20, 27)
)
df

# these are data in the wide format

df %>%
  pivot_longer(cols = age1:age3,
               names_to = "time",
               values_to = "age")

# these are data in the long format

# Depending on the type of analysis you want to perform, you may want to have data
# both in the wide and long format. Usually the long format is preferred (tidy data).
# 
# pivot_wider(): from long to wide
# pivot_longer(): from wide to long


# Examples from the Understanding Society.

# Open the first three waves of data only.

w1 <- vroom("UKDA-6614-tab/tab/ukhls_w1/a_indresp.tab",
            col_select = c(pidp, a_sex_dv, a_age_dv, a_fimnnet_dv))
w2 <- vroom("UKDA-6614-tab/tab/ukhls_w2/b_indresp.tab",
            col_select = c(pidp, b_sex_dv, b_age_dv, b_fimnnet_dv))
w3 <- vroom("UKDA-6614-tab/tab/ukhls_w3/c_indresp.tab",
            col_select = c(pidp, c_sex_dv, c_age_dv, c_fimnnet_dv))

# Join the data

w1_3 <- w1 %>%
  full_join(w2, by = "pidp") %>%
  full_join(w3, by = "pidp")
w1_3

# these are data in the wide format.

# we can, for example, calculate the difference in income between waves 2 and 1

w1_3 %>%
  mutate(inc_diff_21 = b_fimnnet_dv - a_fimnnet_dv) %>%
  select(pidp, a_fimnnet_dv, b_fimnnet_dv, inc_diff_21)

# transform to the long format

w1_3long <- w1_3 %>%
  pivot_longer(a_sex_dv:c_fimnnet_dv, names_to = "variable", values_to = "value") %>%
  separate(variable, into = c("wave", "variable"), sep = "_", extra = "merge") %>%
  pivot_wider(names_from = variable, values_from = value)

# create variables for income in the previous and subsequent waves
# lag() and lead() are functions from dplyr

w1_3long %>%
  group_by(pidp) %>%
  mutate(lagIncome = lag(fimnnet_dv)) %>%
  mutate(leadIncome = lead(fimnnet_dv))


# we could also create the data set in the long format in a different way,
# binding the rows

# standardising column names across waves
colnames(w3) <- colnames(w2) <- colnames(w1) <- c("pidp", "sex_dv", "age_dv", "fimnnet_dv")
w1 %>%
  bind_rows(w2) %>%
  bind_rows(w3) %>%
  arrange(pidp)

# note that we don't have data for missing waves then






