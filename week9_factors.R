# Week 9. Factors
# AB
# 12 March 2021

library(tidyverse)

# Factors in R are useful for working with categorical data.

# Factors are vectors with levels.

df <- read_tsv("UKDA-6614-tab/tab/ukhls_wx/xwavedat.tab")

colnames(df)

# ethnicity

df %>%
  count(ethn_dv)

class(df$ethn_dv)

df <- df %>%
  mutate(ethnNew = case_when(
    ethn_dv == 1 ~ "White British",
    ethn_dv == 2 ~ "Irish",
    ethn_dv == 4 ~ "other White",
    ethn_dv == 9 ~ "Indian",
    ethn_dv == 10 ~ "Pakistani",
    ethn_dv %in% c(3, 5:8, 11:97)  ~ "Other"
  ))

# checking the recoding

df %>%
  count(ethn_dv, ethnNew)

df %>% count(ethnNew)

class(df$ethnNew)

df <- df %>%
  mutate(ethnNew = factor(ethnNew))

class(df$ethnNew)

df %>% count(ethnNew)

# Factor levels

levels(df$ethnNew)

# Changing the reference category.

# Let's regress birth year on ethnicity.

lm(birthy ~ ethnNew, df) %>%
  summary()

# Indian is the reference category, but it is often more comvenient
# to have the largest category as reference.

df %>%
  mutate(ethnNew = relevel(ethnNew, ref = "White British")) %>%
  lm(birthy ~ ethnNew, .) %>% summary()

# Another way of doing this from forcats package.

df %>%
  mutate(ethnNew = fct_relevel(ethnNew, ref = "White British")) %>%
  lm(birthy ~ ethnNew, .) %>% summary()

# You can also re-order the levels of the factor directly.

levels(df$ethnNew) <- c("White British", "Pakistani", "Indian",
                        "Other", "other White", "Irish")

df %>% count(ethnNew)

# The order of levels may also be important for plots,
# and forcats has a number of functions for helping to work
# with factors.
# Please see https://r4ds.had.co.nz/factors.html

