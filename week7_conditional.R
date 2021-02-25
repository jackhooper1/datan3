# Week 7. Conditional structures
# AB
# 25 Feb 2021

library(tidyverse)

# you're already familiar with vectorised ifelse() that we used for recoding variables

# ifelse(test, yes, no)

df <- tibble(x = 1:3)
df

df %>%
  mutate(y = ifelse(x > 1, 1, 0))


# if ... else 
# we used it for a loop to read all data at once
# it is used to control flow in loops / functions

? if

# simple example

x <- 1

if (x == 1) {
  print("x = 1")
} else {
  print("x != 1")
}

# if ... else is not vectorised

df$x

if(df$x == 1) {
  print("x == 1")
}


# something more complex (nested if ... else)

x <- -1

if (x == 1) {
  y <- 2
} else if (x == 2) {
  y <- 4
} else {
  y <- 0
}
print(y)










