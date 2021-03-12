# Week 8. Function application
# AB
# 12 March 2021

library(tidyverse)

df <- read_tsv("UKDA-6614-tab/tab/ukhls_wx/xwavedat.tab")

# How to apply a function to a number of objects.
# calculate the proportion of missing values in a variable.

# In the original UndSoc data set the code for missing value is -9. 

# let's write a function to do this.

propMissing <- function(x) {
  sum(x == -9) / length(x)
}

# test
xx <- c(1:4, -9)
propMissing(xx)

colnames(df)[1:20]

# id number
propMissing(df$pidp)
# sex
propMissing(df$sex_dv)
# birthweight
propMissing(df$bwtlb)

# 1. Write a loop.

output <- numeric(dim(df)[2])

for (i in 1:dim(df)[2]) {
  output[i] <- propMissing(df[[i]])
}
output

# 2. A more elegant solution is to apply a function to the columns of df

# Base R

apply(df, 2, propMissing)

# see also tapply, lapply, sapply, mapply

# purrr from tidyverse

map(df, propMissing)

map_dbl(df, propMissing)

? map
? walk
? pmap

# ch. 21 from R for Data Science



