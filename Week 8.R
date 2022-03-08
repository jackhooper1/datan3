library(tidyverse)
library(vroom)

## Reports
# Exact words don't worry - reasonable
# 2000 words
# Outcome different to last year
# make the data tidy data
# can do something different, just has to include different levels and test 
# skills - does have to be Understanding Society dataset
# no more than 5 tables/graphs
# tell the story
# add narrative
# not about modelling - but you can do it
# confidence intervals, standard error for mean etc. important
# measures of statistical uncertainty
# find something interesting, ie. then focus on the unusual thing
# big to small
# 2 or 3 papers published recently (1 paragraph) - structure thoughts
# not 10 references
# write in R markdown
# show all code

# 1 - not in every wave, multiple variables, "feel closer to","member of party",
# if election next week, what party vote for, panel survey - track people time,
# explore how many loyal, switch, differences between people, narrative, join 
# waves together, use skills learned in module, recode variables, reshape maybe,

#2 - life satisifcation scale 1-11, identify people who got divorced, maritial 
# status, find people who got divorced, average before/after, join dfs

#3 - slightly easier technically, single person households, married couples, 
# three generation households, single mothers, single fathers - compare across 
# regions- not longitudinal, might need to create variable, household charact, 
# could create from egoalt, classification, London vs other regions hypothesis?

## Functions and loops (iteration)
## rarely used
## data science they are used

# for loops and loops
# automate task = function

# to know basics
# number of functions together is a package
# no need in reports
# in ELE test a couple of qs but not focus

#1. Write a function checking if a number is even. If it is even return “Even”.
#If it isn’t return “Odd”. If it’s 0 return “Zero”.(Use the modulus operator%%)

myFirstFunction <- function(x) {
  if (x == 0) {
    print("Zero")
    } else {
      if (x %% 2 == 0) {
        print("Even")
        } else {
          print("Odd")
        }}
}

myFirstFunction(0)
myFirstFunction(1)
myFirstFunction(2)

#2. Write a function finding the largest element of a numeric vector.

max.function <- function(x){
  maxValue <- x[[1]]
  for (i in x) {
    if (i > maxValue) {
      maxValue <- i
    }
  } 
  return(maxValue)
}

max.function(20:30)

# max value only exists within the function
# not visible in global environment

#3. Write a function that returns the number of rows and the number of columns 
#in a data frame (as a vector with two elements).
l <- 2:5
m <- 5:8
x <- cbind(l,m)
x

for (i in x[1]) {
  for (j in x[2]) {
    nrow <- count[i]
  }
  ncols <- count[j]
}

dimFunction <- function(x) {
  return(c(ncol(x),nrow(x)))
}
dimFunction(x)

#4. Write both_na(), a function that takes two vectors of the same length and 
#returns the number of positions that have an NA in both vectors.

x <- c(1,2,NA,2,NA,NA,2,NA)
y <- c(1,3,NA,NA,5,NA,5,NA)

both_na <- function(x,y){
  sum(is.na(x) & is.na(y))
  }

both_na(x,y)


#5. Open the indall file from wave 11 and produce a data frame showing the 

#largest value for each variable (column).

# tapply, sapply, mapply
k_indall <- vroom("UKDA-6614-tab/tab/ukhls/k_indall.tab")

sapply(k_indall, max)
map_dfr(k_indall, max)

dataframe <- function(x) {
  for (i in i[2]) {
  l <- max(i,na.rm=T)
  return(l)
  }
}
dataframe(k_indall)
