# Week 9. Lists
# AB
# 12 March 2021

library(tidyverse)

# Lists

# Data types in R

# Vectors

x <- 1:5
x
x[2]
y <- letters[1:5]
y
y[3]
z <- c(TRUE, FALSE, TRUE, FALSE, TRUE)
z

# Matrix

m <- matrix(1:10, nrow = 2)
m

m[1,5]

# all elements should be of the same type

# Data frames

df <- data.frame(x = x, y = y, z = z)
df

df[1,3]
df[3,1]

df1 <- as_tibble(df)
df1
class(df1)

# Lists are collections of different objects

l1 <- list(x, y, z, m, df)
l1

# extracting elements from the list
l1[1]
class(l1[1])

l1[[1]]


# regression output is stored in a list

model1 <- lm(x ~ z, df)
model1
summary(model1)
class(model1)
typeof(model1)
str(model1)

model1[[1]]
model1$coefficients
