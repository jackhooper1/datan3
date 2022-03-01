## Week 7 Exercises
library(tidyverse)
library(nycflights13)
#12.2.1
#1 
output <- vector("double", ncol(mtcars))
names(output) <- names(mtcars)
for (i in names(mtcars)){
  output[i] <- mean(mtcars[[i]])
}
output

output <- vector("list", ncol(flights))
names(output) <- names(flights)
for (i in names(flights)) {
  output[[i]] <- class(flights[[i]])
}
output

output <- vector("double", ncol(iris))
names(output) <- names(iris)
for(i in names(iris)){
  output[i] <- n_distinct(iris[[i]])
}
output

# number to draw
n <- 10
# values of the mean
mu <- c(-10, 0, 10, 100)
normals <- vector("list", length(mu))
for (i in seq_along(normals)) {
  normals[[i]] <- rnorm(n, mean = mu[i])
}
normals


#12.3.5