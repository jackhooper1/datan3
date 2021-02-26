# Week 7. Loops
# AB
# 25 Feb 2021

# we use loops when we want automate repetitive actions

# for loops

# iterate over a numeric vector

for (i in 1:10) {
  print(1:i)
}

# character vector

longName <- NULL
for (names in c("James", "John", "Mary")) {
  longName <- paste0(longName, names)
  print(longName)
}

# you can use nested loops

# let's iterate over a data frame

df <- tibble(x = 1:10, y = 11:20, z = 10:1)
df
dim(df)

res <- vector("numeric", length = 30)

k <- 1
for (i in 1:dim(df)[1]) {
  for (j in 1:dim(df)[2]) {
    res[k] <- df[[i,j]]
    k <- k + 1
  }
}
res

############################################

# while loops

i <- 1
while (i <= 5) {
  print(i)
  i = i + 1
}

# usually we use for loops, but for some tasks while loops are more
# convenient.

# We rarely use loops in R as 
# 1) it is a vectorised language,
# 2) it has other more convenient tools to automate repetitive tasks. 




