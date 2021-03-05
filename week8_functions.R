# Week 8. Functions
# AB
# 5 March 2021

# Another way to automate tasks in R.

# myFunction <- function(args) {
#   body (what function does)
# }

myMean <- function(x) {sum(x) / length(x)}
myMean(1:10)

mean(1:10)


# from last week:

for (i in 1:10) {
  print(1:i)
}

# as a function
funPrint <- function(x) {
  1:x
}

funPrint(5)

# functions can have several arguments

# Modify myMean to include an extra argument to deal with missing values.

myMean2 <- function(x, rm.missing = FALSE) {
  if (rm.missing) {
    x <- na.omit(x)
  }
  sum(x) / length(x)
}

myMean2(c(1:10, NA))
myMean2(c(1:10, NA), rm.missing = TRUE)

# Modify this function to return an error when the vector is not numeric.

myMean3 <- function(x, rm.missing = FALSE) {
  if (!is.numeric(x)) {
    stop("The vector is not numeric.")
  }
  if (rm.missing) {
    x <- na.omit(x)
  }
  sum(x) / length(x)
}

myMean3(c(1:10, "a"))


# Scoping rules and global and local environments.

# Objects created within a function belong to the local function environment and cannot be accessed
# globally.

y <- 1:5

print_y <- function(){
  print(y)
} 

print_y()


print_y2 <- function(){
  y <- 1:10
  print(y)
} 

print_y2()
y


# Modify myMean() so that it saves the mean in the environment with the name “meanx”. 


myMean4 <- function(x) {
  meanx <<- sum(x) / length(x)
  meanx
}

myMean4(1:10)


