Data Analysis 3: Week 8
================
Alexey Bessudnov
4 March 2020

Plan for today:

Functions.

1)  Simple functions.
2)  The use of arguments.
3)  Conditional execution.
4)  Global and local environments.
5)  Recursion.
6)  Functions with pipes and dplyr.
7)  Functions in applied social data analysis.
8)  Homework for next week: iteration.

Exercise 1. Write a function to calculate the mean of a numeric vector.

``` r
myMean <- function(x) {
  sum(x) / length(x)
}

myMean(1:10)
```

    ## [1] 5.5

``` r
mean(1:10)
```

    ## [1] 5.5

Execise 2. Write a conditional statement that checks if a vector is
numeric, prints its first element if it is and prints its last element
if it isn’t.

``` r
x <- c(1:5, "a")
if (is.numeric(x)){
  x[1]
} else {
  x[length(x)]
}
```

    ## [1] "a"

Exercise 3. Modify myMean to include an extra argument to deal with
missing values.

``` r
myMean2 <- function(x, rm.missing = FALSE) {
        if (rm.missing == TRUE) {
                x <- na.omit(x)
        }
        sum(x) / length(x)
}

myMean(1:10)
```

    ## [1] 5.5

``` r
myMean2(c(1:10, NA))
```

    ## [1] NA

``` r
myMean2(c(1:10, NA), rm.missing = TRUE)
```

    ## [1] 5.5

Exercise 4. Modify this function to return an error when the vector is
not numeric.

``` r
myMean3 <- function(x, rm.missing = FALSE) {
        if (!is.numeric(x)) {
                stop("The vector is not numeric.")
        }
        if (rm.missing == TRUE) {
                x <- na.omit(x)
        }
        sum(x) / length(x)
}
# myMean3(c(1:10, "a"))
```

Exercise 5. Modify this function so that it saves the mean in the
environment with the name “meanx”. (Hint: think about environments.)

``` r
myMean(1:10)
```

    ## [1] 5.5

``` r
# meanx <- myMean(1:10)
myMean4 <- function(x) {
        meanx <<- sum(x) / length(x)
        meanx
}
myMean4(1:10)
```

    ## [1] 5.5

Exercise 6. Write a function to calculate the factorial (i.e. 5\! =
1x2x3x4x5). Note that 0\! = 1, and for the negative numbers the
factorial is not defined. (Hint: use recursion.)

``` r
myFactorial <- function(x){
  if (x == 0) {return(1)}
  x * myFactorial(x-1)
}

myFactorial(4)
```

    ## [1] 24

Functions with dplyr: see
<https://cran.r-project.org/web/packages/dplyr/vignettes/programming.html>

``` r
library(tidyverse)

# Create two tibbles with a similar structure

data1 <- tibble(x1 = c(rep("a", 3), rep("b", 3), rep("c", 3)),
               x2 = c(rep(letters[1:3], 3)),
               y = rpois(9, 5)) %>%
          mutate(y2 = y^2)

data2 <- tibble(x1 = c(rep("a", 3), rep("b", 3), rep("c", 3)),
               x2 = c(rep(letters[1:3], 3)),
               y = rpois(9, 5)) %>%
            mutate(y2 = y^2)


# Find mean y grouping by x1

data1 %>%
  group_by(x1) %>%
  summarise(
    mean_y = mean(y)
  )
```

    ## # A tibble: 3 x 2
    ##   x1    mean_y
    ##   <chr>  <dbl>
    ## 1 a       4.33
    ## 2 b       2.67
    ## 3 c       4.67

``` r
# Can we make a function for this?

mean_y <- function(df){
  df %>%
  group_by(x1) %>%
  summarise(
    mean_y = mean(y)
  )
}

mean_y(data1)
```

    ## # A tibble: 3 x 2
    ##   x1    mean_y
    ##   <chr>  <dbl>
    ## 1 a       4.33
    ## 2 b       2.67
    ## 3 c       4.67

``` r
mean_y(data2)
```

    ## # A tibble: 3 x 2
    ##   x1    mean_y
    ##   <chr>  <dbl>
    ## 1 a       2   
    ## 2 b       5.67
    ## 3 c       5

``` r
# Can we add the grouping variable interactively?

mean_y2 <- function(df, groupVar){
  df %>%
  group_by(groupVar) %>%
  summarise(
    mean_y = mean(y)
  )
}

# mean_y(data1, x1)
# This is not working.

mean_y3 <- function(df, groupVar){
  groupVar <- enquo(groupVar)
  df %>%
  group_by(!! groupVar) %>%
  summarise(
    mean_y = mean(y)
  )
}

mean_y3(data1, x1)
```

    ## # A tibble: 3 x 2
    ##   x1    mean_y
    ##   <chr>  <dbl>
    ## 1 a       4.33
    ## 2 b       2.67
    ## 3 c       4.67

``` r
# You may also want to change expressions in a function.

data1 %>%
  group_by(x1) %>%
  summarise(
    mean_y = mean(y)
  )
```

    ## # A tibble: 3 x 2
    ##   x1    mean_y
    ##   <chr>  <dbl>
    ## 1 a       4.33
    ## 2 b       2.67
    ## 3 c       4.67

``` r
data1 %>%
  group_by(x1) %>%
  summarise(
    mean_y = mean(y + y2)
  )
```

    ## # A tibble: 3 x 2
    ##   x1    mean_y
    ##   <chr>  <dbl>
    ## 1 a       26  
    ## 2 b       11.3
    ## 3 c       28

``` r
mean_y4 <- function(df, groupVar, expr){
  groupVar <- enquo(groupVar)
  df %>%
  group_by(!! groupVar) %>%
  summarise(
    mean_y = mean(expr)
  )
}

# mean_y4(data1, x1, y)
# not working

mean_y5 <- function(df, groupVar, expr){
  groupVar <- enquo(groupVar)
  expr <- enquo(expr)
  df %>%
  group_by(!! groupVar) %>%
  summarise(
    mean_y = mean(!! expr)
  )
}

mean_y5(data1, x1, y)
```

    ## # A tibble: 3 x 2
    ##   x1    mean_y
    ##   <chr>  <dbl>
    ## 1 a       4.33
    ## 2 b       2.67
    ## 3 c       4.67

``` r
mean_y5(data1, x1, y + y2)
```

    ## # A tibble: 3 x 2
    ##   x1    mean_y
    ##   <chr>  <dbl>
    ## 1 a       26  
    ## 2 b       11.3
    ## 3 c       28
