Data Analysis in Social Science (1 March)
================

Exercises.

1)  Check if a number is even. If it is even print “Even”. If it isn’t
    print “Odd”. If it’s 0 print “Zero”. (Use the modulus operator %%).

2)  Write a for loop finding the largest element of a numeric vector and
    print it on the screen.

3)  Write a for loop finding the largest element of a numeric matrix and
    print it on the screen.

4)  Write a while loop finding the largest element of a numeric vector
    and print it on the screen.

5)  x is a vector with whole numbers (zero and positive integers). If
    the largest even element of x is smaller than the largest odd
    element of x, all even elements of x are replaced by 0s. Otherwise
    all odd elements of x are replaced by 0s. For example, if x = {7; 1;
    3; 2; 14; 5; 9; 6} the output should be \[0; 0; 0; 2; 14; 0; 0; 6\].

6)  x is a vector with whole numbers. Write a programme that counts the
    number of pairs of the elements of x where the sum can be divided by
    12 without a remainder. For example, for x = {8; 10; 14; 7; 13; 5;
    30; 9; 6} then the answer is 3 ((10, 14); (7, 5); (30, 6)).
    
    ``` r
    # Ex.1
    
    x <- 2
    if (x == 0) {
      print("x is 0")
    } else if (x %% 2 == 0) {
      print("x is even")
    } else {
      print("x is odd")
    }
    ```
    
        ## [1] "x is even"
    
    ``` r
    # Ex.2 
    x <- c(2, 6, 1, 8, 2)
    max_x <- x[1]
    for (i in seq_along(x)) {
      if (x[i] > max_x) {
        max_x <- x[i]
      }
    }
    max_x
    ```
    
        ## [1] 8
    
    ``` r
    # Ex.3
    x <- matrix(c(2, 6, 1, 8, 2, 10, -7, 5, 52, 6), nrow = 2)
    max_x <- x[1,1]
    dim(x)
    ```
    
        ## [1] 2 5
    
    ``` r
    for (i in 1:dim(x)[1]) {
      for (j in 1:dim(x)[2]) {
        if (x[i,j] > max_x) {
         max_x <- x[i,j]
         }
        }
      }
    
    max_x
    ```
    
        ## [1] 52
    
    ``` r
    # Ex.4
    x <- c(12, 6, 1, 8, 2)
    max_x <- x[1]
    i <- 2
    while (i <= length(x)) {
      if (x[i] > max_x) {
        max_x <- x[i]
      }
    i <- i + 1
      }
    max_x
    ```
    
        ## [1] 12
