Data Analysis in Social Science (15 March)
================

Exercises.

1)  Create a vector of length 100, randomly drawing it from the standard
    normal distribution. Find the mean and standard deviation. Multiply
    the vector by 2. Are the mean and standard deviation going to
    change?

2)  Read the individual wave 10 UndSoc data and extract the variable for
    age from the data frame. What type is it?

3)  Convert sex into a logical vector for being male. Calculate the
    proportion of men in the data set.

4)  Convert sex into a character vector with the values “male” and
    “female”.

5)  Convert sex into a factor. Change the order of levels.

6)  Make a list of four elements containing: 1) the vector from exercise
    1, 2) the vector from exercise 3, 3) TRUE, 4) a list with your name
    and your surname.

7)  Regress personal income (j\_fimnnet\_dv) on age and age squared.
    Extract regression coefficients as a vector.

<!-- end list -->

``` r
library(tidyverse)
```

    ## ── Attaching packages ─────────────────────────────────────── tidyverse 1.3.0 ──

    ## ✓ ggplot2 3.3.3     ✓ purrr   0.3.4
    ## ✓ tibble  3.0.4     ✓ dplyr   1.0.2
    ## ✓ tidyr   1.1.2     ✓ stringr 1.4.0
    ## ✓ readr   1.4.0     ✓ forcats 0.5.0

    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

``` r
# Ex. 1

x <- rnorm(100)
mean(x)
```

    ## [1] 0.007332092

``` r
sd(x)
```

    ## [1] 0.9091107

``` r
mean(x * 2)
```

    ## [1] 0.01466418

``` r
sd(x * 2)
```

    ## [1] 1.818221

``` r
# Ex. 2
df <- read_tsv("UKDA-6614-tab/tab/ukhls_w10/j_indresp.tab")
```

    ## 
    ## ── Column specification ────────────────────────────────────────────────────────
    ## cols(
    ##   .default = col_double()
    ## )
    ## ℹ Use `spec()` for the full column specifications.

``` r
   df %>%
    pull(j_age_dv) %>%
    class()
```

    ## [1] "numeric"

``` r
    df %>%
      pull(j_age_dv) %>%
      typeof()
```

    ## [1] "double"

``` r
    df %>%
      pull(j_age_dv) %>%
      as.integer() %>%
      typeof()
```

    ## [1] "integer"

``` r
# Ex. 3
    df %>%
      pull(j_sex) %>% class()
```

    ## [1] "numeric"

``` r
   df %>%
      mutate(j_male = ifelse(j_sex == 1, TRUE, FALSE)) %>%
      pull(j_male) %>% class()
```

    ## [1] "logical"

``` r
   df %>%
      mutate(j_male = ifelse(j_sex == 1, TRUE, FALSE)) %>%
     pull(j_male) %>% mean()
```

    ## [1] 0.4482487

``` r
      df %>%
      mutate(j_male = ifelse(j_sex == 1, TRUE, FALSE)) %>%
      summarise(
        propMen = mean(j_male)
      )
```

    ## # A tibble: 1 x 1
    ##   propMen
    ##     <dbl>
    ## 1   0.448

``` r
# Ex. 4
      df %>%
      mutate(j_sexChr = ifelse(j_sex == 1, "male", 
                             ifelse(j_sex == 2, "female", NA))) %>%
      pull(j_sexChr) %>% class()
```

    ## [1] "character"

``` r
      df %>%
      mutate(j_sexChr = ifelse(j_sex == 1, "male", 
                             ifelse(j_sex == 2, "female", NA))) %>%
      pull(j_sexChr) %>% mean()
```

    ## Warning in mean.default(.): argument is not numeric or logical: returning NA

    ## [1] NA

``` r
# Ex. 5
      df %>%
        mutate(j_sexChr = ifelse(j_sex == 1, "male", 
                             ifelse(j_sex == 2, "female", NA))) %>%
        mutate(j_sexFct = factor(j_sexChr)) %>%
        pull(j_sexFct) %>% class()
```

    ## [1] "factor"

``` r
        df %>%
          mutate(j_sexChr = ifelse(j_sex == 1, "male", 
                             ifelse(j_sex == 2, "female", NA))) %>%
          mutate(j_sexFct = factor(j_sexChr)) %>%
          pull(j_sexFct) %>% mean()
```

    ## Warning in mean.default(.): argument is not numeric or logical: returning NA

    ## [1] NA

``` r
        df %>%
          mutate(j_sexChr = ifelse(j_sex == 1, "male", 
                             ifelse(j_sex == 2, "female", NA))) %>%
          mutate(j_sexFct = factor(j_sexChr, levels = c("male", "female"))) %>%
          count(j_sexFct)
```

    ## # A tibble: 2 x 2
    ##   j_sexFct     n
    ##   <fct>    <int>
    ## 1 male     15383
    ## 2 female   18935
