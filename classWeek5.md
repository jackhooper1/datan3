Data Analysis in Social Science (8 February)
================

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
library(vroom)
```

Exercises.

1)  Calculate the average age of parents (separately for women and men)
    of children born between waves 9 and 10.
    
    ``` r
    # Read the data for newborn children (born between waves 9 and 10).
    # Observations are at the parent level.
    
    newborn10 <- vroom("UKDA-6614-tab/tab/ukhls_w10/j_newborn.tab")
    ```
    
        ## Rows: 778
        ## Columns: 34
        ## Delimiter: "\t"
        ## dbl [34]: pidp, j_hidp, j_pno, j_newchno, j_hhorig, j_memorig, j_psu, j_strata, j_sampst, ...
        ## 
        ## Use `spec()` to retrieve the guessed column specification
        ## Pass a specification to the `col_types` argument to quiet this message
    
    ``` r
    # Read the data with people's age.
    
    indall10 <- vroom("UKDA-6614-tab/tab/ukhls_w10/j_indall.tab")
    ```
    
        ## Rows: 50,110
        ## Columns: 197
        ## Delimiter: "\t"
        ## dbl [197]: pidp, pid, j_hidp, j_pno, j_hhorig, j_memorig, j_psu, j_strata, j_sampst, j_month...
        ## 
        ## Use `spec()` to retrieve the guessed column specification
        ## Pass a specification to the `col_types` argument to quiet this message
    
    ``` r
    # Use semi_join (filtering join) to keep only new parents in the data set.
    
    indall10 %>%
      semi_join(newborn10, by = "pidp") %>%
      # split by gender
      group_by(j_sex) %>%
      # filter out missing ages
      filter(j_age_dv > 0) %>%
      # calculate mean age
      summarise(
    meanAge = mean(j_age_dv, na.rm = TRUE)
      )
    ```
    
        ## `summarise()` ungrouping output (override with `.groups` argument)
    
        ## # A tibble: 2 x 2
        ##   j_sex meanAge
        ##   <dbl>   <dbl>
        ## 1     1    34.7
        ## 2     2    31.3

2)  Split the table by ethnic group for the following groups: White
    British, Indian, Pakistani, Bangladeshi, and African.
    
    ``` r
    # ethnicity is recoded in the cross-wave file
    stable <- vroom("UKDA-6614-tab/tab/ukhls_wx/xwavedat.tab",
                col_select = c(pidp, racel_dv))
    ```
    
        ## Rows: 148,337
        ## Columns: 2
        ## Delimiter: "\t"
        ## dbl [2]: pidp, racel_dv
        ## 
        ## Use `spec()` to retrieve the guessed column specification
        ## Pass a specification to the `col_types` argument to quiet this message
    
    ``` r
    indall10 %>%
      semi_join(newborn10, by = "pidp") %>%
      # join with ethnicity data
      left_join(stable, by = "pidp") %>%
      # keep only some ethnic groups
      filter(racel_dv %in% c(1, 9, 10, 11, 15)) %>%
      # group by ethnicity and sex
      group_by(racel_dv, j_sex) %>%
      # filter out missing ages
      filter(j_age_dv > 0) %>%
      # calculate mean age
      summarise(
    meanAge = mean(j_age_dv, na.rm = TRUE)
      )
    ```
    
        ## `summarise()` regrouping output by 'racel_dv' (override with `.groups` argument)
    
        ## # A tibble: 10 x 3
        ## # Groups:   racel_dv [5]
        ##    racel_dv j_sex meanAge
        ##       <dbl> <dbl>   <dbl>
        ##  1        1     1    34.5
        ##  2        1     2    31.1
        ##  3        9     1    34.5
        ##  4        9     2    34.7
        ##  5       10     1    33.8
        ##  6       10     2    31.2
        ##  7       11     1    38.1
        ##  8       11     2    30.3
        ##  9       15     1    33.9
        ## 10       15     2    32
    
    ``` r
    # alternatively, you can us ethe variable j_ethn_dv in indall10
    
    indall10 %>%
      semi_join(newborn10, by = "pidp") %>%
      filter(j_ethn_dv %in% c(1, 9, 10, 11, 15)) %>%
      # group by ethnicity and sex
      group_by(j_ethn_dv, j_sex) %>%
      # filter out missing ages
      filter(j_age_dv > 0) %>%
      # calculate mean age
      summarise(
    meanAge = mean(j_age_dv, na.rm = TRUE)
      )
    ```
    
        ## `summarise()` regrouping output by 'j_ethn_dv' (override with `.groups` argument)
    
        ## # A tibble: 10 x 3
        ## # Groups:   j_ethn_dv [5]
        ##    j_ethn_dv j_sex meanAge
        ##        <dbl> <dbl>   <dbl>
        ##  1         1     1    34.4
        ##  2         1     2    31.0
        ##  3         9     1    34.5
        ##  4         9     2    34.7
        ##  5        10     1    33.8
        ##  6        10     2    30.9
        ##  7        11     1    38.1
        ##  8        11     2    30.3
        ##  9        15     1    33.9
        ## 10        15     2    32.7

3)  Produce a data table that includes only twins born between waves 9
    and 10. How does the age of their parents compare with the age of
    parents of non-twin children?
    
    ``` r
    twins <- newborn10 %>%
      # find parents with more than one new child
      group_by(pidp) %>%
      # create a variable with the number of children per parent
      mutate(n_children = n()) %>%
      # keep only those with > 1 children
      filter(n_children > 1) %>%
      select(pidp, n_children)
    
    # repeat as in ex.1 but only for the parents of twins.
    
    indall10 %>%
      semi_join(twins, by = "pidp") %>%
      # split by gender
      group_by(j_sex) %>%
      # filter out missing ages
      filter(j_age_dv > 0) %>%
      # calculate mean age
      summarise(
    meanAge = mean(j_age_dv, na.rm = TRUE)
      )
    ```
    
        ## `summarise()` ungrouping output (override with `.groups` argument)
    
        ## # A tibble: 2 x 2
        ##   j_sex meanAge
        ##   <dbl>   <dbl>
        ## 1     1    40.4
        ## 2     2    30.9
