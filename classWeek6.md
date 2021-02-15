Data Analysis in Social Science (15 February)
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

1)  Open the data for the first 5 waves of the UndSoc data (indresp
    files). We will need the following variables: pidp, sex\_dv,
    age\_dv, fimnnet\_dv (total monthly personal income). Join the data
    sets together. Reshape to the long format.
    
    ``` r
    w1 <- vroom("UKDA-6614-tab/tab/ukhls_w1/a_indresp.tab",
            col_select = c(pidp, a_sex_dv, a_age_dv, a_fimnnet_dv))
    ```
    
        ## Rows: 50,994
        ## Columns: 4
        ## Delimiter: "\t"
        ## dbl [4]: pidp, a_sex_dv, a_age_dv, a_fimnnet_dv
        ## 
        ## Use `spec()` to retrieve the guessed column specification
        ## Pass a specification to the `col_types` argument to quiet this message
    
    ``` r
    w2 <- vroom("UKDA-6614-tab/tab/ukhls_w2/b_indresp.tab",
            col_select = c(pidp, b_sex_dv, b_age_dv, b_fimnnet_dv))
    ```
    
        ## Rows: 54,569
        ## Columns: 4
        ## Delimiter: "\t"
        ## dbl [4]: pidp, b_sex_dv, b_age_dv, b_fimnnet_dv
        ## 
        ## Use `spec()` to retrieve the guessed column specification
        ## Pass a specification to the `col_types` argument to quiet this message
    
    ``` r
    w3 <- vroom("UKDA-6614-tab/tab/ukhls_w3/c_indresp.tab",
            col_select = c(pidp, c_sex_dv, c_age_dv, c_fimnnet_dv))
    ```
    
        ## Rows: 49,692
        ## Columns: 4
        ## Delimiter: "\t"
        ## dbl [4]: pidp, c_sex_dv, c_age_dv, c_fimnnet_dv
        ## 
        ## Use `spec()` to retrieve the guessed column specification
        ## Pass a specification to the `col_types` argument to quiet this message
    
    ``` r
    w4 <- vroom("UKDA-6614-tab/tab/ukhls_w4/d_indresp.tab",
            col_select = c(pidp, d_sex_dv, d_age_dv, d_fimnnet_dv))
    ```
    
        ## Rows: 47,071
        ## Columns: 4
        ## Delimiter: "\t"
        ## dbl [4]: pidp, d_sex_dv, d_age_dv, d_fimnnet_dv
        ## 
        ## Use `spec()` to retrieve the guessed column specification
        ## Pass a specification to the `col_types` argument to quiet this message
    
    ``` r
    w5 <- vroom("UKDA-6614-tab/tab/ukhls_w5/e_indresp.tab",
            col_select = c(pidp, e_sex_dv, e_age_dv, e_fimnnet_dv))
    ```
    
        ## Rows: 44,833
        ## Columns: 4
        ## Delimiter: "\t"
        ## dbl [4]: pidp, e_sex_dv, e_age_dv, e_fimnnet_dv
        ## 
        ## Use `spec()` to retrieve the guessed column specification
        ## Pass a specification to the `col_types` argument to quiet this message
    
    ``` r
    Joined <- w1 %>%
      full_join(w2, by = "pidp") %>%
      full_join(w3, by = "pidp") %>%
      full_join(w4, by = "pidp") %>%
      full_join(w5, by = "pidp")
    
    Long <- Joined %>%
      pivot_longer(a_sex_dv:e_fimnnet_dv, names_to = "variable",
               values_to = "value") %>%
      separate(variable, into = c("wave", "variable"), sep ="_",
           extra = "merge") %>%
      pivot_wider(names_from = variable, values_from = value)
    
    Long
    ```
    
        ## # A tibble: 375,545 x 5
        ##        pidp wave  sex_dv age_dv fimnnet_dv
        ##       <dbl> <chr>  <dbl>  <dbl>      <dbl>
        ##  1 68001367 a          1     39      1400 
        ##  2 68001367 b         NA     NA        NA 
        ##  3 68001367 c         NA     NA        NA 
        ##  4 68001367 d         NA     NA        NA 
        ##  5 68001367 e         NA     NA        NA 
        ##  6 68004087 a          1     59       802.
        ##  7 68004087 b          1     60      1277.
        ##  8 68004087 c          1     61       914.
        ##  9 68004087 d          1     62       914.
        ## 10 68004087 e          1     63      1016.
        ## # … with 375,535 more rows

2)  Calculate mean income by sex across all five waves and save as a
    table with summary statistics. Reshape so that you have sexes in
    rows and waves in columns. Reshape so that you have waves in rows
    and sexes in columns. Calculate the difference between male and
    female income by wave.
    
    ``` r
    summary(Long$fimnnet_dv)
    ```
    
        ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
        ##  -17741     603    1111    1330    1730  350392  128386
    
    ``` r
    tableIncome <- Long %>%
      filter(sex_dv %in% 1:2) %>%
      mutate(sex = ifelse(sex_dv == 1, "male", "female")) %>%
      group_by(sex, wave) %>%
      summarise(
        meanIncome = mean(fimnnet_dv, na.rm = TRUE)
      )
    ```
    
        ## `summarise()` regrouping output by 'sex' (override with `.groups` argument)
    
    ``` r
    tableIncome %>%
      pivot_wider(names_from = wave, values_from = meanIncome)
    ```
    
        ## # A tibble: 2 x 6
        ## # Groups:   sex [2]
        ##   sex        a     b     c     d     e
        ##   <chr>  <dbl> <dbl> <dbl> <dbl> <dbl>
        ## 1 female 1020. 1083. 1133. 1162. 1181.
        ## 2 male   1462. 1538. 1616. 1654. 1679.
    
    ``` r
    tableIncome %>%
      pivot_wider(names_from = sex, values_from = meanIncome) %>%
      mutate(femaleToMaleRatio = female / male)
    ```
    
        ## # A tibble: 5 x 4
        ##   wave  female  male femaleToMaleRatio
        ##   <chr>  <dbl> <dbl>             <dbl>
        ## 1 a      1020. 1462.             0.698
        ## 2 b      1083. 1538.             0.704
        ## 3 c      1133. 1616.             0.701
        ## 4 d      1162. 1654.             0.702
        ## 5 e      1181. 1679.             0.704
