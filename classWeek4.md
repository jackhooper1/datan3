Data Analysis in Social Science (1 February)
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
dfStable <- read_tsv("UKDA-6614-tab/tab/ukhls_wx/xwavedat.tab")
```

    ## 
    ## ── Column specification ────────────────────────────────────────────────────────
    ## cols(
    ##   .default = col_double()
    ## )
    ## ℹ Use `spec()` for the full column specifications.

This is a cross-wave data file with stable characteristics of
individuals. See the codebook at
<https://www.understandingsociety.ac.uk/documentation/mainstage/dataset-documentation/search/datafile/xwavedat>.

Exercises.

1.  Select the variables for: sex (derived), date of birth (doby\_dv),
    ethnic group (racel\_dv). Also keep the cross-wave identifier (pidp)
    and the sample origin variable (memorig).
    
    ``` r
    dfStable1 <- dfStable %>%
      select(pidp, memorig, sex_dv, doby_dv, racel_dv)
    ```

2.  Filter the data to keep: a) men only. b) people born before 1950 and
    after 1975. c) men of Pakistani origin born in 1958 or 1982.
    
    ``` r
    # a)
    dfStable1 %>%
      filter(sex_dv == 1)
    ```
    
        ## # A tibble: 59,642 x 5
        ##      pidp memorig sex_dv doby_dv racel_dv
        ##     <dbl>   <dbl>  <dbl>   <dbl>    <dbl>
        ##  1 184965       3      1    1982        1
        ##  2 223725       3      1    1975        1
        ##  3 261125       3      1    1985        1
        ##  4 299885       3      1    1980        1
        ##  5 496405       3      1    1975        1
        ##  6 537205       3      1    1975        1
        ##  7 541285       3      1    1985        1
        ##  8 665045       3      1    1981        1
        ##  9 732365       3      1    1985        1
        ## 10 760925       3      1    1981       -9
        ## # … with 59,632 more rows
    
    ``` r
    # b)
    dfStable1 %>% count(doby_dv)
    ```
    
        ## # A tibble: 115 x 2
        ##    doby_dv     n
        ##      <dbl> <int>
        ##  1     -20 26871
        ##  2      -9   309
        ##  3    1900     1
        ##  4    1908     2
        ##  5    1910     3
        ##  6    1911     4
        ##  7    1912    13
        ##  8    1913    18
        ##  9    1914    20
        ## 10    1915    22
        ## # … with 105 more rows
    
    ``` r
    dfStable1 %>%
      filter(doby_dv > 0) %>% 
      filter(doby_dv < 1950 | doby_dv > 1975)
    ```
    
        ## # A tibble: 84,028 x 5
        ##      pidp memorig sex_dv doby_dv racel_dv
        ##     <dbl>   <dbl>  <dbl>   <dbl>    <dbl>
        ##  1  22445       3      2    1984        1
        ##  2  29925       3      2    1977        1
        ##  3  76165       3      2    1982        1
        ##  4 184965       3      1    1982        1
        ##  5 261125       3      1    1985        1
        ##  6 280165       3      2    1979        1
        ##  7 299885       3      1    1980        1
        ##  8 333205       3      2    1990        1
        ##  9 387605       3      2    1988        1
        ## 10 469205       3      2    1990        1
        ## # … with 84,018 more rows
    
    ``` r
    # c)
    dfStable1 %>%
      filter(sex_dv == 1) %>%
      filter(racel_dv == 10) %>%
      filter(doby_dv == 1958 | doby_dv == 1982)
    ```
    
        ## # A tibble: 41 x 5
        ##         pidp memorig sex_dv doby_dv racel_dv
        ##        <dbl>   <dbl>  <dbl>   <dbl>    <dbl>
        ##  1  69584407       7      1    1982       10
        ##  2 205542927       7      1    1958       10
        ##  3 205845531       7      1    1982       10
        ##  4 347949284       7      1    1982       10
        ##  5 411567965       3      1    1958       10
        ##  6 620581844       7      1    1982       10
        ##  7 749455207       7      1    1958       10
        ##  8 750075367       7      1    1958       10
        ##  9 817945503       7      1    1982       10
        ## 10 889140896       8      1    1982       10
        ## # … with 31 more rows
    
    ``` r
    dfStable1 %>%
      filter(sex_dv == 1 & racel_dv == 10 & (doby_dv == 1958 | doby_dv == 1982))
    ```
    
        ## # A tibble: 41 x 5
        ##         pidp memorig sex_dv doby_dv racel_dv
        ##        <dbl>   <dbl>  <dbl>   <dbl>    <dbl>
        ##  1  69584407       7      1    1982       10
        ##  2 205542927       7      1    1958       10
        ##  3 205845531       7      1    1982       10
        ##  4 347949284       7      1    1982       10
        ##  5 411567965       3      1    1958       10
        ##  6 620581844       7      1    1982       10
        ##  7 749455207       7      1    1958       10
        ##  8 750075367       7      1    1958       10
        ##  9 817945503       7      1    1982       10
        ## 10 889140896       8      1    1982       10
        ## # … with 31 more rows

3.  Recode birth year into cohorts (a new variable): the G.I. Generation
    (born before 1924), Silent Generation (1925-42), Baby Boomers
    (1943-65), Generation X (1966-1980), Millenials (1981-99),
    Generation Z (2000-). (The years are approximate.)
    
    ``` r
    dfStable1 <-  dfStable1 %>%
      mutate(cohort = case_when(
        doby_dv > 0 & doby_dv < 1924 ~ "G.I.Generation",
        between(doby_dv, 1925, 1942) ~ "Silent generation",
        doby_dv %in% 1943:1965 ~ "Baby boomers",
        doby_dv %in% 1966:1980 ~ "Generation X",
        doby_dv %in% 1981:1999 ~ "Millenials",
        doby_dv > 2000 ~ "Generation Z"
      ))
    
    dfStable1 %>% count(cohort)
    ```
    
        ## # A tibble: 7 x 2
        ##   cohort                n
        ##   <chr>             <int>
        ## 1 Baby boomers      28424
        ## 2 G.I.Generation      927
        ## 3 Generation X      23908
        ## 4 Generation Z      21775
        ## 5 Millenials        34192
        ## 6 Silent generation 10201
        ## 7 <NA>              28910

4.  Recode ethnicity into the following groups: white British, Other
    White, Indian, Pakistani, other. (This classification doesn’t make
    much sense, but we’re doing this as an exercise).
    
    ``` r
    dfStable1 <- dfStable1 %>%
      mutate(ethnNew = recode(racel_dv,
        '1' = "White British",
        '2' = "other White",
        '3' = "other White",
        '4' = "other White",
        '9' = "Indian",
        '10' = "Pakistani",
        '-9' = NA_character_,
        .default = "other"
      ))
    
    dfStable1 %>% count(ethnNew)
    ```
    
        ## # A tibble: 6 x 2
        ##   ethnNew           n
        ##   <chr>         <int>
        ## 1 Indian         3704
        ## 2 other         11375
        ## 3 other White    5500
        ## 4 Pakistani      3316
        ## 5 White British 76510
        ## 6 <NA>          47932

5.  Count the number of people belonging to different ethnic groups (and
    produce percentages).
    
    ``` r
    dfStable1 %>%
      count(ethnNew) %>%
      mutate(perc = n / sum(n) * 100)
    ```
    
        ## # A tibble: 6 x 3
        ##   ethnNew           n  perc
        ##   <chr>         <int> <dbl>
        ## 1 Indian         3704  2.50
        ## 2 other         11375  7.67
        ## 3 other White    5500  3.71
        ## 4 Pakistani      3316  2.24
        ## 5 White British 76510 51.6 
        ## 6 <NA>          47932 32.3

6.  Summarise the proportion of white British by generation.
    
    ``` r
    dfStable1 %>%
      filter(racel_dv != -9) %>%
      mutate(whiteBritish = ifelse(racel_dv == 1, 1, 0)) %>%
      group_by(cohort) %>%
      summarise(
    propWhiteBritish = mean(whiteBritish, na.rm = TRUE) * 100
      )
    ```
    
        ## `summarise()` ungrouping output (override with `.groups` argument)
    
        ## # A tibble: 7 x 2
        ##   cohort            propWhiteBritish
        ##   <chr>                        <dbl>
        ## 1 Baby boomers                  80.7
        ## 2 G.I.Generation                93.4
        ## 3 Generation X                  64.7
        ## 4 Generation Z                  60.2
        ## 5 Millenials                    64.2
        ## 6 Silent generation             86.9
        ## 7 <NA>                          92.0
