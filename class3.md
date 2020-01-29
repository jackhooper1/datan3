Data Analysis 3: Class 3
================
Alexey Bessudnov
29 January 2020

Plan for today:

  - Test assignment.
  - The **tidyverse** framework (<https://www.tidyverse.org/>).
  - Reading in data with **readr**.
  - Transforming data with **dplyr**.
  - Statistical assignment 1.
  - Homework for next week.

Importing data: read ch.11 from R for Data Science (Data import):
<https://r4ds.had.co.nz/data-import.html> and ch.2 from my website (Read
data): <http://abessudnov.net/dataanalysis3/readdata.html>.

``` r
library(tidyverse)
Data <- read_tsv("data/UKDA-6614-tab/tab/ukhls_wx/xwavedat.tab")
```

This is a cross-wave data file with stable characteristics of
individuals. See the codebook at
<https://www.understandingsociety.ac.uk/documentation/mainstage/dataset-documentation/search/datafile/xwavedat>.

See the dplyr cheetsheet:
<https://github.com/rstudio/cheatsheets/blob/master/data-transformation.pdf>

Exercises.

1.  Select the variables for: sex (derived), date of birth (derived),
    ethnic group (racel\_dv). Also keep the cross-wave identifier (pidp)
    and the sample origin variable (memorig).
    
    ``` r
    # With base R:
    subset(Data, select = c("pidp", "memorig", "sex_dv", "doby_dv", "racel_dv"))
    ```
    
        ## # A tibble: 147,087 x 5
        ##     pidp memorig sex_dv doby_dv racel_dv
        ##    <dbl>   <dbl>  <dbl>   <dbl>    <dbl>
        ##  1   687       3    -20     -20        1
        ##  2  1367       3    -20     -20       15
        ##  3  2051       3    -20     -20       15
        ##  4  2727       3    -20     -20       97
        ##  5  3407       3    -20     -20        1
        ##  6  4091       3    -20     -20        1
        ##  7  4767       3    -20     -20        1
        ##  8  5451       3    -20     -20        1
        ##  9  6135       3    -20     -20        1
        ## 10  6807       3    -20     -20        1
        ## # ... with 147,077 more rows
    
    ``` r
    Data[,c(1, 5, 15, 19)]
    ```
    
        ## # A tibble: 147,087 x 4
        ##     pidp memorig birthy lwenum_dv
        ##    <dbl>   <dbl>  <dbl>     <dbl>
        ##  1   687       3   1899       -20
        ##  2  1367       3   1963       -20
        ##  3  2051       3   1965       -20
        ##  4  2727       3   1933       -20
        ##  5  3407       3   1955       -20
        ##  6  4091       3   1945       -20
        ##  7  4767       3   1939       -20
        ##  8  5451       3   1941       -20
        ##  9  6135       3   1972       -20
        ## 10  6807       3   1965       -20
        ## # ... with 147,077 more rows
    
    ``` r
    # with dplyr
    select(Data, pidp, memorig, sex_dv, doby_dv, racel_dv)
    ```
    
        ## # A tibble: 147,087 x 5
        ##     pidp memorig sex_dv doby_dv racel_dv
        ##    <dbl>   <dbl>  <dbl>   <dbl>    <dbl>
        ##  1   687       3    -20     -20        1
        ##  2  1367       3    -20     -20       15
        ##  3  2051       3    -20     -20       15
        ##  4  2727       3    -20     -20       97
        ##  5  3407       3    -20     -20        1
        ##  6  4091       3    -20     -20        1
        ##  7  4767       3    -20     -20        1
        ##  8  5451       3    -20     -20        1
        ##  9  6135       3    -20     -20        1
        ## 10  6807       3    -20     -20        1
        ## # ... with 147,077 more rows
    
    ``` r
    # using a pipe (%>%)
    
    Data <- Data %>%
      select(pidp, memorig, sex_dv, doby_dv, racel_dv)
    ```

2.  Filter the data to keep (in new data frames): a) men only. b) people
    born before 1950 and after 1975. c) men of Pakistani origin born in
    1958 or 1982.
    
    ``` r
    # a)
    Data %>%
      filter(sex_dv == 1)
    ```
    
        ## # A tibble: 59,000 x 5
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
        ## # ... with 58,990 more rows
    
    ``` r
    # b)
    Data %>%
      filter(doby_dv > 0 & (doby_dv < 1950 |  doby_dv > 1975))
    ```
    
        ## # A tibble: 82,907 x 5
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
        ## # ... with 82,897 more rows
    
    ``` r
    # c)
    Data %>%
      filter(sex_dv == 1 & racel_dv == 10 & (doby_dv == 1958 | doby_dv == 1982))
    ```
    
        ## # A tibble: 40 x 5
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
        ## # ... with 30 more rows

3.  Recode birth year into cohorts (a new variable): the G.I. Generation
    (born before 1924), Silent Generation (1925-42), Baby Boomers
    (1943-65), Generation X (1966-1980), Millenials (1981-99),
    Generation Z (2000-). (The years are approximate.)
    
    ``` r
    # with case_when() (good with complex conditions)
    Data <- Data %>%
      mutate(generation = case_when(
    between(doby_dv, 0, 1924) ~ "GI Generation",
    between(doby_dv, 1925, 1942) ~ "Silent Generation",
    between(doby_dv, 1943, 1965) ~ "Baby Boomers",
    between(doby_dv, 1966, 1980) ~ "Generation X",
    between(doby_dv, 1981, 1999) ~ "Millenials",
    doby_dv >= 2000 ~ "Generation Z"
      ))
    # case_when is particularly useful when there are multiple variables in logical statements, for example:
    
    # men, 18 to 25 years old, recoded to "young men"
    # (the variable names do not refer to our data; this is just an example.)
    # case_when(
    #         between(age, 18, 25) & sex == "male" ~ "young men"
    # )
    
    # You can also use ifelse for recoding, but it works best with simple cases.
    # recode sex into "male" or "female"", dropping other cases.
    
    Data %>%
        mutate(sexBinary = ifelse(sex_dv == 1, "male",
                                   ifelse(sex_dv == 2, "female", NA)))
    ```
    
        ## # A tibble: 147,087 x 7
        ##     pidp memorig sex_dv doby_dv racel_dv generation sexBinary
        ##    <dbl>   <dbl>  <dbl>   <dbl>    <dbl> <chr>      <chr>    
        ##  1   687       3    -20     -20        1 <NA>       <NA>     
        ##  2  1367       3    -20     -20       15 <NA>       <NA>     
        ##  3  2051       3    -20     -20       15 <NA>       <NA>     
        ##  4  2727       3    -20     -20       97 <NA>       <NA>     
        ##  5  3407       3    -20     -20        1 <NA>       <NA>     
        ##  6  4091       3    -20     -20        1 <NA>       <NA>     
        ##  7  4767       3    -20     -20        1 <NA>       <NA>     
        ##  8  5451       3    -20     -20        1 <NA>       <NA>     
        ##  9  6135       3    -20     -20        1 <NA>       <NA>     
        ## 10  6807       3    -20     -20        1 <NA>       <NA>     
        ## # ... with 147,077 more rows

4.  Recode ethnicity into the following groups: white British, Other
    White, Indian, Pakistani, other. (This classification doesn’t make
    much sense, but we’re doing this as an exercise).
    
    ``` r
    table(Data$racel_dv)
    ```
    
        ## 
        ##    -9     1     2     3     4     5     6     7     8     9    10    11    12 
        ## 47639 75868  2085    32  3342   638   263   399   364  3650  3241  2011   506 
        ##    13    14    15    16    17    97 
        ##  1069  1938  2766   200   514   562
    
    ``` r
    # using recode
    
    Data <- Data %>%
        mutate(ethnRecoded = recode(racel_dv,
                `1` = "white British",
                `2` = "other White",
                `3` = "other White",
                `4` = "other White",
                `9` = "Indian",
                `10` = "Pakistani",
                `-9` = NA_character_,
                # .default is for all other values
                .default = "other"
        )) %>%
        # let's make this new variable a factor
        mutate(ethnRecoded = factor(ethnRecoded))
    
    # checking that the recoding was correct
    Data %>%
        count(racel_dv, ethnRecoded)
    ```
    
        ## Warning: Factor `ethnRecoded` contains implicit NA, consider using
        ## `forcats::fct_explicit_na`
    
        ## # A tibble: 19 x 3
        ##    racel_dv ethnRecoded       n
        ##       <dbl> <fct>         <int>
        ##  1       -9 <NA>          47639
        ##  2        1 white British 75868
        ##  3        2 other White    2085
        ##  4        3 other White      32
        ##  5        4 other White    3342
        ##  6        5 other           638
        ##  7        6 other           263
        ##  8        7 other           399
        ##  9        8 other           364
        ## 10        9 Indian         3650
        ## 11       10 Pakistani      3241
        ## 12       11 other          2011
        ## 13       12 other           506
        ## 14       13 other          1069
        ## 15       14 other          1938
        ## 16       15 other          2766
        ## 17       16 other           200
        ## 18       17 other           514
        ## 19       97 other           562
    
    ``` r
    # we could do the same recoding with case_when
    
    Data %>%
        mutate(ethnRecoded = case_when(
                racel_dv == 1 ~ "white British",
                between(racel_dv, 2, 4) ~ "other White",
                racel_dv == 9 ~ "Indian",
                racel_dv == 10 ~ "Pakistani",
                (racel_dv >= 5 & racel_dv <= 8) | racel_dv > 10 ~ "other",
                racel_dv == -9 ~ NA_character_
        )) %>%
        count(racel_dv, ethnRecoded)
    ```
    
        ## # A tibble: 19 x 3
        ##    racel_dv ethnRecoded       n
        ##       <dbl> <chr>         <int>
        ##  1       -9 <NA>          47639
        ##  2        1 white British 75868
        ##  3        2 other White    2085
        ##  4        3 other White      32
        ##  5        4 other White    3342
        ##  6        5 other           638
        ##  7        6 other           263
        ##  8        7 other           399
        ##  9        8 other           364
        ## 10        9 Indian         3650
        ## 11       10 Pakistani      3241
        ## 12       11 other          2011
        ## 13       12 other           506
        ## 14       13 other          1069
        ## 15       14 other          1938
        ## 16       15 other          2766
        ## 17       16 other           200
        ## 18       17 other           514
        ## 19       97 other           562

5.  Count the number of people belonging to different ethnic groups (and
    produce percentages).
    
    ``` r
    Data %>%
      count(ethnRecoded) %>%
      mutate(perc = n / sum(n) * 100)
    ```
    
        ## Warning: Factor `ethnRecoded` contains implicit NA, consider using
        ## `forcats::fct_explicit_na`
    
        ## # A tibble: 6 x 3
        ##   ethnRecoded       n  perc
        ##   <fct>         <int> <dbl>
        ## 1 Indian         3650  2.48
        ## 2 other         11230  7.63
        ## 3 other White    5459  3.71
        ## 4 Pakistani      3241  2.20
        ## 5 white British 75868 51.6 
        ## 6 <NA>          47639 32.4

6.  Summarise the proportion of white British by generation.
    
    ``` r
    Data %>%
      filter(racel_dv != -9) %>%
      mutate(whiteBritish = if_else(racel_dv == 1, 1, 0)) %>%
      group_by(generation) %>%
      summarise(
    propWhiteBritish = mean(whiteBritish, na.rm = TRUE) * 100
      )
    ```
    
        ## # A tibble: 7 x 2
        ##   generation        propWhiteBritish
        ##   <chr>                        <dbl>
        ## 1 Baby Boomers                  80.7
        ## 2 Generation X                  64.7
        ## 3 Generation Z                  61.1
        ## 4 GI Generation                 92.5
        ## 5 Millenials                    64.2
        ## 6 Silent Generation             87.0
        ## 7 <NA>                          93.0

7.  Summarise the percentage of women by birth year.
    
    ``` r
    Data %>%
        filter(doby_dv > 0) %>%
        mutate(female = ifelse(sex_dv == 2, TRUE,
                                   ifelse(sex_dv == 1, FALSE, NA))) %>%
        group_by(doby_dv) %>%
        summarise(
                propFemale = mean(female, na.rm = TRUE) * 100
                )
    ```
    
        ## # A tibble: 111 x 2
        ##    doby_dv propFemale
        ##      <dbl>      <dbl>
        ##  1    1908      100  
        ##  2    1910      100  
        ##  3    1911       25  
        ##  4    1912       76.9
        ##  5    1913       72.2
        ##  6    1914       80  
        ##  7    1915       40.9
        ##  8    1916       73.8
        ##  9    1917       60  
        ## 10    1918       66.7
        ## # ... with 101 more rows
