Data Analysis in Social Science (25 January)
================

## Exercise 1.

Open the xwavedat.tab data set in the ukhls\_wx folder. What data does
it contain?

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

df <- read_tsv("UKDA-6614-tab/tab/ukhls_wx/xwavedat.tab")
```

    ## 
    ## ── Column specification ────────────────────────────────────────────────────────
    ## cols(
    ##   .default = col_double()
    ## )
    ## ℹ Use `spec()` for the full column specifications.

``` r
# or

df <- vroom("UKDA-6614-tab/tab/ukhls_wx/xwavedat.tab")
```

    ## Rows: 148,337
    ## Columns: 167
    ## Delimiter: "\t"
    ## dbl [167]: pidp, pid, xwdat_dv, hhorig, memorig, memorig_bh, quarter, psu, strata, sampst, p...
    ## 
    ## Use `spec()` to retrieve the guessed column specification
    ## Pass a specification to the `col_types` argument to quiet this message

## Exercise 2.

Open the j\_egoalt data set in the ukhls\_w10 folder. What data does it
contain?

``` r
jegoalt <- read_tsv("UKDA-6614-tab/tab/ukhls_w10/j_egoalt.tab")
```

    ## 
    ## ── Column specification ────────────────────────────────────────────────────────
    ## cols(
    ##   pidp = col_double(),
    ##   j_hidp = col_double(),
    ##   j_pno = col_double(),
    ##   apidp = col_double(),
    ##   j_apno = col_double(),
    ##   j_hhorig = col_double(),
    ##   j_memorig = col_double(),
    ##   j_psu = col_double(),
    ##   j_strata = col_double(),
    ##   j_sampst = col_double(),
    ##   j_sex = col_double(),
    ##   j_relationship = col_double(),
    ##   j_rel_dv = col_double(),
    ##   j_relationship_dv = col_double(),
    ##   j_asex = col_double(),
    ##   pid = col_double(),
    ##   apid = col_double(),
    ##   j_alwstat = col_double(),
    ##   j_elwstat = col_double()
    ## )

## Exercise 3.

Open the text of Hamlet in R:
<https://www.gutenberg.org/files/27761/27761-h/27761-h.htm>

The first row should contain “Ber. Who’s there?”

``` r
Hamlet <- read_lines("https://www.gutenberg.org/files/27761/27761-h/27761-h.htm")
Hamlet[1:10]
```

    ##  [1] "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\""                                
    ##  [2] "    \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\">"                                
    ##  [3] ""                                                                                          
    ##  [4] "<html xmlns=\"http://www.w3.org/1999/xhtml\">"                                             
    ##  [5] "  <head>"                                                                                  
    ##  [6] "    <title>"                                                                               
    ##  [7] "      The Project Gutenberg eBook of Hamlet by William Shakespeare, edited by Charles Kean"
    ##  [8] "    </title>"                                                                              
    ##  [9] "    <meta http-equiv=\"Content-Type\" content=\"text/html;charset=UTF-8\" />"              
    ## [10] "<style type = \"text/css\">"

``` r
# "Ber. Who's there?" in line 553

Hamlet2 <- read_lines("https://www.gutenberg.org/files/27761/27761-h/27761-h.htm",
                     skip = 552)
Hamlet2[1:10]
```

    ##  [1] "<p><i>Ber.</i> Who's there?</p>"                                                                                     
    ##  [2] ""                                                                                                                    
    ##  [3] "<p><i>Fran.</i> (<span class=\"smallroman\">R.</span>) Nay, answer me:<a class=\"tag\" name=\"tagI_1\" id=\"tagI_1\""
    ##  [4] "href=\"#noteI_1\">1</a> stand, and unfold<a class=\"tag\" name=\"tagI_2\" id=\"tagI_2\""                             
    ##  [5] "href=\"#noteI_2\">2</a> yourself.</p>"                                                                               
    ##  [6] ""                                                                                                                    
    ##  [7] "<div class=\"verse\">"                                                                                               
    ##  [8] "<p><i>Ber.</i> Long live the king!<a class=\"tag\" name=\"tagI_3\" id=\"tagI_3\""                                    
    ##  [9] "href=\"#noteI_3\">3</a></p>"                                                                                         
    ## [10] "</div>"

``` r
# This example is just to demonstrate that you can read different types of data into R. 
```
