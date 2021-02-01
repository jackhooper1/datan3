Data Analysis in Social Science (18 January)
================

``` r
knitr::opts_chunk$set(echo = TRUE)
knitr::opts_chunk$set(warning = FALSE)
knitr::opts_chunk$set(message = FALSE)
```

## Class on 18 January

Today we will work with the data set USArrests that is a build in data
set in R.

``` r
data(USArrests)
head(USArrests)
```

    ##            Murder Assault UrbanPop Rape
    ## Alabama      13.2     236       58 21.2
    ## Alaska       10.0     263       48 44.5
    ## Arizona       8.1     294       80 31.0
    ## Arkansas      8.8     190       50 19.5
    ## California    9.0     276       91 40.6
    ## Colorado      7.9     204       78 38.7

## Your tasks for today.

1.  Extract a vector with the names of the states.

<!-- end list -->

``` r
USArrests
```

    ##                Murder Assault UrbanPop Rape
    ## Alabama          13.2     236       58 21.2
    ## Alaska           10.0     263       48 44.5
    ## Arizona           8.1     294       80 31.0
    ## Arkansas          8.8     190       50 19.5
    ## California        9.0     276       91 40.6
    ## Colorado          7.9     204       78 38.7
    ## Connecticut       3.3     110       77 11.1
    ## Delaware          5.9     238       72 15.8
    ## Florida          15.4     335       80 31.9
    ## Georgia          17.4     211       60 25.8
    ## Hawaii            5.3      46       83 20.2
    ## Idaho             2.6     120       54 14.2
    ## Illinois         10.4     249       83 24.0
    ## Indiana           7.2     113       65 21.0
    ## Iowa              2.2      56       57 11.3
    ## Kansas            6.0     115       66 18.0
    ## Kentucky          9.7     109       52 16.3
    ## Louisiana        15.4     249       66 22.2
    ## Maine             2.1      83       51  7.8
    ## Maryland         11.3     300       67 27.8
    ## Massachusetts     4.4     149       85 16.3
    ## Michigan         12.1     255       74 35.1
    ## Minnesota         2.7      72       66 14.9
    ## Mississippi      16.1     259       44 17.1
    ## Missouri          9.0     178       70 28.2
    ## Montana           6.0     109       53 16.4
    ## Nebraska          4.3     102       62 16.5
    ## Nevada           12.2     252       81 46.0
    ## New Hampshire     2.1      57       56  9.5
    ## New Jersey        7.4     159       89 18.8
    ## New Mexico       11.4     285       70 32.1
    ## New York         11.1     254       86 26.1
    ## North Carolina   13.0     337       45 16.1
    ## North Dakota      0.8      45       44  7.3
    ## Ohio              7.3     120       75 21.4
    ## Oklahoma          6.6     151       68 20.0
    ## Oregon            4.9     159       67 29.3
    ## Pennsylvania      6.3     106       72 14.9
    ## Rhode Island      3.4     174       87  8.3
    ## South Carolina   14.4     279       48 22.5
    ## South Dakota      3.8      86       45 12.8
    ## Tennessee        13.2     188       59 26.9
    ## Texas            12.7     201       80 25.5
    ## Utah              3.2     120       80 22.9
    ## Vermont           2.2      48       32 11.2
    ## Virginia          8.5     156       63 20.7
    ## Washington        4.0     145       73 26.2
    ## West Virginia     5.7      81       39  9.3
    ## Wisconsin         2.6      53       66 10.8
    ## Wyoming           6.8     161       60 15.6

``` r
str(USArrests)
```

    ## 'data.frame':    50 obs. of  4 variables:
    ##  $ Murder  : num  13.2 10 8.1 8.8 9 7.9 3.3 5.9 15.4 17.4 ...
    ##  $ Assault : int  236 263 294 190 276 204 110 238 335 211 ...
    ##  $ UrbanPop: int  58 48 80 50 91 78 77 72 80 60 ...
    ##  $ Rape    : num  21.2 44.5 31 19.5 40.6 38.7 11.1 15.8 31.9 25.8 ...

``` r
stateNames <- row.names(USArrests)
stateNames
```

    ##  [1] "Alabama"        "Alaska"         "Arizona"        "Arkansas"      
    ##  [5] "California"     "Colorado"       "Connecticut"    "Delaware"      
    ##  [9] "Florida"        "Georgia"        "Hawaii"         "Idaho"         
    ## [13] "Illinois"       "Indiana"        "Iowa"           "Kansas"        
    ## [17] "Kentucky"       "Louisiana"      "Maine"          "Maryland"      
    ## [21] "Massachusetts"  "Michigan"       "Minnesota"      "Mississippi"   
    ## [25] "Missouri"       "Montana"        "Nebraska"       "Nevada"        
    ## [29] "New Hampshire"  "New Jersey"     "New Mexico"     "New York"      
    ## [33] "North Carolina" "North Dakota"   "Ohio"           "Oklahoma"      
    ## [37] "Oregon"         "Pennsylvania"   "Rhode Island"   "South Carolina"
    ## [41] "South Dakota"   "Tennessee"      "Texas"          "Utah"          
    ## [45] "Vermont"        "Virginia"       "Washington"     "West Virginia" 
    ## [49] "Wisconsin"      "Wyoming"

``` r
class(stateNames)
```

    ## [1] "character"

``` r
USArrests$stateNames <- row.names(USArrests)
USArrests
```

    ##                Murder Assault UrbanPop Rape     stateNames
    ## Alabama          13.2     236       58 21.2        Alabama
    ## Alaska           10.0     263       48 44.5         Alaska
    ## Arizona           8.1     294       80 31.0        Arizona
    ## Arkansas          8.8     190       50 19.5       Arkansas
    ## California        9.0     276       91 40.6     California
    ## Colorado          7.9     204       78 38.7       Colorado
    ## Connecticut       3.3     110       77 11.1    Connecticut
    ## Delaware          5.9     238       72 15.8       Delaware
    ## Florida          15.4     335       80 31.9        Florida
    ## Georgia          17.4     211       60 25.8        Georgia
    ## Hawaii            5.3      46       83 20.2         Hawaii
    ## Idaho             2.6     120       54 14.2          Idaho
    ## Illinois         10.4     249       83 24.0       Illinois
    ## Indiana           7.2     113       65 21.0        Indiana
    ## Iowa              2.2      56       57 11.3           Iowa
    ## Kansas            6.0     115       66 18.0         Kansas
    ## Kentucky          9.7     109       52 16.3       Kentucky
    ## Louisiana        15.4     249       66 22.2      Louisiana
    ## Maine             2.1      83       51  7.8          Maine
    ## Maryland         11.3     300       67 27.8       Maryland
    ## Massachusetts     4.4     149       85 16.3  Massachusetts
    ## Michigan         12.1     255       74 35.1       Michigan
    ## Minnesota         2.7      72       66 14.9      Minnesota
    ## Mississippi      16.1     259       44 17.1    Mississippi
    ## Missouri          9.0     178       70 28.2       Missouri
    ## Montana           6.0     109       53 16.4        Montana
    ## Nebraska          4.3     102       62 16.5       Nebraska
    ## Nevada           12.2     252       81 46.0         Nevada
    ## New Hampshire     2.1      57       56  9.5  New Hampshire
    ## New Jersey        7.4     159       89 18.8     New Jersey
    ## New Mexico       11.4     285       70 32.1     New Mexico
    ## New York         11.1     254       86 26.1       New York
    ## North Carolina   13.0     337       45 16.1 North Carolina
    ## North Dakota      0.8      45       44  7.3   North Dakota
    ## Ohio              7.3     120       75 21.4           Ohio
    ## Oklahoma          6.6     151       68 20.0       Oklahoma
    ## Oregon            4.9     159       67 29.3         Oregon
    ## Pennsylvania      6.3     106       72 14.9   Pennsylvania
    ## Rhode Island      3.4     174       87  8.3   Rhode Island
    ## South Carolina   14.4     279       48 22.5 South Carolina
    ## South Dakota      3.8      86       45 12.8   South Dakota
    ## Tennessee        13.2     188       59 26.9      Tennessee
    ## Texas            12.7     201       80 25.5          Texas
    ## Utah              3.2     120       80 22.9           Utah
    ## Vermont           2.2      48       32 11.2        Vermont
    ## Virginia          8.5     156       63 20.7       Virginia
    ## Washington        4.0     145       73 26.2     Washington
    ## West Virginia     5.7      81       39  9.3  West Virginia
    ## Wisconsin         2.6      53       66 10.8      Wisconsin
    ## Wyoming           6.8     161       60 15.6        Wyoming

2.  Create a new data set that only includes states with the murder rate
    \> 10.

<!-- end list -->

``` r
USArrests
```

    ##                Murder Assault UrbanPop Rape     stateNames
    ## Alabama          13.2     236       58 21.2        Alabama
    ## Alaska           10.0     263       48 44.5         Alaska
    ## Arizona           8.1     294       80 31.0        Arizona
    ## Arkansas          8.8     190       50 19.5       Arkansas
    ## California        9.0     276       91 40.6     California
    ## Colorado          7.9     204       78 38.7       Colorado
    ## Connecticut       3.3     110       77 11.1    Connecticut
    ## Delaware          5.9     238       72 15.8       Delaware
    ## Florida          15.4     335       80 31.9        Florida
    ## Georgia          17.4     211       60 25.8        Georgia
    ## Hawaii            5.3      46       83 20.2         Hawaii
    ## Idaho             2.6     120       54 14.2          Idaho
    ## Illinois         10.4     249       83 24.0       Illinois
    ## Indiana           7.2     113       65 21.0        Indiana
    ## Iowa              2.2      56       57 11.3           Iowa
    ## Kansas            6.0     115       66 18.0         Kansas
    ## Kentucky          9.7     109       52 16.3       Kentucky
    ## Louisiana        15.4     249       66 22.2      Louisiana
    ## Maine             2.1      83       51  7.8          Maine
    ## Maryland         11.3     300       67 27.8       Maryland
    ## Massachusetts     4.4     149       85 16.3  Massachusetts
    ## Michigan         12.1     255       74 35.1       Michigan
    ## Minnesota         2.7      72       66 14.9      Minnesota
    ## Mississippi      16.1     259       44 17.1    Mississippi
    ## Missouri          9.0     178       70 28.2       Missouri
    ## Montana           6.0     109       53 16.4        Montana
    ## Nebraska          4.3     102       62 16.5       Nebraska
    ## Nevada           12.2     252       81 46.0         Nevada
    ## New Hampshire     2.1      57       56  9.5  New Hampshire
    ## New Jersey        7.4     159       89 18.8     New Jersey
    ## New Mexico       11.4     285       70 32.1     New Mexico
    ## New York         11.1     254       86 26.1       New York
    ## North Carolina   13.0     337       45 16.1 North Carolina
    ## North Dakota      0.8      45       44  7.3   North Dakota
    ## Ohio              7.3     120       75 21.4           Ohio
    ## Oklahoma          6.6     151       68 20.0       Oklahoma
    ## Oregon            4.9     159       67 29.3         Oregon
    ## Pennsylvania      6.3     106       72 14.9   Pennsylvania
    ## Rhode Island      3.4     174       87  8.3   Rhode Island
    ## South Carolina   14.4     279       48 22.5 South Carolina
    ## South Dakota      3.8      86       45 12.8   South Dakota
    ## Tennessee        13.2     188       59 26.9      Tennessee
    ## Texas            12.7     201       80 25.5          Texas
    ## Utah              3.2     120       80 22.9           Utah
    ## Vermont           2.2      48       32 11.2        Vermont
    ## Virginia          8.5     156       63 20.7       Virginia
    ## Washington        4.0     145       73 26.2     Washington
    ## West Virginia     5.7      81       39  9.3  West Virginia
    ## Wisconsin         2.6      53       66 10.8      Wisconsin
    ## Wyoming           6.8     161       60 15.6        Wyoming

``` r
USArrests[, 5]
```

    ##  [1] "Alabama"        "Alaska"         "Arizona"        "Arkansas"      
    ##  [5] "California"     "Colorado"       "Connecticut"    "Delaware"      
    ##  [9] "Florida"        "Georgia"        "Hawaii"         "Idaho"         
    ## [13] "Illinois"       "Indiana"        "Iowa"           "Kansas"        
    ## [17] "Kentucky"       "Louisiana"      "Maine"          "Maryland"      
    ## [21] "Massachusetts"  "Michigan"       "Minnesota"      "Mississippi"   
    ## [25] "Missouri"       "Montana"        "Nebraska"       "Nevada"        
    ## [29] "New Hampshire"  "New Jersey"     "New Mexico"     "New York"      
    ## [33] "North Carolina" "North Dakota"   "Ohio"           "Oklahoma"      
    ## [37] "Oregon"         "Pennsylvania"   "Rhode Island"   "South Carolina"
    ## [41] "South Dakota"   "Tennessee"      "Texas"          "Utah"          
    ## [45] "Vermont"        "Virginia"       "Washington"     "West Virginia" 
    ## [49] "Wisconsin"      "Wyoming"

``` r
USArrests[USArrests$Murder > 10,]
```

    ##                Murder Assault UrbanPop Rape     stateNames
    ## Alabama          13.2     236       58 21.2        Alabama
    ## Florida          15.4     335       80 31.9        Florida
    ## Georgia          17.4     211       60 25.8        Georgia
    ## Illinois         10.4     249       83 24.0       Illinois
    ## Louisiana        15.4     249       66 22.2      Louisiana
    ## Maryland         11.3     300       67 27.8       Maryland
    ## Michigan         12.1     255       74 35.1       Michigan
    ## Mississippi      16.1     259       44 17.1    Mississippi
    ## Nevada           12.2     252       81 46.0         Nevada
    ## New Mexico       11.4     285       70 32.1     New Mexico
    ## New York         11.1     254       86 26.1       New York
    ## North Carolina   13.0     337       45 16.1 North Carolina
    ## South Carolina   14.4     279       48 22.5 South Carolina
    ## Tennessee        13.2     188       59 26.9      Tennessee
    ## Texas            12.7     201       80 25.5          Texas

``` r
subset(USArrests, USArrests$Murder > 10)
```

    ##                Murder Assault UrbanPop Rape     stateNames
    ## Alabama          13.2     236       58 21.2        Alabama
    ## Florida          15.4     335       80 31.9        Florida
    ## Georgia          17.4     211       60 25.8        Georgia
    ## Illinois         10.4     249       83 24.0       Illinois
    ## Louisiana        15.4     249       66 22.2      Louisiana
    ## Maryland         11.3     300       67 27.8       Maryland
    ## Michigan         12.1     255       74 35.1       Michigan
    ## Mississippi      16.1     259       44 17.1    Mississippi
    ## Nevada           12.2     252       81 46.0         Nevada
    ## New Mexico       11.4     285       70 32.1     New Mexico
    ## New York         11.1     254       86 26.1       New York
    ## North Carolina   13.0     337       45 16.1 North Carolina
    ## South Carolina   14.4     279       48 22.5 South Carolina
    ## Tennessee        13.2     188       59 26.9      Tennessee
    ## Texas            12.7     201       80 25.5          Texas

3.  Create a new data set that only includes the first 15 states in the
    original data set.

<!-- end list -->

``` r
USArrests[1:15,]
```

    ##             Murder Assault UrbanPop Rape  stateNames
    ## Alabama       13.2     236       58 21.2     Alabama
    ## Alaska        10.0     263       48 44.5      Alaska
    ## Arizona        8.1     294       80 31.0     Arizona
    ## Arkansas       8.8     190       50 19.5    Arkansas
    ## California     9.0     276       91 40.6  California
    ## Colorado       7.9     204       78 38.7    Colorado
    ## Connecticut    3.3     110       77 11.1 Connecticut
    ## Delaware       5.9     238       72 15.8    Delaware
    ## Florida       15.4     335       80 31.9     Florida
    ## Georgia       17.4     211       60 25.8     Georgia
    ## Hawaii         5.3      46       83 20.2      Hawaii
    ## Idaho          2.6     120       54 14.2       Idaho
    ## Illinois      10.4     249       83 24.0    Illinois
    ## Indiana        7.2     113       65 21.0     Indiana
    ## Iowa           2.2      56       57 11.3        Iowa

4.  Knit your document as pdf, html and Word files.

5.  Change the document settings so that the code is not displayed, and
    messages and warnings are not shown. Set cache to TRUE. When do you
    want to use cache? Can you change these settings for individual
    chunks of code?
