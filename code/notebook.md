Group Assignment Trimester 1 25/26
================
2025-09-16

You are a consultant for Pacific Insights Analytics (PIA), a boutique
data analytics firm specializing in workforce performance. PIA has been
hired by Huaxia Communications, a major Chinese company operating
several large call centers across the country. Huaxia is facing rising
operational challenges. Staff attrition is driving up recruitment and
training costs and retaining top performers is becoming increasingly
difficult. To address these issues, Huaxia has provided you with
historical data from one of its call centers. This data comes from
multiple administrative sources, including worker surveys, HR records,
and call center productivity logs. Because the data was collected from
different systems, it contains inconsistencies, formatting problems, and
other quality issues. Your task is to clean, explore, and analyze the
data in order to generate clear, evidence-based recommendations that can
help Huaxia’s management improve retention and boost productivity.
Important: You should not run any regression models. The goal is to
understand the data and identify patterns that can inform management
decisions through EDA and hypothesis testing.

## Packages

``` r
library(tidyverse)
```

    ## ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
    ## ✔ dplyr     1.1.4     ✔ readr     2.1.5
    ## ✔ forcats   1.0.0     ✔ stringr   1.5.1
    ## ✔ ggplot2   3.5.2     ✔ tibble    3.3.0
    ## ✔ lubridate 1.9.4     ✔ tidyr     1.3.1
    ## ✔ purrr     1.1.0     
    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()
    ## ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors

``` r
library(haven)
library(corrr)
library(purrr)
library(glue)
library(dplyr)
library(tidyr)
library(ggplot2)
library(ggcorrplot)
library(glue)
library(patchwork)
library(lubridate)
library(ISOweek)
library(skimr)
```

    ## 
    ## Attaching package: 'skimr'
    ## 
    ## The following object is masked from 'package:corrr':
    ## 
    ##     focus

``` r
library(stargazer)
```

    ## 
    ## Please cite as: 
    ## 
    ##  Hlavac, Marek (2022). stargazer: Well-Formatted Regression and Summary Statistics Tables.
    ##  R package version 5.2.3. https://CRAN.R-project.org/package=stargazer

## Loading the data

``` r
data_perf <- read_dta("../data/performance_labeled.dta")
data_attitude <- read_dta("../data/attitude_labeled.dta")
data_endperiod <- read_dta("../data/endperiod_outcomes_labeled.dta")
data_sum_vol <- read_dta("../data/summary_volunteer_labeled.dta")
data_wage_new <- read_dta("../data/wage_new_labeled.dta")

performance <- as_tibble(data_perf)
attitude <- as_tibble(data_attitude)
endperiod <- as_tibble(data_endperiod)
volunteer <- as_tibble(data_sum_vol)
wage <- as_tibble(data_wage_new)

rm(data_perf)
rm(data_attitude)
rm(data_endperiod)
rm(data_sum_vol)
rm(data_wage_new)
```

# Cleaning data

Here come some handy functions to reduce duplicate code for cleaning the
data:

Extract a numeric number from a string with different decimal
separators:

``` r
extract_numeric <- function(x) {
  # remove leading/trailing spaces
  x <- trimws(x)
  
  # normalize dash types (en-dash, em-dash) to minus
  x <- gsub("–|—", "-", x)  
  
  # allow scientific notation: replace commas in mantissa
  if (grepl("[eE]", x)) {
    # if comma before e → decimal comma
    x <- sub(",", ".", x)
    # remove thousand separators before e
    x <- gsub("\\.(?=\\d{3}[eE])", "", x, perl = TRUE)
    x <- gsub(",(?=\\d{3}[eE])", "", x, perl = TRUE)
    return(as.numeric(x))
  }
  
  # remove spaces
  x <- gsub(" ", "", x)
  
  # case 1: comma used as decimal
  if (grepl(",", x) & !grepl("\\.", x)) {
    x <- gsub(",", ".", x)
  }
  
  # case 2: both comma and dot appear
  # assume comma is thousands separator, remove it
  if (grepl(",.*\\.", x)) {
    x <- gsub(",", "", x)
  }
  
  as.numeric(x)
}

find_non_numeric_chars <- function(x) {
  non_numeric <- gsub("[-0-9]", "", x)  # remove digits, minus, dot, comma
  unique(non_numeric[non_numeric != ""])
}

find_non_numeric_rows <- function(x) {
  non_numeric <- !grepl("^[-]?[0-9]+([0-9]+)?$",x) # remove digits, minus, dot, comma
  non_numeric <- non_numeric[non_numeric != ""]
  x[non_numeric]
}
```

## Clean volunteer

Rows: 140 Columns: 9 \$ personid <chr> “33350”, “40034”, “31292”,
“21654”, “36908”, “13980”, “44794… \$ age <dbl> 26, 19, 27, 29, 23, 1,
23, 25, 21, 23, 23, 30, 22, 22, 23, 2… \$ tenure <dbl> 22, 9, 24, 37,
15, 51, 3, 42, 3, 34, 24, 25, 3, 66, 9, 70, 4… \$ children <chr>”0”,
“0”, “0”, “0”, “0”, “0”, “0”, “0”, “0”, “0”, “0”, “0”, … \$ bedroom
<chr> “1”, “1”, “1”, “1”, “1”, “1”, “1”, “1”, “1”, “1”, “1”, “1”, … \$
commute <chr> “120”, “300”, “135”, “80”, “60”, ” 120”, “40 min”, “60”,
“1… \$ gender <chr>”m”, “woman”, “F”, “f”, “m”, “woman”, “male”,
“woman”, “MALE… \$ married <chr>”SINGLE”, “N”, “NO”, “N”, “NO”, ”
single”, ” N”, “NO”, “sin… \$ high_educ <chr>”Y”, “no”, “YES”, “YES”,
“N”, “NO”, “n”, “YES”, “no”, “Y”, “…

================================================== Statistic N Mean
St. Dev. Min Median Max  
————————————————– age 140 23.536 4.352 1 23 35  
tenure 140 22.971 24.296 2.000 13.000 150.000 ————————————————– \[1\] ”
” ” min” ” MIN” ” ” ” minute” ” minutes” “. h”  
\[8\] ” MIN”  
\[1\] ” 120” “40 min” ” 50 MIN” ” 170” ” 210”  
\[6\] “40 minute” “240 minutes” “1.2 h” ” 180” ” 90 MIN”  
\[11\] “40 minute” “30 minutes” ” 180” “120 MIN” ” 120”  
\[16\] “50 min” “100 min” ” 120” “60 minutes” “1.2 h”  
\[21\] “180 minutes” ” 150” “2.3 h”

We can see a lot of columns with numerical values within strstrings and
additional chars. Furthermore, categorical variables like married and
gender contain different labels for the same categories.

``` r
# convert to integer to remove leading zeros an than to str
volunteer$personid <- as.character(as.integer(volunteer$personid))
# Clean Numbers with extra characters
# Remove non-numeric characters, except dots and commas and convert to numeric
volunteer[["commute"]] <- sapply(volunteer$commute, extract_numeric)
```

    ## Warning in FUN(X[[i]], ...): NAs introduced by coercion
    ## Warning in FUN(X[[i]], ...): NAs introduced by coercion
    ## Warning in FUN(X[[i]], ...): NAs introduced by coercion
    ## Warning in FUN(X[[i]], ...): NAs introduced by coercion
    ## Warning in FUN(X[[i]], ...): NAs introduced by coercion
    ## Warning in FUN(X[[i]], ...): NAs introduced by coercion
    ## Warning in FUN(X[[i]], ...): NAs introduced by coercion
    ## Warning in FUN(X[[i]], ...): NAs introduced by coercion
    ## Warning in FUN(X[[i]], ...): NAs introduced by coercion
    ## Warning in FUN(X[[i]], ...): NAs introduced by coercion
    ## Warning in FUN(X[[i]], ...): NAs introduced by coercion
    ## Warning in FUN(X[[i]], ...): NAs introduced by coercion
    ## Warning in FUN(X[[i]], ...): NAs introduced by coercion
    ## Warning in FUN(X[[i]], ...): NAs introduced by coercion
    ## Warning in FUN(X[[i]], ...): NAs introduced by coercion

``` r
volunteer[["commute"]] <- as.numeric(volunteer[["commute"]])

# convert "no" strings in the children column to 0 children
volunteer[["children"]] <- ifelse(volunteer[["children"]] %in% c("No", ""), "0", volunteer[["children"]])
# convert to boolean by first converting to integer
volunteer[["children"]] <- as.logical(as.integer(volunteer[["children"]]))
# Convert Tenure to integer, as float dont make sense for months
volunteer[["tenure"]] <- as.integer(volunteer[["tenure"]])

# convert bedroom to boolean by first converting 0s to False, 1 to true
volunteer[["bedroom"]]<- ifelse(
    volunteer[["bedroom"]] == "0", 
    0, 
    1
    )
volunteer[["bedroom"]] <- as.integer(volunteer[["bedroom"]])
# normalize gender data by
# lower all charcters
volunteer[["gender"]] <- tolower(volunteer[["gender"]])
# trim whitespaces
volunteer[["gender"]] <- trimws(volunteer[["gender"]])
# cast all possible options for male and female
volunteer[["gender"]] <- ifelse(volunteer[["gender"]] %in% c("m", "male", "man"), "male", volunteer[["gender"]])
volunteer[["gender"]] <- ifelse(volunteer[["gender"]] %in% c("woman", "female", "f", "w","fem"), "female", volunteer[["gender"]])

# using a similar approach to the gender we convert the married status now
volunteer[["married"]] <- tolower(volunteer[["married"]])
# trim whitespaces
volunteer[["married"]] <- trimws(volunteer[["married"]])
# cast all possible options for married/not married
volunteer[["married"]] <- ifelse(volunteer[["married"]] %in% c("married", "yes", "y", "true"), "TRUE", volunteer[["married"]])
volunteer[["married"]] <- ifelse(volunteer[["married"]] %in% c("no", "not married", "n", "na", "single","false"), "FALSE", volunteer[["married"]])

# using a similar approach to the married column we convert the high_educ status now
volunteer[["high_educ"]] <- tolower(volunteer[["high_educ"]])
# trim whitespaces
volunteer[["high_educ"]] <- trimws(volunteer[["high_educ"]])
# cast all possible options for high_educ/not high_educ
volunteer[["high_educ"]] <- ifelse(volunteer[["high_educ"]] %in% c("yes", "y"), "TRUE", volunteer[["high_educ"]])
volunteer[["high_educ"]] <- ifelse(volunteer[["high_educ"]] %in% c("no", "n","false"), "FALSE", volunteer[["high_educ"]])

# convert to boolean
volunteer[["high_educ"]] <- as.logical(volunteer[["high_educ"]])

# create numerical variable cols for gender and married
volunteer$gender_num <-as.integer(ifelse(volunteer$gender=="male",0,1))
volunteer$married_num <-as.integer(ifelse(volunteer$married==FALSE,0,1))
volunteer$high_educ_num <-as.integer(ifelse(volunteer$high_educ==FALSE,0,1))
volunteer$children_num <-as.integer(ifelse(volunteer$children==FALSE,0,1))

# remove duplicates on same personid
volunteer <- volunteer %>%
  distinct(personid, .keep_all = TRUE)
```

``` r
# Now we have much better insights using stargazer
stargazer(as.data.frame(volunteer), type = "text", median = TRUE, header = TRUE)
```

    ## 
    ## =================================================
    ## Statistic      N   Mean   St. Dev. Min Median Max
    ## -------------------------------------------------
    ## age           135 23.452   4.315    1    23   35 
    ## tenure        135 23.385   24.608   2    13   150
    ## children      135  0.148   0.357    0    0     1 
    ## bedroom       135  0.978   0.148    0    1     1 
    ## commute       122 104.721  68.544   1    85   300
    ## high_educ     135  0.415   0.495    0    0     1 
    ## gender_num    135  0.496   0.502    0    0     1 
    ## married_num   135  0.200   0.401    0    0     1 
    ## high_educ_num 135  0.415   0.495    0    0     1 
    ## children_num  135  0.148   0.357    0    0     1 
    ## -------------------------------------------------

## Clean attitude

``` r
glimpse(attitude)
```

    ## Rows: 2,379
    ## Columns: 5
    ## $ personid   <dbl> 4122, 4122, 4122, 4122, 4122, 4122, 4122, 4122, 4122, 4122,…
    ## $ year_week  <chr> "202249", "202250", "202251", "202252", "202253", "202302",…
    ## $ exhaustion <dbl> 9, 8, 8, 6, 12, 12, 12, 12, 10, 12, 11, 12, 12, 12, 14, 12,…
    ## $ negative   <dbl> 20, 21, 20, 17, 19, 18, 20, 16, 18, 24, 18, 19, 20, 19, 18,…
    ## $ positive   <dbl> 20, 25, 24, 22, 19, 19, 19, 22, 20, 23, 23, 22, 22, 24, 23,…

``` r
stargazer(as.data.frame(attitude), type = "text", median = TRUE, header = TRUE)
```

    ## 
    ## ==========================================================
    ## Statistic    N      Mean     St. Dev.   Min  Median  Max  
    ## ----------------------------------------------------------
    ## personid   2,379 33,005.050 10,331.780 4,122 36,288 45,442
    ## exhaustion 2,379   8.612      7.794    0.000 7.000  36.000
    ## negative   2,379   16.651     6.848    8.000 16.000 40.000
    ## positive   2,379   24.215     6.668    8.000 24.000 40.000
    ## ----------------------------------------------------------

``` r
unique(attitude[["exhausting"]])
```

    ## NULL

``` r
unique(attitude[["positive"]])
```

    ##  [1] 20.00000 25.00000 24.00000 22.00000 19.00000 23.00000 21.00000 18.00000
    ##  [9] 17.00000  8.00000 16.00000 32.00000 34.00000 30.00000 29.00000 31.00000
    ## [17] 24.24000 28.00000 22.56000 27.00000 26.00000 28.13000 33.00000 35.00000
    ## [25]  9.00000 12.00000 14.00000 13.00000 15.00000 27.19000 11.00000 10.00000
    ## [33] 38.00000 26.28000 36.00000 23.79000 23.48000 37.00000 39.00000 40.00000
    ## [41] 21.91000 22.13000 23.46000 27.18000 26.88000 27.11000 26.04000 25.04065
    ## [49] 27.05000

``` r
unique(attitude[["negative"]])
```

    ##  [1] 20.00000 21.00000 17.00000 19.00000 18.00000 16.00000 24.00000 28.00000
    ##  [9] 22.00000 26.00000 25.00000 27.00000 10.00000 15.00000 14.00000 13.00000
    ## [17] 17.55000 11.00000 12.00000 18.53000  9.00000 17.87716  8.00000 14.85000
    ## [25] 15.47000 23.00000 37.00000 16.22000 30.00000 17.98000 18.19000 32.00000
    ## [33] 29.00000 40.00000 18.77000 31.00000 19.75000 17.57000 14.89000 15.04000
    ## [41] 34.00000 39.00000 36.00000 35.00000 15.55000 15.94000 16.75610 15.10000

We see that we do not have missing values, and the data types for all
columns already fit. We only need to convert personid to a string.

``` r
# convert to integer to remove leading zeros an than to str
attitude$personid <- as.character(as.integer(attitude$personid))
# remove duplicates on same personid and year_week
attitude <- attitude %>%
  distinct(personid, year_week, .keep_all = TRUE)
```

## Clean endperiod

``` r
glimpse(endperiod)
```

    ## Rows: 135
    ## Columns: 4
    ## $ personid       <dbl> 4122, 6278, 7720, 8834, 8854, 10098, 10356, 12426, 1297…
    ## $ promote_switch <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 1, 1, 0, 0, 0, 1, 0…
    ## $ quitjob        <dbl> 0, 1, 0, 0, 0, 1, 0, 0, 0, 0, 0, 1, 0, 0, 0, 1, 1, 0, 0…
    ## $ costofcommute  <chr> "18", "12", "9", "0", "4", "12", "0", "0", "0", "6.00元"…

``` r
stargazer(as.data.frame(endperiod), type = "text", median = TRUE, header = TRUE)
```

    ## 
    ## ============================================================
    ## Statistic       N     Mean     St. Dev.   Min  Median  Max  
    ## ------------------------------------------------------------
    ## personid       135 32,716.780 10,835.700 4,122 37,292 45,442
    ## promote_switch 135   0.163      0.371      0     0      1   
    ## quitjob        135   0.296      0.458      0     0      1   
    ## ------------------------------------------------------------

``` r
# check character columns with numerical values to extract for non numeric chars to
# be aware for special cases
find_non_numeric_chars(endperiod$costofcommute)
```

    ##  [1] ".元"      ".元 /月"  "."        "人民币 ." "NA"       "CNY ."   
    ##  [7] "  "       ","        ". 元"     "¥."       " "

``` r
find_non_numeric_rows(endperiod$costofcommute)
```

    ##  [1] "6.00元"           "-"                "25.00元 /月"      "11.8181819915771"
    ##  [5] "人民币 14.00"     "NA"               "CNY 3.00"         "  6"             
    ##  [9] "2,00"             "17.7272720336914" "  0"              "4.54545450210571"
    ## [13] "2,00"             "0.00 元"          "¥10.00"           " 10"             
    ## [17] "0,00"

``` r
# here we only need to take care of the decimal dots (will be added to the extraction function), any other chars can be ignored
```

No missing values, promote_switch and quitjob are logical values, though
we also will create a numerical version (0-1) for them to compute
statistics. costofcommute need to be converted to doubles.

``` r
# convert to integer to remove leading zeros an than to str
endperiod$personid <- as.character(as.integer(endperiod$personid))
# convert to integers
endperiod[["promote_switch_num"]] <- as.integer(endperiod[["promote_switch"]])
endperiod[["quitjob_num"]] <- as.integer(endperiod[["quitjob"]])
# convert to logical (too have numerical and categorical variables)
endperiod[["promote_switch"]] <- as.logical(endperiod[["promote_switch_num"]])
endperiod[["quitjob"]] <- as.logical(endperiod[["quitjob_num"]])
# extract numbers from string. Both, dots and commas are used as decimal separators
endperiod[["costofcommute"]]<- gsub("[^0-9.,]", "", endperiod[["costofcommute"]])
endperiod[["costofcommute"]] <- as.numeric(endperiod[["costofcommute"]])
```

    ## Warning: NAs introduced by coercion

``` r
# remove duplicates on same personid
endperiod <- endperiod %>%
  distinct(personid, .keep_all = TRUE)
```

``` r
glimpse(endperiod)
```

    ## Rows: 135
    ## Columns: 6
    ## $ personid           <chr> "4122", "6278", "7720", "8834", "8854", "10098", "1…
    ## $ promote_switch     <lgl> FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FA…
    ## $ quitjob            <lgl> FALSE, TRUE, FALSE, FALSE, FALSE, TRUE, FALSE, FALS…
    ## $ costofcommute      <dbl> 18.00000, 12.00000, 9.00000, 0.00000, 4.00000, 12.0…
    ## $ promote_switch_num <int> 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 1, 1, 0, 0, 0, …
    ## $ quitjob_num        <int> 0, 1, 0, 0, 0, 1, 0, 0, 0, 0, 0, 1, 0, 0, 0, 1, 1, …

``` r
stargazer(as.data.frame(endperiod), type = "text", median = TRUE, header = TRUE)
```

    ## 
    ## =========================================================
    ## Statistic           N  Mean  St. Dev.  Min  Median  Max  
    ## ---------------------------------------------------------
    ## promote_switch     135 0.163  0.371     0     0      1   
    ## quitjob            135 0.296  0.458     0     0      1   
    ## costofcommute      130 7.555  7.283   0.000 6.000  55.000
    ## promote_switch_num 135 0.163  0.371     0     0      1   
    ## quitjob_num        135 0.296  0.458     0     0      1   
    ## ---------------------------------------------------------

Cost of commute has two missing values, which needs to be considered
during analysis.

## Clean performance

``` r
glimpse(performance)
```

    ## Rows: 9,870
    ## Columns: 12
    ## $ personid          <dbl> 4122, 4122, 4122, 4122, 4122, 4122, 4122, 4122, 4122…
    ## $ year_week         <chr> "202201", "202202", "202203", "202204", "202205", "2…
    ## $ perform1          <chr> "-1.14160859584808", "0.414592355489731", "1.0139772…
    ## $ phonecall         <chr> "-1.13219404220581", "0.312170952558517", "1.0971519…
    ## $ phonecallraw      <chr> "223", "499", "649", "709", "403", "760", "268", "73…
    ## $ homethatweek      <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0…
    ## $ logphonecall      <chr> "5.40717172622681", "6.21260595321655", "6.475432872…
    ## $ logcallpersec     <chr> "-5.10147857666016", "-5.15978050231934", "-5.142978…
    ## $ logcalllength     <chr> "10.5086498260498", "11.372386932373", "11.618411064…
    ## $ logcall_dayworked <chr> "9.41003799438477", "9.58062744140625", "9.826651573…
    ## $ logdaysworked     <chr> "1.0986123085022", "1.7917594909668", "1.79175949096…
    ## $ date              <chr> "2022-01-03", "2022-01-10", "2022-01-17", "2022-01-2…

``` r
stargazer(as.data.frame(performance), type = "text", median = TRUE, header = TRUE)
```

    ## 
    ## ============================================================
    ## Statistic      N      Mean     St. Dev.   Min  Median  Max  
    ## ------------------------------------------------------------
    ## personid     9,870 32,493.740 10,623.780 4,122 36,908 45,442
    ## homethatweek 9,870   0.196      0.397      0     0      1   
    ## ------------------------------------------------------------

``` r
unique(performance[["homethatweek"]])
```

    ## [1] 0 1

``` r
# check character columns with numerical values to extract for non numeric chars to
# be aware for special cases
find_non_numeric_chars(performance$perform1)
```

    ## [1] "."  "−." ".e"

``` r
#find_non_numeric_rows(performance$perform1)
find_non_numeric_chars(performance$phonecall)
```

    ## [1] "."  "−."

``` r
find_non_numeric_chars(performance$phonecallraw)
```

    ## [1] "NA" "  " " "

``` r
find_non_numeric_chars(performance$homethatweek)
```

    ## character(0)

``` r
find_non_numeric_chars(performance$logphonecall)
```

    ## [1] "."  "NA" ","

``` r
find_non_numeric_chars(performance$logcallpersec)
```

    ## [1] "."  "NA" ","

``` r
find_non_numeric_chars(performance$logcalllength)
```

    ## [1] "."  "NA" ","

``` r
find_non_numeric_chars(performance$logcall_dayworked)
```

    ## [1] "."  "NA" ","

``` r
find_non_numeric_chars(performance$logdaysworked)
```

    ## [1] "." ","

``` r
# here we  need to take care of the decimal dots, dashes to negative(negative) and scientifics notion with "e" (will be added to the extraction function)
```

Personid needs to be converted to a string, as well as all all the call
and performance measurements need to be converted to doubles.
Homethatweek needs to be converted to a logical and numerical

``` r
# convert to integer to remove leading zeros an than to str
performance$personid <- as.character(as.integer(performance$personid))
# trim year_week and remove all not nuemerical characters
performance[["year_week"]] <- trimws(performance[["year_week"]])
performance[["year_week"]]<- gsub("[^0-9.]", "", performance[["year_week"]])
# convert to float/doubles
performance[["perform1"]] <- as.numeric(performance[["perform1"]])
```

    ## Warning: NAs introduced by coercion

``` r
performance[["phonecall"]] <- as.numeric(performance[["phonecall"]])
```

    ## Warning: NAs introduced by coercion

``` r
# convert to integer
performance[["phonecallraw"]] <- as.integer(performance[["phonecallraw"]])
```

    ## Warning: NAs introduced by coercion

``` r
# convert to boolean
performance[["homethatweek_num"]] <- as.integer(performance[["homethatweek"]])
performance[["homethatweek"]] <- as.logical(performance[["homethatweek_num"]])
# convert to float/doubles
performance[["logphonecall"]] <- as.numeric(performance[["logphonecall"]])
```

    ## Warning: NAs introduced by coercion

``` r
performance[["logcallpersec"]] <- as.numeric(performance[["logcallpersec"]])
```

    ## Warning: NAs introduced by coercion

``` r
performance[["logcalllength"]] <- as.numeric(performance[["logcalllength"]])
```

    ## Warning: NAs introduced by coercion

``` r
performance[["logcall_dayworked"]] <- as.numeric(performance[["logcall_dayworked"]])
```

    ## Warning: NAs introduced by coercion

``` r
performance[["logdaysworked"]] <- as.numeric(performance[["logdaysworked"]])
```

    ## Warning: NAs introduced by coercion

``` r
# convert to Date
performance[["date"]] <- as.Date(performance[["date"]])

# remove duplicates on same personid, year_week
performance <- performance %>%
  distinct(personid, year_week, .keep_all = TRUE)
```

``` r
glimpse(performance)
```

    ## Rows: 9,870
    ## Columns: 13
    ## $ personid          <chr> "4122", "4122", "4122", "4122", "4122", "4122", "412…
    ## $ year_week         <chr> "202201", "202202", "202203", "202204", "202205", "2…
    ## $ perform1          <dbl> -1.1416086, 0.4145924, 1.0139773, 1.4812315, -0.1150…
    ## $ phonecall         <dbl> -1.13219404, 0.31217095, 1.09715199, 1.41114438, -0.…
    ## $ phonecallraw      <int> 223, 499, 649, 709, 403, 760, 268, 73, 301, 629, 529…
    ## $ homethatweek      <lgl> FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FAL…
    ## $ logphonecall      <dbl> 5.407172, 6.212606, 6.475433, 6.563856, 5.998937, 6.…
    ## $ logcallpersec     <dbl> -5.101479, -5.159781, -5.142978, -5.161007, -5.15217…
    ## $ logcalllength     <dbl> 10.508650, 11.372387, 11.618411, 11.724862, 11.15110…
    ## $ logcall_dayworked <dbl> 9.410038, 9.580627, 9.826652, 9.778953, 9.359349, 9.…
    ## $ logdaysworked     <dbl> 1.098612, 1.791759, 1.791759, 1.945910, 1.791759, 1.…
    ## $ date              <date> 2022-01-03, 2022-01-10, 2022-01-17, 2022-01-24, 202…
    ## $ homethatweek_num  <int> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0…

``` r
stargazer(as.data.frame(performance), type = "text", median = TRUE, header = TRUE)
```

    ## 
    ## =============================================================
    ## Statistic           N    Mean   St. Dev.  Min   Median  Max  
    ## -------------------------------------------------------------
    ## perform1          9,827 -0.015   0.988   -3.031 0.049  4.163 
    ## phonecall         9,716 -0.006   0.963   -3.113 0.070  5.828 
    ## phonecallraw      9,589 440.214 142.528    1     445   1,264 
    ## homethatweek      9,870  0.196   0.397     0      0      1   
    ## logphonecall      9,574  6.009   0.481   0.000  6.098  7.142 
    ## logcallpersec     9,576 -5.170   0.160   -5.951 -5.169 -1.099
    ## logcalllength     9,576 11.175   0.521   2.485  11.273 12.116
    ## logcall_dayworked 9,576  9.474   0.457   2.485  9.552  10.359
    ## logdaysworked     9,855  1.685   0.282   0.000  1.792  1.946 
    ## homethatweek_num  9,870  0.196   0.397     0      0      1   
    ## -------------------------------------------------------------

Almost all measurements have missing data.

## Clean wage

``` r
glimpse(wage)
```

    ## Rows: 3,007
    ## Columns: 5
    ## $ basewage   <chr> "1650", "1650", "1650", "1650", "1650", "1650", "1800", "18…
    ## $ grosswage  <chr> "3650.76000976562", "4775.52001953125", "4358", "4801", "40…
    ## $ personid   <chr> "4122", "4122", "4122", "4122", "4122", "4122", "4122", "41…
    ## $ wage_month <chr> "202201", "202202", "202203", "202204", "202205", "202206",…
    ## $ bonustotal <chr> "2000.76000976562", "3125.52001953125", "2708", "3151", "23…

``` r
stargazer(as.data.frame(wage), type = "text", median = TRUE, header = TRUE)
```

    ## 
    ## ========================================
    ## Statistic N Mean St. Dev. Min Median Max
    ## ========================================

``` r
# check character columns with numerical values to extract for non numeric chars to
# be aware for special cases
find_non_numeric_chars(wage$basewage)
```

    ## [1] ","  " "  "  " " ." ",."

``` r
find_non_numeric_rows(wage$basewage)
```

    ##  [1] "1600,00"  " 1600"    " 1700"    " 1600"    " 1700"    "  1600"  
    ##  [7] "  1600"   "1800,00"  "2 000.00" "1700,00"  "  1700"   "  1700"  
    ## [13] " 1700"    " 1700"    "  1750"   "  2100"   "2,300.00" "1,500.00"
    ## [19] " 1650"    "  2300"   " 1700"    " 1450"    " 1600"    "1400,00" 
    ## [25] "  1800"   "1400,00"  "1400,00"  "1,550.00" " 1750"    "1,550.00"
    ## [31] " 1500"    "1 400.00" "  1550"   "  1800"   " 1400"    "  1400"  
    ## [37] "1550,00"  " 1500"    "  1750"   " 1850"    "1 400.00" "  1600"  
    ## [43] "  1600"   "1650,00"  "1,500.00" "  1750"   " 1500"    "1800,00" 
    ## [49] "1 500.00" " 1500"    "1500,00"  "1,500.00"

``` r
# only trimming needed
```

We have no numerical values yet. basewage, can be converted to integer,
gross_wage and bonustotal can be converted to double

``` r
# convert to integer to remove leading zeros an than to str
wage$personid <- as.character(as.integer(wage$personid))
wage[["basewage"]] <- as.integer(wage[["basewage"]])
```

    ## Warning: NAs introduced by coercion

``` r
wage[["grosswage"]] <- as.numeric(wage[["grosswage"]])
```

    ## Warning: NAs introduced by coercion

``` r
wage[["wage_month"]] <- trimws(wage[["wage_month"]])
wage[["wage_month"]]<- gsub("[^0-9.]", "", wage[["wage_month"]])
wage <- rename(wage, year_month=wage_month)
wage[["bonustotal"]] <- as.numeric(wage[["bonustotal"]])
```

    ## Warning: NAs introduced by coercion

``` r
# remove duplicates on same personid, yearmonth
wage <- wage %>%
  distinct(personid, year_month, .keep_all = TRUE)
```

``` r
glimpse(wage)
```

    ## Rows: 3,007
    ## Columns: 5
    ## $ basewage   <int> 1650, 1650, 1650, 1650, 1650, 1650, 1800, 1800, 1800, 1800,…
    ## $ grosswage  <dbl> 3650.76, 4775.52, 4358.00, 4801.00, 4045.12, 5497.76, 3123.…
    ## $ personid   <chr> "4122", "4122", "4122", "4122", "4122", "4122", "4122", "41…
    ## $ year_month <chr> "202201", "202202", "202203", "202204", "202205", "202206",…
    ## $ bonustotal <dbl> 2000.76, 3125.52, 2708.00, 3151.00, 2395.12, 3847.76, 1323.…

``` r
stargazer(as.data.frame(wage), type = "text", median = TRUE, header = TRUE)
```

    ## 
    ## ================================================================
    ## Statistic    N     Mean    St. Dev.   Min    Median      Max    
    ## ----------------------------------------------------------------
    ## basewage   2,987 1,666.271  221.174   650     1,600     2,850   
    ## grosswage  3,001 3,133.573 1,064.704 48.850 2,978.720 14,553.000
    ## bonustotal 2,993 1,502.263  926.276  0.000  1,325.170 12,853.000
    ## ----------------------------------------------------------------

All columns again contain missing data.

# Merging data together

## Merging

``` r
# First we try to merge all volunteer related data together on personid (unique for a person)
volunteer_endperiod <- merge(volunteer, endperiod, by="personid", all=TRUE)
# Second we merge all weekly based data together
attitude_performance <- merge(attitude, performance, by=c("personid","year_week"), all=TRUE)
# As wage only has a yearmonth column, we cannot directly merge it with an other table
# The goal now is to create one big dataframe with all volunteer information, their performance, attitude and how much they have earned. To do so, we need to add the current month the yearweek is in in attitude_performance and merge it with wage. Afterwards with can easily add the volunteer information.
attitude_performance <- attitude_performance %>%
  mutate(
    week_date = ISOweek2date(
      paste0(substr(year_week,1,4), "-W", substr(year_week,5,6), "-1")
    ),
    year_month = format(week_date, "%Y%m")  # gives e.g. "202401", "202412"
  )
# merge on year_month
attitude_performance_wage <- merge(attitude_performance, wage, by=c("personid","year_month"), all.x=TRUE)
# merge on personid
volunteer_endperiod_attitude_performance_wage <- merge(attitude_performance_wage, volunteer_endperiod, by="personid")

# lets try to get a dataframe with ALL data and no NAs at all
distinct_all <- na.omit(volunteer_endperiod_attitude_performance_wage)
```

## Analyzing merged dataframes

``` r
stargazer(as.data.frame(volunteer_endperiod), type = "text", median = TRUE, header = TRUE)
```

    ## 
    ## ===========================================================
    ## Statistic           N   Mean   St. Dev.  Min  Median  Max  
    ## -----------------------------------------------------------
    ## age                135 23.452   4.315     1     23     35  
    ## tenure             135 23.385   24.608    2     13    150  
    ## children           135  0.148   0.357     0     0      1   
    ## bedroom            135  0.978   0.148     0     1      1   
    ## commute            122 104.721  68.544    1     85    300  
    ## high_educ          135  0.415   0.495     0     0      1   
    ## gender_num         135  0.496   0.502     0     0      1   
    ## married_num        135  0.200   0.401     0     0      1   
    ## high_educ_num      135  0.415   0.495     0     0      1   
    ## children_num       135  0.148   0.357     0     0      1   
    ## promote_switch     135  0.163   0.371     0     0      1   
    ## quitjob            135  0.296   0.458     0     0      1   
    ## costofcommute      130  7.555   7.283   0.000 6.000  55.000
    ## promote_switch_num 135  0.163   0.371     0     0      1   
    ## quitjob_num        135  0.296   0.458     0     0      1   
    ## -----------------------------------------------------------

``` r
# looks good
stargazer(as.data.frame(attitude_performance), type = "text", median = TRUE, header = TRUE)
```

    ## 
    ## =============================================================
    ## Statistic           N    Mean   St. Dev.  Min   Median  Max  
    ## -------------------------------------------------------------
    ## exhaustion        2,379  8.612   7.794   0.000  7.000  36.000
    ## negative          2,379 16.651   6.848   8.000  16.000 40.000
    ## positive          2,379 24.215   6.668   8.000  24.000 40.000
    ## perform1          9,827 -0.015   0.988   -3.031 0.049  4.163 
    ## phonecall         9,716 -0.006   0.963   -3.113 0.070  5.828 
    ## phonecallraw      9,589 440.214 142.528    1     445   1,264 
    ## homethatweek      9,870  0.196   0.397     0      0      1   
    ## logphonecall      9,574  6.009   0.481   0.000  6.098  7.142 
    ## logcallpersec     9,576 -5.170   0.160   -5.951 -5.169 -1.099
    ## logcalllength     9,576 11.175   0.521   2.485  11.273 12.116
    ## logcall_dayworked 9,576  9.474   0.457   2.485  9.552  10.359
    ## logdaysworked     9,855  1.685   0.282   0.000  1.792  1.946 
    ## homethatweek_num  9,870  0.196   0.397     0      0      1   
    ## -------------------------------------------------------------

``` r
# when using this dataframe we have to be cautions as attitude has way less columns than and because n(attitude_performance) > n(performance), as a result of the outer-join, both table have combinations of personid, year_weak the otherone does not
stargazer(as.data.frame(attitude_performance_wage), type = "text", median = TRUE, header = TRUE) 
```

    ## 
    ## ========================================================================
    ## Statistic           N      Mean    St. Dev.   Min    Median      Max    
    ## ------------------------------------------------------------------------
    ## exhaustion        2,379    8.612     7.794   0.000    7.000     36.000  
    ## negative          2,379   16.651     6.848   8.000   16.000     40.000  
    ## positive          2,379   24.215     6.668   8.000   24.000     40.000  
    ## perform1          9,827   -0.015     0.988   -3.031   0.049     4.163   
    ## phonecall         9,716   -0.006     0.963   -3.113   0.070     5.828   
    ## phonecallraw      9,589   440.214   142.528    1       445      1,264   
    ## homethatweek      9,870    0.196     0.397     0        0         1     
    ## logphonecall      9,574    6.009     0.481   0.000    6.098     7.142   
    ## logcallpersec     9,576   -5.170     0.160   -5.951  -5.169     -1.099  
    ## logcalllength     9,576   11.175     0.521   2.485   11.273     12.116  
    ## logcall_dayworked 9,576    9.474     0.457   2.485    9.552     10.359  
    ## logdaysworked     9,855    1.685     0.282   0.000    1.792     1.946   
    ## homethatweek_num  9,870    0.196     0.397     0        0         1     
    ## basewage          9,953  1,602.507  176.442   650     1,600     2,450   
    ## grosswage         10,001 3,080.657 1,000.450 48.850 2,864.000 14,553.000
    ## bonustotal        9,979  1,493.570  912.302  0.000  1,284.430 12,853.000
    ## ------------------------------------------------------------------------

``` r
stargazer(as.data.frame(volunteer_endperiod_attitude_performance_wage), type = "text", median = TRUE, header = TRUE)
```

    ## 
    ## =========================================================================
    ## Statistic            N      Mean    St. Dev.   Min    Median      Max    
    ## -------------------------------------------------------------------------
    ## exhaustion         2,379    8.612     7.794   0.000    7.000     36.000  
    ## negative           2,379   16.651     6.848   8.000   16.000     40.000  
    ## positive           2,379   24.215     6.668   8.000   24.000     40.000  
    ## perform1           9,827   -0.015     0.988   -3.031   0.049     4.163   
    ## phonecall          9,716   -0.006     0.963   -3.113   0.070     5.828   
    ## phonecallraw       9,589   440.214   142.528    1       445      1,264   
    ## homethatweek       9,870    0.196     0.397     0        0         1     
    ## logphonecall       9,574    6.009     0.481   0.000    6.098     7.142   
    ## logcallpersec      9,576   -5.170     0.160   -5.951  -5.169     -1.099  
    ## logcalllength      9,576   11.175     0.521   2.485   11.273     12.116  
    ## logcall_dayworked  9,576    9.474     0.457   2.485    9.552     10.359  
    ## logdaysworked      9,855    1.685     0.282   0.000    1.792     1.946   
    ## homethatweek_num   9,870    0.196     0.397     0        0         1     
    ## basewage           9,953  1,602.507  176.442   650     1,600     2,450   
    ## grosswage          10,001 3,080.657 1,000.450 48.850 2,864.000 14,553.000
    ## bonustotal         9,979  1,493.570  912.302  0.000  1,284.430 12,853.000
    ## age                10,079  23.485     4.191     1       23         35    
    ## tenure             10,079  23.893    24.773     2       18        150    
    ## children           10,079   0.141     0.348     0        0         1     
    ## bedroom            10,079   0.974     0.160     0        1         1     
    ## commute            9,118   103.519   67.620     1       80        300    
    ## high_educ          10,079   0.400     0.490     0        0         1     
    ## gender_num         10,079   0.498     0.500     0        0         1     
    ## married_num        10,079   0.181     0.385     0        0         1     
    ## high_educ_num      10,079   0.400     0.490     0        0         1     
    ## children_num       10,079   0.141     0.348     0        0         1     
    ## promote_switch     10,079   0.182     0.386     0        0         1     
    ## quitjob            10,079   0.238     0.426     0        0         1     
    ## costofcommute      9,734    7.397     7.304   0.000    6.000     55.000  
    ## promote_switch_num 10,079   0.182     0.386     0        0         1     
    ## quitjob_num        10,079   0.238     0.426     0        0         1     
    ## -------------------------------------------------------------------------

``` r
stargazer(as.data.frame(distinct_all), type = "text", median = TRUE, header = TRUE)
```

    ## 
    ## ===========================================================================
    ## Statistic            N     Mean    St. Dev.     Min     Median      Max    
    ## ---------------------------------------------------------------------------
    ## exhaustion         1,790   8.745     7.903     0.000     8.000     36.000  
    ## negative           1,790  16.583     7.025     8.000    16.000     40.000  
    ## positive           1,790  23.918     6.728     8.000    24.000     40.000  
    ## perform1           1,790   0.079     0.857    -2.906     0.101     2.772   
    ## phonecall          1,790   0.038     0.806    -3.098     0.046     2.266   
    ## phonecallraw       1,790  440.092   125.999      2        442       835    
    ## homethatweek       1,790   0.518     0.500       0         1         1     
    ## logphonecall       1,790   6.035     0.365     0.693     6.091     6.727   
    ## logcallpersec      1,790  -5.158     0.130    -5.541    -5.157     -4.075  
    ## logcalllength      1,790  11.193     0.372     5.561    11.243     11.952  
    ## logcall_dayworked  1,790   9.467     0.353     3.951     9.507     10.104  
    ## logdaysworked      1,790   1.726     0.182     0.693     1.792     1.946   
    ## homethatweek_num   1,790   0.518     0.500       0         1         1     
    ## basewage           1,790 1,688.911  158.551    1,300     1,700     2,150   
    ## grosswage          1,790 3,334.994 1,049.045 1,497.400 3,095.000 14,553.000
    ## bonustotal         1,790 1,654.377  970.529   40.000   1,415.000 12,853.000
    ## age                1,790  23.689     3.493      18        23         34    
    ## tenure             1,790  24.375    26.649       2        19        150    
    ## children           1,790   0.093     0.290       0         0         1     
    ## bedroom            1,790   0.941     0.235       0         1         1     
    ## commute            1,790  105.489   67.516      20        80        300    
    ## high_educ          1,790   0.339     0.473       0         0         1     
    ## gender_num         1,790   0.477     0.500       0         0         1     
    ## married_num        1,790   0.128     0.334       0         0         1     
    ## high_educ_num      1,790   0.339     0.473       0         0         1     
    ## children_num       1,790   0.093     0.290       0         0         1     
    ## promote_switch     1,790   0.186     0.389       0         0         1     
    ## quitjob            1,790   0.000     0.000       0         0         0     
    ## costofcommute      1,790   6.884     6.079     0.000     6.000     30.000  
    ## promote_switch_num 1,790   0.186     0.389       0         0         1     
    ## quitjob_num        1,790   0.000     0.000       0         0         0     
    ## ---------------------------------------------------------------------------

When joing everything together we can see that we do not have all data
for each person and its work weeks, what was to be expected. When
removing all NAs we can see that the numbers slightly change and for
examples all quitters are gone. Therefore we have to use the
volunteer_endperiod_attitude_performance_wage with caution and eliminate
NAs for each calculation individually. Though this dataframe is handy to
use for the further work.

Note: all now resulted dataframes still contain NA values

# General Analysis

Get first basic insights of the tables before eliminating null values
and merging data together. For a better overview we will look at each
table alone, not on the composition table.

## Statistics

### Performance

``` r
stargazer(as.data.frame(performance), type = "text", median = TRUE, header = TRUE)
```

    ## 
    ## =============================================================
    ## Statistic           N    Mean   St. Dev.  Min   Median  Max  
    ## -------------------------------------------------------------
    ## perform1          9,827 -0.015   0.988   -3.031 0.049  4.163 
    ## phonecall         9,716 -0.006   0.963   -3.113 0.070  5.828 
    ## phonecallraw      9,589 440.214 142.528    1     445   1,264 
    ## homethatweek      9,870  0.196   0.397     0      0      1   
    ## logphonecall      9,574  6.009   0.481   0.000  6.098  7.142 
    ## logcallpersec     9,576 -5.170   0.160   -5.951 -5.169 -1.099
    ## logcalllength     9,576 11.175   0.521   2.485  11.273 12.116
    ## logcall_dayworked 9,576  9.474   0.457   2.485  9.552  10.359
    ## logdaysworked     9,855  1.685   0.282   0.000  1.792  1.946 
    ## homethatweek_num  9,870  0.196   0.397     0      0      1   
    ## -------------------------------------------------------------

A lot of missing values, 296 at max, which will fall out from the
dataset when working with it.  
Working from home rate is at 19,64% on average, probably as a result of
a Work-from-home-policy.

### Volunteer

``` r
stargazer(as.data.frame(volunteer), type = "text", median = TRUE, header = TRUE)
```

    ## 
    ## =================================================
    ## Statistic      N   Mean   St. Dev. Min Median Max
    ## -------------------------------------------------
    ## age           135 23.452   4.315    1    23   35 
    ## tenure        135 23.385   24.608   2    13   150
    ## children      135  0.148   0.357    0    0     1 
    ## bedroom       135  0.978   0.148    0    1     1 
    ## commute       122 104.721  68.544   1    85   300
    ## high_educ     135  0.415   0.495    0    0     1 
    ## gender_num    135  0.496   0.502    0    0     1 
    ## married_num   135  0.200   0.401    0    0     1 
    ## high_educ_num 135  0.415   0.495    0    0     1 
    ## children_num  135  0.148   0.357    0    0     1 
    ## -------------------------------------------------

No missing values. The average worker is 23,5 years old with a mean
commute time of 100 (what?), while there are greater difference, and is
for almost 23 (days?) within the company. 15% have children, 20% are
married, more than 40% have a higher education. Both, men and women work
at the company.

### Endperiod

``` r
stargazer(as.data.frame(endperiod), type = "text", median = TRUE, header = TRUE)
```

    ## 
    ## =========================================================
    ## Statistic           N  Mean  St. Dev.  Min  Median  Max  
    ## ---------------------------------------------------------
    ## promote_switch     135 0.163  0.371     0     0      1   
    ## quitjob            135 0.296  0.458     0     0      1   
    ## costofcommute      130 7.555  7.283   0.000 6.000  55.000
    ## promote_switch_num 135 0.163  0.371     0     0      1   
    ## quitjob_num        135 0.296  0.458     0     0      1   
    ## ---------------------------------------------------------

On average, a employee had to commute for 10 minutes a day, but this
number may heavily differ to some employees, 16% got a promotion, while
almost 30% quit their job at some point.

### Wage

``` r
stargazer(as.data.frame(wage), type = "text", median = TRUE, header = TRUE)
```

    ## 
    ## ================================================================
    ## Statistic    N     Mean    St. Dev.   Min    Median      Max    
    ## ----------------------------------------------------------------
    ## basewage   2,987 1,666.271  221.174   650     1,600     2,850   
    ## grosswage  3,001 3,133.573 1,064.704 48.850 2,978.720 14,553.000
    ## bonustotal 2,993 1,502.263  926.276  0.000  1,325.170 12,853.000
    ## ----------------------------------------------------------------

On average an employee had a base wage of around 1650€ which can
slightly differ. A heavily uneven distribution can be seen for the
bonuses for employees, while some refer no bonus at all others receive
almost 13.000€ as bonus, on average it is 900€ per employee.

### Attitude

``` r
stargazer(as.data.frame(attitude), type = "text", median = TRUE, header = TRUE)
```

    ## 
    ## ====================================================
    ## Statistic    N    Mean  St. Dev.  Min  Median  Max  
    ## ----------------------------------------------------
    ## exhaustion 2,379 8.612   7.794   0.000 7.000  36.000
    ## negative   2,379 16.651  6.848   8.000 16.000 40.000
    ## positive   2,379 24.215  6.668   8.000 24.000 40.000
    ## ----------------------------------------------------

### All together

``` r
stargazer(as.data.frame(volunteer_endperiod_attitude_performance_wage), type = "text", median = TRUE, header = TRUE)
```

    ## 
    ## =========================================================================
    ## Statistic            N      Mean    St. Dev.   Min    Median      Max    
    ## -------------------------------------------------------------------------
    ## exhaustion         2,379    8.612     7.794   0.000    7.000     36.000  
    ## negative           2,379   16.651     6.848   8.000   16.000     40.000  
    ## positive           2,379   24.215     6.668   8.000   24.000     40.000  
    ## perform1           9,827   -0.015     0.988   -3.031   0.049     4.163   
    ## phonecall          9,716   -0.006     0.963   -3.113   0.070     5.828   
    ## phonecallraw       9,589   440.214   142.528    1       445      1,264   
    ## homethatweek       9,870    0.196     0.397     0        0         1     
    ## logphonecall       9,574    6.009     0.481   0.000    6.098     7.142   
    ## logcallpersec      9,576   -5.170     0.160   -5.951  -5.169     -1.099  
    ## logcalllength      9,576   11.175     0.521   2.485   11.273     12.116  
    ## logcall_dayworked  9,576    9.474     0.457   2.485    9.552     10.359  
    ## logdaysworked      9,855    1.685     0.282   0.000    1.792     1.946   
    ## homethatweek_num   9,870    0.196     0.397     0        0         1     
    ## basewage           9,953  1,602.507  176.442   650     1,600     2,450   
    ## grosswage          10,001 3,080.657 1,000.450 48.850 2,864.000 14,553.000
    ## bonustotal         9,979  1,493.570  912.302  0.000  1,284.430 12,853.000
    ## age                10,079  23.485     4.191     1       23         35    
    ## tenure             10,079  23.893    24.773     2       18        150    
    ## children           10,079   0.141     0.348     0        0         1     
    ## bedroom            10,079   0.974     0.160     0        1         1     
    ## commute            9,118   103.519   67.620     1       80        300    
    ## high_educ          10,079   0.400     0.490     0        0         1     
    ## gender_num         10,079   0.498     0.500     0        0         1     
    ## married_num        10,079   0.181     0.385     0        0         1     
    ## high_educ_num      10,079   0.400     0.490     0        0         1     
    ## children_num       10,079   0.141     0.348     0        0         1     
    ## promote_switch     10,079   0.182     0.386     0        0         1     
    ## quitjob            10,079   0.238     0.426     0        0         1     
    ## costofcommute      9,734    7.397     7.304   0.000    6.000     55.000  
    ## promote_switch_num 10,079   0.182     0.386     0        0         1     
    ## quitjob_num        10,079   0.238     0.426     0        0         1     
    ## -------------------------------------------------------------------------

Exhaustion scores are mostly low and clustered around 7–9 but some
people reach much higher (up to 36), negative scores center near 16 with
moderate variation across the group, and positive scores center near 24
with a similar moderate spread, meaning most participants stay within
roughly one standard deviation (about ±7 points) of these averages but a
few reach the extreme minimum or maximum values

## Linear Regression

``` r
nums <- dplyr::select(volunteer_endperiod_attitude_performance_wage, where(is.numeric))
cm <- cor(nums, use = "pairwise.complete.obs")
```

    ## Warning in cor(nums, use = "pairwise.complete.obs"): the standard deviation is
    ## zero

``` r
ggcorrplot(cm, lab = FALSE)
```

![](notebook_files/figure-gfm/unnamed-chunk-26-1.png)<!-- -->

Here we can see that the performance metric “perform1” has a correlation
to phonecall, phonecallraw, logphonecall, logcalllength, which gives us
a hint on how this metrics was calculated. Furthermore we can see, a
negative correlation between “negative” and “positive” from the attitude
dataset, which makes sense as these are opposites. Because of the
timeseries data of the performance data set, the light correlation can
be explained as the attitude may change from day to day, or not (what we
can not infer from that). There is also a slight correlation between
exhaustion and negative attitude, which seems logical. What we cannot
see are significant correlations between quit_job and any variable and
other than the cluster itself between perfrom1 and any other variable.
Quit_job has moderate negative correlations to bonuses/grosswage and
promotions.

We cannot see any clear indicators on how to fight churn rate and/or
what motivates people to perform better. Therefore we will have a look
on different groups of people.

# Detailed exploration analysis

## Quitter vs no-quitters

Lets compare quitter vs non-quitter to better understand both groups:

``` r
df <- volunteer_endperiod_attitude_performance_wage
quitter_df <- df %>% filter(quitjob == TRUE)
non_quitter_df <- df %>% filter(quitjob == FALSE)

# general impression
stargazer(as.data.frame(quitter_df), type = "text", median = TRUE, header = TRUE)
```

    ## 
    ## =======================================================================
    ## Statistic            N     Mean    St. Dev.  Min    Median      Max    
    ## -----------------------------------------------------------------------
    ## perform1           2,383  -0.239    1.020   -3.031  -0.209     3.367   
    ## phonecall          2,316  -0.182    1.036   -3.113  -0.145     5.828   
    ## phonecallraw       2,272  419.832  150.751    1       419      1,264   
    ## homethatweek       2,401   0.088    0.283     0        0         1     
    ## logphonecall       2,267   5.939    0.563   0.000    6.038     7.142   
    ## logcallpersec      2,264  -5.141    0.192   -5.721  -5.148     -1.099  
    ## logcalllength      2,266  11.075    0.608   2.485   11.198     12.002  
    ## logcall_dayworked  2,266   9.414    0.517   2.485    9.506     10.210  
    ## logdaysworked      2,398   1.636    0.327   0.000    1.609     1.946   
    ## homethatweek_num   2,401   0.088    0.283     0        0         1     
    ## basewage           2,365 1,561.818 185.091   650     1,550     2,450   
    ## grosswage          2,379 2,655.609 815.264  48.850 2,490.450 10,761.100
    ## bonustotal         2,370 1,125.712 726.929  0.000   971.690  9,330.070 
    ## age                2,401  23.676    3.088     19      23         32    
    ## tenure             2,401  23.439    28.649    2       10        150    
    ## children           2,401   0.221    0.415     0        0         1     
    ## bedroom            2,401   1.000    0.000     1        1         1     
    ## commute            2,118  104.710   76.718    1       120       300    
    ## high_educ          2,401   0.461    0.499     0        0         1     
    ## gender_num         2,401   0.450    0.498     0        0         1     
    ## married_num        2,401   0.239    0.427     0        0         1     
    ## high_educ_num      2,401   0.461    0.499     0        0         1     
    ## children_num       2,401   0.221    0.415     0        0         1     
    ## promote_switch     2,401   0.000    0.000     0        0         0     
    ## quitjob            2,401   1.000    0.000     1        1         1     
    ## costofcommute      2,289   9.209    10.494  0.000    8.000     55.000  
    ## promote_switch_num 2,401   0.000    0.000     0        0         0     
    ## quitjob_num        2,401   1.000    0.000     1        1         1     
    ## -----------------------------------------------------------------------

``` r
stargazer(as.data.frame(non_quitter_df), type = "text", median = TRUE, header = TRUE)
```

    ## 
    ## =========================================================================
    ## Statistic            N     Mean    St. Dev.    Min    Median      Max    
    ## -------------------------------------------------------------------------
    ## exhaustion         2,379   8.612     7.794    0.000    7.000     36.000  
    ## negative           2,379  16.651     6.848    8.000   16.000     40.000  
    ## positive           2,379  24.215     6.668    8.000   24.000     40.000  
    ## perform1           7,444   0.057     0.966   -3.031    0.119     4.163   
    ## phonecall          7,400   0.049     0.932   -3.113    0.127     3.758   
    ## phonecallraw       7,317  446.543   139.282     1       452      1,141   
    ## homethatweek       7,469   0.231     0.422      0        0         1     
    ## logphonecall       7,307   6.031     0.451    0.000    6.114     7.040   
    ## logcallpersec      7,312  -5.179     0.147   -5.951   -5.175     -3.296  
    ## logcalllength      7,310  11.205     0.487    3.989   11.296     12.116  
    ## logcall_dayworked  7,310   9.492     0.435    2.639    9.567     10.359  
    ## logdaysworked      7,457   1.701     0.263    0.000    1.792     1.946   
    ## homethatweek_num   7,469   0.231     0.422      0        0         1     
    ## basewage           7,588 1,615.188  171.712    650     1,600     2,300   
    ## grosswage          7,622 3,213.324 1,015.850 956.440 2,997.000 14,553.000
    ## bonustotal         7,609 1,608.148  933.670   0.000  1,381.860 12,853.000
    ## age                7,678  23.426     4.479      1       23         35    
    ## tenure             7,678  24.035    23.430      2       19        150    
    ## children           7,678   0.116     0.320      0        0         1     
    ## bedroom            7,678   0.966     0.182      0        1         1     
    ## commute            7,000  103.158   64.618      2       80        300    
    ## high_educ          7,678   0.380     0.486      0        0         1     
    ## gender_num         7,678   0.513     0.500      0        1         1     
    ## married_num        7,678   0.163     0.369      0        0         1     
    ## high_educ_num      7,678   0.380     0.486      0        0         1     
    ## children_num       7,678   0.116     0.320      0        0         1     
    ## promote_switch     7,678   0.239     0.427      0        0         1     
    ## quitjob            7,678   0.000     0.000      0        0         0     
    ## costofcommute      7,445   6.840     5.881    0.000    6.000     30.000  
    ## promote_switch_num 7,678   0.239     0.427      0        0         1     
    ## quitjob_num        7,678   0.000     0.000      0        0         0     
    ## -------------------------------------------------------------------------

``` r
stargazer(as.data.frame(df), type = "text", median = TRUE, header = TRUE)
```

    ## 
    ## =========================================================================
    ## Statistic            N      Mean    St. Dev.   Min    Median      Max    
    ## -------------------------------------------------------------------------
    ## exhaustion         2,379    8.612     7.794   0.000    7.000     36.000  
    ## negative           2,379   16.651     6.848   8.000   16.000     40.000  
    ## positive           2,379   24.215     6.668   8.000   24.000     40.000  
    ## perform1           9,827   -0.015     0.988   -3.031   0.049     4.163   
    ## phonecall          9,716   -0.006     0.963   -3.113   0.070     5.828   
    ## phonecallraw       9,589   440.214   142.528    1       445      1,264   
    ## homethatweek       9,870    0.196     0.397     0        0         1     
    ## logphonecall       9,574    6.009     0.481   0.000    6.098     7.142   
    ## logcallpersec      9,576   -5.170     0.160   -5.951  -5.169     -1.099  
    ## logcalllength      9,576   11.175     0.521   2.485   11.273     12.116  
    ## logcall_dayworked  9,576    9.474     0.457   2.485    9.552     10.359  
    ## logdaysworked      9,855    1.685     0.282   0.000    1.792     1.946   
    ## homethatweek_num   9,870    0.196     0.397     0        0         1     
    ## basewage           9,953  1,602.507  176.442   650     1,600     2,450   
    ## grosswage          10,001 3,080.657 1,000.450 48.850 2,864.000 14,553.000
    ## bonustotal         9,979  1,493.570  912.302  0.000  1,284.430 12,853.000
    ## age                10,079  23.485     4.191     1       23         35    
    ## tenure             10,079  23.893    24.773     2       18        150    
    ## children           10,079   0.141     0.348     0        0         1     
    ## bedroom            10,079   0.974     0.160     0        1         1     
    ## commute            9,118   103.519   67.620     1       80        300    
    ## high_educ          10,079   0.400     0.490     0        0         1     
    ## gender_num         10,079   0.498     0.500     0        0         1     
    ## married_num        10,079   0.181     0.385     0        0         1     
    ## high_educ_num      10,079   0.400     0.490     0        0         1     
    ## children_num       10,079   0.141     0.348     0        0         1     
    ## promote_switch     10,079   0.182     0.386     0        0         1     
    ## quitjob            10,079   0.238     0.426     0        0         1     
    ## costofcommute      9,734    7.397     7.304   0.000    6.000     55.000  
    ## promote_switch_num 10,079   0.182     0.386     0        0         1     
    ## quitjob_num        10,079   0.238     0.426     0        0         1     
    ## -------------------------------------------------------------------------

``` r
# plot per numeric variable
ggplot(df, aes(x=quitjob, y=perform1)) + geom_boxplot()
```

    ## Warning: Removed 252 rows containing non-finite outside the scale range
    ## (`stat_boxplot()`).

![](notebook_files/figure-gfm/unnamed-chunk-27-1.png)<!-- -->

``` r
ggplot(df, aes(x=quitjob, y=tenure)) + geom_boxplot()
```

![](notebook_files/figure-gfm/unnamed-chunk-27-2.png)<!-- -->

``` r
ggplot(df, aes(x=quitjob, y=age)) + geom_boxplot()
```

![](notebook_files/figure-gfm/unnamed-chunk-27-3.png)<!-- -->

``` r
ggplot(df, aes(x=quitjob, y=commute)) + geom_boxplot()
```

    ## Warning: Removed 961 rows containing non-finite outside the scale range
    ## (`stat_boxplot()`).

![](notebook_files/figure-gfm/unnamed-chunk-27-4.png)<!-- -->

``` r
ggplot(df, aes(x=quitjob, y=costofcommute)) + geom_boxplot()
```

    ## Warning: Removed 345 rows containing non-finite outside the scale range
    ## (`stat_boxplot()`).

![](notebook_files/figure-gfm/unnamed-chunk-27-5.png)<!-- -->

``` r
ggplot(df, aes(x=quitjob, y=basewage)) + geom_boxplot()
```

    ## Warning: Removed 126 rows containing non-finite outside the scale range
    ## (`stat_boxplot()`).

![](notebook_files/figure-gfm/unnamed-chunk-27-6.png)<!-- -->

``` r
ggplot(df, aes(x=quitjob, y=bonustotal)) + geom_boxplot()
```

    ## Warning: Removed 100 rows containing non-finite outside the scale range
    ## (`stat_boxplot()`).

![](notebook_files/figure-gfm/unnamed-chunk-27-7.png)<!-- -->

``` r
ggplot(df, aes(x=quitjob, y=grosswage)) + geom_boxplot()
```

    ## Warning: Removed 78 rows containing non-finite outside the scale range
    ## (`stat_boxplot()`).

![](notebook_files/figure-gfm/unnamed-chunk-27-8.png)<!-- -->

``` r
# Look at correlations
# subselect variable available in both groups (e.g. attitude data not available for quitters)
non_quitter_df <- non_quitter_df %>% select(costofcommute,commute,tenure,age,grosswage,basewage,bonustotal,logdaysworked,logcall_dayworked,logcalllength,logcallpersec,logphonecall,phonecallraw,phonecall,perform1, children_num, high_educ_num, married_num, gender_num, homethatweek_num)

quitter_df <- quitter_df %>% select(costofcommute,commute,tenure,age,grosswage,basewage,bonustotal,logdaysworked,logcall_dayworked,logcalllength,logcallpersec,logphonecall,phonecallraw,phonecall,perform1, children_num, high_educ_num, married_num, gender_num, homethatweek_num)

nums <- dplyr::select(quitter_df, where(is.numeric))
cm <- cor(nums, use = "pairwise.complete.obs")
ggcorrplot(cm, lab = FALSE)
```

![](notebook_files/figure-gfm/unnamed-chunk-27-9.png)<!-- -->

``` r
nums <- dplyr::select(non_quitter_df, where(is.numeric))
cm <- cor(nums, use = "pairwise.complete.obs")
ggcorrplot(cm, lab = FALSE)
```

![](notebook_files/figure-gfm/unnamed-chunk-27-10.png)<!-- -->

Generally speaking quitters perform less, work for the company shorter
than average, have an average age, have slightly higher costs of commute
while commuting about the same in minutes per week, earn less and work
not from home in comparison to workers who stay. When begin older,
quitters have a higher tendency to have children, being married and have
higher education. People who were older and left at some point generally
performed better than younger quitters. People who stay, tend to have
lower cost of commute and lower commute times (maybe because they commit
to working for the company and move closer to it).

We can already get a clue from this comparison of the differences
between quitters and non quitters about the group constellation and
personas.

## Groups

Lets have a look on all possible combinations of categorical values and
which are represented within the volunteer dataset with a threshold of
at least 100 people-work-performances per group. We choose this sample
size to increase our confidence. Afterwards we will do the same general
analysis we did for the whole population for each group.

### Available groups

``` r
# make it easier to process
df <- volunteer_endperiod_attitude_performance_wage
# define categorical variables
cat_cols <- c("gender","married","high_educ","bedroom","children","promote_switch","quitjob", "homethatweek")

# factors
df <- df %>% mutate(across(all_of(cat_cols), ~ factor(.x)))

# build grid of categories
lvls <- lapply(df[cat_cols], levels)
grid <- tidyr::expand_grid(!!!lvls)

# create summary for each category/group of people working at the company
summary_df <- df %>%
  group_by(across(all_of(cat_cols))) %>%
  summarise(
    # add number per category to filter later
    n = n(),
    across(where(is.numeric),
           list(mean = ~mean(.x, na.rm = TRUE),
                sd   = ~sd(.x,   na.rm = TRUE),
                se = ~sd(.x, na.rm = TRUE) / sqrt(sum(!is.na(.x))),
                med  = ~median(.x, na.rm = TRUE),
                min  = ~min(.x, na.rm = TRUE),
                max  = ~max(.x, na.rm = TRUE)),
           .names = "{.col}_{.fn}"
           ),
    .groups = "drop"
) %>%
  mutate(group_index = row_number()) %>% # create an index for each group for bettler handling
  arrange(desc(n))
```

    ## Warning: There were 396 warnings in `summarise()`.
    ## The first warning was:
    ## ℹ In argument: `across(...)`.
    ## ℹ In group 7: `gender = female`, `married = FALSE`, `high_educ = FALSE`,
    ##   `bedroom = 1`, `children = FALSE`, `promote_switch = FALSE`, `quitjob =
    ##   TRUE`, `homethatweek = FALSE`.
    ## Caused by warning in `min()`:
    ## ! no non-missing arguments to min; returning Inf
    ## ℹ Run `dplyr::last_dplyr_warnings()` to see the 395 remaining warnings.

``` r
# add group_index to the big d
group_characteristics <- summary_df %>% select(group_index, gender, married, bedroom, children, high_educ, promote_switch, quitjob, homethatweek)
volunteer_endperiod_attitude_performance_wage <- merge(volunteer_endperiod_attitude_performance_wage, group_characteristics, by=c("gender", "married", "bedroom", "children", "high_educ", "promote_switch", "quitjob", "homethatweek"))
# filter by number of rows per group
summary_df <- summary_df %>% filter(n>=100) 
```

As a result we receive 23 distinct groups out of 8x8=64 possible options
where each groups represents a work week of an employee:

### Top 6 groups by number

``` r
top <- summary_df %>% slice(1:6)

total_n <- summary_df %>% summarise(total_n = sum(n))
top_n <- top %>% summarise(top_n = sum(n))

top_n / total_n # about one quarter of the groups contain over 50% of the total observations
```

    ##       top_n
    ## 1 0.5245345

``` r
top %>% select(gender, married, bedroom, children, high_educ, promote_switch, quitjob, homethatweek)
```

    ## # A tibble: 6 × 8
    ##   gender married bedroom children high_educ promote_switch quitjob homethatweek
    ##   <fct>  <fct>   <fct>   <fct>    <fct>     <fct>          <fct>   <fct>       
    ## 1 female FALSE   1       FALSE    FALSE     FALSE          FALSE   FALSE       
    ## 2 male   FALSE   1       FALSE    FALSE     FALSE          FALSE   FALSE       
    ## 3 male   FALSE   1       FALSE    TRUE      FALSE          FALSE   FALSE       
    ## 4 male   FALSE   1       FALSE    FALSE     FALSE          TRUE    FALSE       
    ## 5 male   FALSE   1       FALSE    FALSE     TRUE           FALSE   FALSE       
    ## 6 female FALSE   1       FALSE    TRUE      FALSE          FALSE   FALSE

``` r
# 
```

``` r
# First we look at the quitters and try to see which are more likely to quit
ggplot(summary_df, aes(x = factor(group_index), y = quitjob_num_mean)) +
  geom_boxplot() +
  labs(x = "Group index", y = "percentage to quit (0-1)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
```

![](notebook_files/figure-gfm/unnamed-chunk-30-1.png)<!-- -->

Here we can clearly identify groups 7,16,40,48 and 60 where all people
quit. Within the rest of the groups nobody quit.

### Analyze quitter groups

``` r
df <- volunteer_endperiod_attitude_performance_wage
```

#### Group 7

357 observations of 6 volunteer

**Persona observations:**  
female  
not married  
number of bedrooms: 1  
no children  
no high education  
no promotion  
worked for 7.6 months at the company when left (\< avg)  
20.5 years old (\< avg)  
  
**Other average weekly observations:**  
not working from home  
performed -0.308 (\< avg)  
basewage per month: 1453 (\< avg)  
bonus per month: 831 (\<\< avg)  
grosswage per month: 2265 (\< avg)  
cost of commute per month: 4 (\< avg)  
commute minutes per week: 103 (\<avg)  
logdaysworked: 1.6 (~= avg)

This groups shows a very similar correlation matrix in comparison to the
overall sample, no specialities.

*Interpretation*  
Looks like a group of female juniors (wage, performance, tenure, age),
living not close by, probably the first or second job after high
school(?) or even a job for a gap year. Probably left because of
pursuing with school or another job for a pay increase.

*Recommendation*

This group is amoung the youngest and has a worse performance than the
average. Offering working from home/remote working capabilities can be a
recommended, as these group could for example continue working during
university or traveling. When performance increases a raise would be
appropiate but now as first action.

``` r
# select group data
group7 <- df %>% filter(group_index==7)
stargazer(as.data.frame(group7), type = "text", median = TRUE, header = TRUE)
```

    ## 
    ## =====================================================================
    ## Statistic           N    Mean    St. Dev.   Min    Median      Max   
    ## ---------------------------------------------------------------------
    ## bedroom            357   1.000    0.000      1        1         1    
    ## children           357   0.000    0.000      0        0         0    
    ## high_educ          357   0.000    0.000      0        0         0    
    ## promote_switch     357   0.000    0.000      0        0         0    
    ## quitjob            357   1.000    0.000      1        1         1    
    ## homethatweek       357   0.000    0.000      0        0         0    
    ## perform1           345  -0.308    0.854   -3.031   -0.204     2.230  
    ## phonecall          320  -0.220    0.983   -3.054   -0.157     5.828  
    ## phonecallraw       316  410.668  138.199    22       419      1,264  
    ## logphonecall       315   5.941    0.443    3.091    6.038     7.142  
    ## logcallpersec      307  -5.217    0.114   -5.560   -5.198    -4.924  
    ## logcalllength      308  11.142    0.444    8.473   11.222    11.900  
    ## logcall_dayworked  308   9.493    0.369    7.087    9.566    10.049  
    ## logdaysworked      357   1.603    0.325    0.000    1.609     1.946  
    ## homethatweek_num   357   0.000    0.000      0        0         0    
    ## basewage           356 1,453.792 152.790    650     1,500     1,750  
    ## grosswage          357 2,265.992 490.587  698.070 2,208.000 4,487.660
    ## bonustotal         352  831.392  457.691   0.000   795.000  2,937.660
    ## age                357  20.501    1.315     19       21        22    
    ## tenure             357   7.622    2.661      2        9        10    
    ## commute            357  118.263   97.337    20       120       300   
    ## gender_num         357   1.000    0.000      1        1         1    
    ## married_num        357   0.000    0.000      0        0         0    
    ## high_educ_num      357   0.000    0.000      0        0         0    
    ## children_num       357   0.000    0.000      0        0         0    
    ## costofcommute      357   4.113    4.491    0.000    4.545    10.000  
    ## promote_switch_num 357   0.000    0.000      0        0         0    
    ## quitjob_num        357   1.000    0.000      1        1         1    
    ## group_index        357   7.000    0.000      7        7         7    
    ## ---------------------------------------------------------------------

``` r
# group definition
#group7 %>% select(gender, married, bedroom, children, high_educ, promote_switch, quitjob, homethatweek)
# distinct persons
unique(group7 %>%select(personid))
```

    ##     personid
    ## 1      40034
    ## 2      38862
    ## 4      44256
    ## 5      40346
    ## 26     42104
    ## 110    39478

``` r
nums <- dplyr::select(group7, where(is.numeric))
cm <- cor(nums, use = "pairwise.complete.obs")
```

    ## Warning in cor(nums, use = "pairwise.complete.obs"): the standard deviation is
    ## zero

``` r
ggcorrplot(cm, lab = FALSE)
```

![](notebook_files/figure-gfm/unnamed-chunk-32-1.png)<!-- -->

#### Group 16

338 observations for 6 volunteers

**Persona observations:**  
female  
not married  
number of bedrooms: 1  
no children  
high education  
no promotion  
worked for 23.5 months at the company when left (= avg)  
23.7 years old (\>= avg)  
  
**Other average weekly observations:**  
not working from home  
performed: 0 (= avg)  
basewage per month: 1598 (\< avg)  
bonus per month: 1362 (\< avg)  
grosswage per month: 2940 (\<=avg)  
cost of commute per month: 3.9 (\< avg)  
commute min per week: 52 (\<\< avg)  
logdaysworked: 1.65 (~= avg)

For this group the strong positive correlation between tenure and age
stands out. For the full sample it is slightly negative. Commute and age
have a much stronger negative corrleation

*Interpretation*  
Looks like a group of female juniors (wage, performance, tenure, age),
who already worked for almost 2 years, living somewhat central, probably
the first or second job college. Decent performance, not a top performer
but average. Probably left because they did not receive a promotion and
went for a higher paying job as their salary was still below average.

*Recommendation*  
As this group has an average performance while still being young, and
already has a higher education degree (so they probably wont leave for
education) our client should prioritize this group and try to give
employees in this group a raise and a promotion to keep them within the
company. Additionally offer working from home as a benifit, though is
not the greatest leverage for this group.

``` r
# select group data
group16 <- df %>% filter(group_index==16)
stargazer(as.data.frame(group16), type = "text", median = TRUE, header = TRUE)
```

    ## 
    ## =======================================================================
    ## Statistic           N    Mean    St. Dev.    Min     Median      Max   
    ## -----------------------------------------------------------------------
    ## bedroom            338   1.000    0.000       1         1         1    
    ## children           338   0.000    0.000       0         0         0    
    ## high_educ          338   1.000    0.000       1         1         1    
    ## promote_switch     338   0.000    0.000       0         0         0    
    ## quitjob            338   1.000    0.000       1         1         1    
    ## homethatweek       338   0.000    0.000       0         0         0    
    ## perform1           338  -0.0001   1.079    -3.031     0.049     2.863  
    ## phonecall          320  -0.038    1.045    -3.054     0.048     2.463  
    ## phonecallraw       307  454.397  154.865     51        457       910   
    ## logphonecall       306   6.046    0.419     3.932     6.124     6.813  
    ## logcallpersec      310  -5.159    0.141    -5.495    -5.151    -4.624  
    ## logcalllength      310  11.183    0.562     5.489    11.316    11.991  
    ## logcall_dayworked  310   9.483    0.485     3.697     9.552    10.100  
    ## logdaysworked      337   1.645    0.395     0.000     1.792     1.946  
    ## homethatweek_num   338   0.000    0.000       0         0         0    
    ## basewage           338 1,598.521  93.866    1,450     1,600     1,750  
    ## grosswage          334 2,940.331 837.862  1,215.600 2,713.000 6,153.380
    ## bonustotal         338 1,362.637 809.880    5.000   1,144.520 4,403.380
    ## age                338  23.683    1.309      22        23        26    
    ## tenure             338  23.485    6.243      13        24        30    
    ## commute            227  52.106    41.709      2        60        120   
    ## gender_num         338   1.000    0.000       1         1         1    
    ## married_num        338   0.000    0.000       0         0         0    
    ## high_educ_num      338   1.000    0.000       1         1         1    
    ## children_num       338   0.000    0.000       0         0         0    
    ## costofcommute      274   3.931    2.204       0         5         6    
    ## promote_switch_num 338   0.000    0.000       0         0         0    
    ## quitjob_num        338   1.000    0.000       1         1         1    
    ## group_index        338  16.000    0.000      16        16        16    
    ## -----------------------------------------------------------------------

``` r
# group definition
#group16 %>% select(gender, married, bedroom, children, high_educ, promote_switch, quitjob, homethatweek)
# distinct persons
unique(group16 %>%select(personid))
```

    ##    personid
    ## 1     31150
    ## 2     37276
    ## 3     26634
    ## 5     36494
    ## 41    29230
    ## 85    26934

``` r
nums <- dplyr::select(group16, where(is.numeric))
cm <- cor(nums, use = "pairwise.complete.obs")
```

    ## Warning in cor(nums, use = "pairwise.complete.obs"): the standard deviation is
    ## zero

``` r
ggcorrplot(cm, lab = FALSE)
```

![](notebook_files/figure-gfm/unnamed-chunk-33-1.png)<!-- -->

#### Group 40

643 observations for 12 volunteers

**Persona observations:**  
male  
not married  
number of bedrooms: 1  
no children  
no high education  
no promotion  
worked for 26.8 months at the company when left (= avg)  
22.8 years old (\<= avg)  
  
**Other average weekly observations:**  
not working from home  
performed: -0.46 (\<\< avg)  
basewage per month: 1492 (\< avg)  
bonus per month: 1149 (\< avg)  
grosswage per month: 2604 (\<=avg)  
cost of commute per month: 8.3 (\> avg)  
commute min per week: 119 (\<\< avg)  
logdaysworked: 1.7 (~= avg)

This correlation matrix is again very similar to the full sample matrix,
no outstanding differences.

*Interpretation*  
Young male group, with no high education and family, which has never
worked from home. Heavily underperformed, while working as many days as
the average. Lives somewhat far away (over avg) with avg cost for
commuting. Earns about 400 less than avereag despite the bad
performance. Group7 had similar circumstances, despite commute, but a
much better performance with lower salary. But the group is twice as big
as this group.

*Recommendation*

As the performance is worse than average, trying to keep this group of
workers should not be prioritized.

``` r
# select group data
group40 <- df %>% filter(group_index==40)
stargazer(as.data.frame(group40), type = "text", median = TRUE, header = TRUE)
```

    ## 
    ## =====================================================================
    ## Statistic           N    Mean    St. Dev.   Min    Median      Max   
    ## ---------------------------------------------------------------------
    ## bedroom            643   1.000    0.000      1        1         1    
    ## children           643   0.000    0.000      0        0         0    
    ## high_educ          643   0.000    0.000      0        0         0    
    ## promote_switch     643   0.000    0.000      0        0         0    
    ## quitjob            643   1.000    0.000      1        1         1    
    ## homethatweek       643   0.000    0.000      0        0         0    
    ## perform1           642  -0.462    0.899   -3.024   -0.446     2.540  
    ## phonecall          638  -0.392    0.900   -3.106   -0.377     2.554  
    ## phonecallraw       631  391.097  135.837     1       385       883   
    ## logphonecall       631   5.880    0.533    0.000    5.953     6.783  
    ## logcallpersec      629  -5.157    0.174   -5.609   -5.162    -3.178  
    ## logcalllength      630  11.038    0.572    3.178   11.131    11.824  
    ## logcall_dayworked  630   9.327    0.503    2.485    9.406    10.021  
    ## logdaysworked      642   1.697    0.265    0.000    1.792     1.946  
    ## homethatweek_num   643   0.000    0.000      0        0         0    
    ## basewage           629 1,492.130 142.227    650     1,500     1,900  
    ## grosswage          638 2,604.691 768.685  344.830 2,440.690 5,011.380
    ## bonustotal         638 1,149.647 684.029   0.000   966.000  3,461.380
    ## age                643  22.879    2.293     19       23        27    
    ## tenure             643  26.818    45.869     3       10        150   
    ## commute            612  119.649   51.741    30       120       200   
    ## gender_num         643   0.000    0.000      0        0         0    
    ## married_num        643   0.000    0.000      0        0         0    
    ## high_educ_num      643   0.000    0.000      0        0         0    
    ## children_num       643   0.000    0.000      0        0         0    
    ## costofcommute      643   8.283    2.400      4       10        12    
    ## promote_switch_num 643   0.000    0.000      0        0         0    
    ## quitjob_num        643   1.000    0.000      1        1         1    
    ## group_index        643  40.000    0.000     40       40        40    
    ## ---------------------------------------------------------------------

``` r
# group definition
#group40 %>% select(gender, married, bedroom, children, high_educ, promote_switch, quitjob, homethatweek)
# distinct persons
unique(group40 %>%select(personid))
```

    ##     personid
    ## 1      44794
    ## 3      43288
    ## 4      38552
    ## 7      38842
    ## 19     41332
    ## 55     40008
    ## 128    43524
    ## 136    35822
    ## 177    34890
    ## 184    41320
    ## 275    39096
    ## 405    42096

``` r
nums <- dplyr::select(group40, where(is.numeric))
cm <- cor(nums, use = "pairwise.complete.obs")
```

    ## Warning in cor(nums, use = "pairwise.complete.obs"): the standard deviation is
    ## zero

``` r
ggcorrplot(cm, lab = FALSE)
```

![](notebook_files/figure-gfm/unnamed-chunk-34-1.png)<!-- -->

#### Group 48

218 observations for 4 volunteers

**Persona observations:**  
male  
not married  
number of bedrooms: 1  
no children  
high education  
no promotion  
worked for 15.4 months at the company when left (= avg)  
21.8 years old (\<= avg)  
  
**Other average weekly observations:**  
not working from home  
performed: -0.612 (\<\< avg)  
basewage per month: 1566 (\< avg)  
bonus per month: 1087 (\< avg)  
grosswage per month: 2627 (\<=avg)  
cost of commute per month: 9.5 (\> avg)  
commute min per week: 33.4 (\<\< avg)  
logdaysworked: 1.7 (~= avg)

We can see a strong correlation between costofcommute, tenure and age
and a rather negative one between costofcommute, commute, tenure, age
and grosswage/bonustotal. But small sample size (less confidence).

*Interpretation*  
Young male group, with no education and no family, which has never
worked from home. Heavily underperformed, even more than males with high
education who quit, while working as many days as the average. Lives
closer to work than the average with slightly higher than avg cost for
commuting. Earns about 400 less than average despite the bad
performance. Unclear why they left, probably also because they wanted
more money/a promotion.

*Recommendation*

Because of the small sample size confidence is low. Tough, as the
performance is bad, trying to keep this group of workers should not be
prioritized. The company has to ask itself if it is willing to pay more
money for this performance,

``` r
# select group data
group48 <- df %>% filter(group_index==48)
stargazer(as.data.frame(group48), type = "text", median = TRUE, header = TRUE)
```

    ## 
    ## =====================================================================
    ## Statistic           N    Mean    St. Dev.   Min    Median      Max   
    ## ---------------------------------------------------------------------
    ## bedroom            218   1.000    0.000      1        1         1    
    ## children           218   0.000    0.000      0        0         0    
    ## high_educ          218   1.000    0.000      1        1         1    
    ## promote_switch     218   0.000    0.000      0        0         0    
    ## quitjob            218   1.000    0.000      1        1         1    
    ## homethatweek       218   0.000    0.000      0        0         0    
    ## perform1           218  -0.627    0.690   -2.601   -0.614     1.202  
    ## phonecall          216  -0.446    0.750   -2.618   -0.401     2.029  
    ## phonecallraw       218  365.362  104.638    65      373.5      695   
    ## logphonecall       216   5.845    0.379    4.174    5.923     6.544  
    ## logcallpersec      217  -5.210    0.159   -5.721   -5.193    -4.581  
    ## logcalllength      218  11.054    0.336    9.594   11.143    11.468  
    ## logcall_dayworked  218   9.379    0.298    8.157    9.422     9.880  
    ## logdaysworked      218   1.675    0.208    0.693    1.792     1.946  
    ## homethatweek_num   218   0.000    0.000      0        0         0    
    ## basewage           214 1,566.355 106.099   1,300    1,600     1,850  
    ## grosswage          218 2,627.424 519.862  971.380 2,523.000 3,784.760
    ## bonustotal         218 1,087.269 438.508  30.000   985.340  2,134.760
    ## age                218  21.844    1.314     20       22        23    
    ## tenure             218  15.486    12.637     4        4        34    
    ## commute            218  33.482    27.988     1       30        75    
    ## gender_num         218   0.000    0.000      0        0         0    
    ## married_num        218   0.000    0.000      0        0         0    
    ## high_educ_num      218   1.000    0.000      1        1         1    
    ## children_num       218   0.000    0.000      0        0         0    
    ## costofcommute      173   9.526    10.197     0        8        25    
    ## promote_switch_num 218   0.000    0.000      0        0         0    
    ## quitjob_num        218   1.000    0.000      1        1         1    
    ## group_index        218  48.000    0.000     48       48        48    
    ## ---------------------------------------------------------------------

``` r
# group definition
#group48 %>% select(gender, married, bedroom, children, high_educ, promote_switch, quitjob, homethatweek)
# distinct persons
unique(group48 %>%select(personid))
```

    ##    personid
    ## 1     32804
    ## 2     24324
    ## 9     42618
    ## 48    42634

``` r
nums <- dplyr::select(group48, where(is.numeric))
cm <- cor(nums, use = "pairwise.complete.obs")
```

    ## Warning in cor(nums, use = "pairwise.complete.obs"): the standard deviation is
    ## zero

``` r
ggcorrplot(cm, lab = FALSE)
```

![](notebook_files/figure-gfm/unnamed-chunk-35-1.png)<!-- -->

#### Group 60

202 observations for 4 volunteers

**Persona observations:**  
male  
married  
number of bedrooms: 1  
children  
high education  
no promotion  
worked for 22 months at the company when left (\> avg)  
27.8 years old (\> avg)  
  
**Other average weekly observations:**  
not working from home  
performed: -0.22 (\< avg)  
basewage per month: 1555 (\< avg)  
bonus per month: 1256 (\< avg)  
grosswage per month: 2784 (\<=avg)  
cost of commute per month: 20.5 (\>\> avg)  
commute min per week: 101.5 (= avg)  
logdaysworked: 1.5 (\<= avg)

We can see a strong correlation age, tenure and commute. And a moderate
between tenure, bonustotal, grosswage, basewage (steady salary raises(?)
But small sample size (less confidence).

*Interpretation*  
Older than avg. male group, with high education and family, which has
never worked from home. Underperformance a bit, while working as a
little less days as the average. Earn about the same amount less than
what they perform less. High cost of commute and average commute minutes
per week. Probably left because of this, and maybe the performance also
was affect by that. As the salary was steadily improved while working
their, this was probably not the main reason.

*Recommendation*

Because of the small sample size confidence is low. Tough, as the
performance is probably worse because of the high commute cost in
combination with having a family. A first important action can be the
offering of working from home to reduce negative aspects of commuting
and increasing performance. If performance actually rises, salary should
be increased as well according to that.

``` r
# select group data
group60 <- df %>% filter(group_index==60)
stargazer(as.data.frame(group60), type = "text", median = TRUE, header = TRUE)
```

    ## 
    ## =====================================================================
    ## Statistic           N    Mean    St. Dev.   Min    Median      Max   
    ## ---------------------------------------------------------------------
    ## bedroom            202   1.000    0.000      1        1         1    
    ## children           202   1.000    0.000      1        1         1    
    ## high_educ          202   1.000    0.000      1        1         1    
    ## promote_switch     202   0.000    0.000      0        0         0    
    ## quitjob            202   1.000    0.000      1        1         1    
    ## homethatweek       202   0.000    0.000      0        0         0    
    ## perform1           201  -0.216    1.292   -2.996   -0.008     2.164  
    ## phonecall          202  -0.182    1.343   -3.113    0.077     2.729  
    ## phonecallraw       184  435.120  181.523     2       461       823   
    ## logphonecall       184   5.904    0.790    0.693    6.133     6.713  
    ## logcallpersec      184  -5.074    0.196   -5.578   -5.124    -4.533  
    ## logcalllength      183  10.976    0.754    6.271   11.183    11.880  
    ## logcall_dayworked  183   9.416    0.631    5.578    9.599    10.088  
    ## logdaysworked      202   1.532    0.364    0.000    1.609     1.946  
    ## homethatweek_num   202   0.000    0.000      0        0         0    
    ## basewage           195 1,555.128 109.941   1,400    1,550     1,800  
    ## grosswage          195 2,784.271 680.297  672.590 2,549.860 4,434.900
    ## bonustotal         195 1,256.063 592.845  20.000  1,054.000 2,734.900
    ## age                202  27.847    1.740     25       28        30    
    ## tenure             202  22.064    16.113     8        9        47    
    ## commute            154  101.494   99.220    30       32        240   
    ## gender_num         202   0.000    0.000      0        0         0    
    ## married_num        202   1.000    0.000      1        1         1    
    ## high_educ_num      202   1.000    0.000      1        1         1    
    ## children_num       202   1.000    0.000      1        1         1    
    ## costofcommute      202  20.515    21.521     2        2        55    
    ## promote_switch_num 202   0.000    0.000      0        0         0    
    ## quitjob_num        202   1.000    0.000      1        1         1    
    ## group_index        202  60.000    0.000     60       60        60    
    ## ---------------------------------------------------------------------

``` r
# group definition
#group60 %>% select(gender, married, bedroom, children, high_educ, promote_switch, quitjob, homethatweek)
# distinct persons
unique(group60 %>%select(personid))
```

    ##    personid
    ## 1     40174
    ## 2     39942
    ## 6     16334
    ## 17    29808

``` r
nums <- dplyr::select(group60, where(is.numeric))
cm <- cor(nums, use = "pairwise.complete.obs")
```

    ## Warning in cor(nums, use = "pairwise.complete.obs"): the standard deviation is
    ## zero

``` r
ggcorrplot(cm, lab = FALSE)
```

![](notebook_files/figure-gfm/unnamed-chunk-36-1.png)<!-- -->

## Top performing 10%

### Select

``` r
df <- volunteer_endperiod_attitude_performance_wage

#df <- na.omit(df$perform1)

# define threshold
threshold <- quantile(df$perform1, 0.9, na.rm = TRUE)

# select data
df <- df %>%
  mutate(isTop10 = if_else(perform1 >= threshold, TRUE, FALSE)) %>% filter(!is.na(isTop10))

top10 <- df %>% filter(isTop10 == TRUE)
bottom90 <- df %>% filter(isTop10 == FALSE)
```

### Compare

Here we use the same approach as for the quitters:

``` r
# compare general statistics
stargazer(as.data.frame(top10), type = "text", median = TRUE, header = TRUE)
```

    ## 
    ## ========================================================================
    ## Statistic           N    Mean    St. Dev.     Min     Median      Max   
    ## ------------------------------------------------------------------------
    ## bedroom            983   0.961     0.193       0         1         1    
    ## children           983   0.239     0.427       0         0         1    
    ## high_educ          983   0.481     0.500       0         0         1    
    ## promote_switch     983   0.217     0.412       0         0         1    
    ## quitjob            983   0.182     0.386       0         0         1    
    ## homethatweek       983   0.254     0.436       0         0         1    
    ## exhaustion         189   6.804     6.105       0         6        25    
    ## negative           189  14.947     5.966       8        13        40    
    ## positive           189  26.619     5.710       8        28        40    
    ## perform1           983   1.643     0.430     1.169     1.522     4.163  
    ## phonecall          982   1.480     0.488     0.039     1.415     5.828  
    ## phonecallraw       982  665.302   93.572      426       649      1,264  
    ## logphonecall       980   6.491     0.134     6.054     6.475     7.142  
    ## logcallpersec      979  -5.125     0.140    -5.458    -5.120    -3.832  
    ## logcalllength      980  11.615     0.160    10.231    11.610    12.116  
    ## logcall_dayworked  981   9.818     0.162     8.285     9.827    10.359  
    ## logdaysworked      982   1.796     0.137     1.386     1.792     1.946  
    ## homethatweek_num   983   0.254     0.436       0         0         1    
    ## basewage           980 1,658.520  183.865    1,300     1,600     2,300  
    ## grosswage          978 3,746.791 1,058.139 1,375.000 3,532.000 8,418.000
    ## bonustotal         969 2,092.682  951.870   70.000   1,941.000 6,268.000
    ## age                983  23.834     5.606       1        23        34    
    ## tenure             983  32.765    25.032       2        28        150   
    ## commute            947  107.360   69.337       2        80        300   
    ## gender_num         983   0.752     0.432       0         1         1    
    ## married_num        983   0.308     0.462       0         0         1    
    ## high_educ_num      983   0.481     0.500       0         0         1    
    ## children_num       983   0.239     0.427       0         0         1    
    ## costofcommute      955   7.019     7.280     0.000     5.000    55.000  
    ## promote_switch_num 983   0.217     0.412       0         0         1    
    ## quitjob_num        983   0.182     0.386       0         0         1    
    ## group_index        983  23.146    17.373       1        16        62    
    ## isTop10            983   1.000     0.000       1         1         1    
    ## ------------------------------------------------------------------------

``` r
stargazer(as.data.frame(bottom90), type = "text", median = TRUE, header = TRUE)
```

    ## 
    ## =======================================================================
    ## Statistic            N     Mean    St. Dev.  Min    Median      Max    
    ## -----------------------------------------------------------------------
    ## bedroom            8,844   0.975    0.156     0        1         1     
    ## children           8,844   0.132    0.339     0        0         1     
    ## high_educ          8,844   0.392    0.488     0        0         1     
    ## promote_switch     8,844   0.177    0.381     0        0         1     
    ## quitjob            8,844   0.249    0.433     0        0         1     
    ## homethatweek       8,844   0.190    0.392     0        0         1     
    ## exhaustion         1,976   8.742    7.861   0.000    8.000     36.000  
    ## negative           1,976  16.800    6.861   8.000   16.000     40.000  
    ## positive           1,976  23.948    6.699   8.000   24.000     40.000  
    ## perform1           8,844  -0.199    0.851   -3.031  -0.073     1.169   
    ## phonecall          8,714  -0.172    0.854   -3.113  -0.046     3.812   
    ## phonecallraw       8,587  414.708  123.300    1       429       977    
    ## logphonecall       8,574   5.955    0.476   0.000    6.061     6.884   
    ## logcallpersec      8,571  -5.175    0.160   -5.951  -5.173     -1.099  
    ## logcalllength      8,571  11.128    0.511   2.485   11.237     11.883  
    ## logcall_dayworked  8,569   9.438    0.448   2.485    9.514     10.186  
    ## logdaysworked      8,830   1.674    0.288   0.000    1.792     1.946   
    ## homethatweek_num   8,844   0.190    0.392     0        0         1     
    ## basewage           8,727 1,591.062 170.406   650     1,550     2,450   
    ## grosswage          8,774 2,990.829 951.818  48.850 2,792.915 14,553.000
    ## bonustotal         8,761 1,416.748 872.868  0.000  1,207.000 12,853.000
    ## age                8,844  23.444    4.026     1       23         35    
    ## tenure             8,844  23.011    24.617    2       13        150    
    ## commute            7,936  103.154   67.580    1       80        300    
    ## gender_num         8,844   0.472    0.499     0        0         1     
    ## married_num        8,844   0.170    0.375     0        0         1     
    ## high_educ_num      8,844   0.392    0.488     0        0         1     
    ## children_num       8,844   0.132    0.339     0        0         1     
    ## costofcommute      8,532   7.471    7.346   0.000    6.000     55.000  
    ## promote_switch_num 8,844   0.177    0.381     0        0         1     
    ## quitjob_num        8,844   0.249    0.433     0        0         1     
    ## group_index        8,844  29.117    17.646    1       37         62    
    ## isTop10            8,844   0.000    0.000     0        0         0     
    ## -----------------------------------------------------------------------

``` r
# compare distributions
ggplot(df, aes(x=isTop10, y=perform1)) + geom_boxplot()
```

![](notebook_files/figure-gfm/unnamed-chunk-38-1.png)<!-- -->

``` r
ggplot(df, aes(x=isTop10, y=tenure)) + geom_boxplot()
```

![](notebook_files/figure-gfm/unnamed-chunk-38-2.png)<!-- -->

``` r
ggplot(df, aes(x=isTop10, y=age)) + geom_boxplot()
```

![](notebook_files/figure-gfm/unnamed-chunk-38-3.png)<!-- -->

``` r
ggplot(df, aes(x=isTop10, y=commute)) + geom_boxplot()
```

    ## Warning: Removed 944 rows containing non-finite outside the scale range
    ## (`stat_boxplot()`).

![](notebook_files/figure-gfm/unnamed-chunk-38-4.png)<!-- -->

``` r
ggplot(df, aes(x=isTop10, y=costofcommute)) + geom_boxplot()
```

    ## Warning: Removed 340 rows containing non-finite outside the scale range
    ## (`stat_boxplot()`).

![](notebook_files/figure-gfm/unnamed-chunk-38-5.png)<!-- -->

``` r
ggplot(df, aes(x=isTop10, y=basewage)) + geom_boxplot()
```

    ## Warning: Removed 120 rows containing non-finite outside the scale range
    ## (`stat_boxplot()`).

![](notebook_files/figure-gfm/unnamed-chunk-38-6.png)<!-- -->

``` r
ggplot(df, aes(x=isTop10, y=bonustotal)) + geom_boxplot()
```

    ## Warning: Removed 97 rows containing non-finite outside the scale range
    ## (`stat_boxplot()`).

![](notebook_files/figure-gfm/unnamed-chunk-38-7.png)<!-- -->

``` r
ggplot(df, aes(x=isTop10, y=grosswage)) + geom_boxplot()
```

    ## Warning: Removed 75 rows containing non-finite outside the scale range
    ## (`stat_boxplot()`).

![](notebook_files/figure-gfm/unnamed-chunk-38-8.png)<!-- -->

``` r
# create correlation matrices
nums <- dplyr::select(top10, where(is.numeric))
cm <- cor(nums, use = "pairwise.complete.obs")
```

    ## Warning in cor(nums, use = "pairwise.complete.obs"): the standard deviation is
    ## zero

``` r
ggcorrplot(cm, lab = FALSE)
```

![](notebook_files/figure-gfm/unnamed-chunk-38-9.png)<!-- -->

``` r
nums <- dplyr::select(bottom90, where(is.numeric))
cm <- cor(nums, use = "pairwise.complete.obs")
```

    ## Warning in cor(nums, use = "pairwise.complete.obs"): the standard deviation is
    ## zero

``` r
ggcorrplot(cm, lab = FALSE)
```

![](notebook_files/figure-gfm/unnamed-chunk-38-10.png)<!-- -->

The average employee among the top 10% performing ones, has no children,
though it is more likely than for the other 90%, works 25% oft the time
from home, is almost 24 years old, works for 32 months at the company
and an average commute and average cost of commute. Almost 22% receive a
promotion, whereas 17.7% of lower performing people receive one. They
earn over the average by bonuses, but have a average base salary. Though
some not high performing individuals receive enormous bonuses. Only 18%
high performing people quit while 25% quit from the other 90%.

Top performer are less likely to quit when receiving a higher bonus than
non performer, but more likely if they have a long commute as this is
negatively correlative with their negative attitude. Working from home
does not affect performance of both groups. Working from home increases
positivity while lowering negativity and exhaustion for the top
performer.

*Recommendations*

- Bonuses combined with goals/milestones to motivate people and increase
  performance, but only if you reach them

- check existing bonuses for not high performing people and reevaluate
  if that is fair, this might be a motivation killer

- offer more working from home for high performer, as it does not affect
  their performance and rather increases positivity

# Hypothesis testing
