# Practice Closures Trigger Analysis

In this analysis, we attempt to quantify and categorise the number of
GPs practices that have closed in England each year from 2015 to 2023.

``` r
library(magrittr)

for (file in list.files("../../data/payments/raw/")) {
  n_prac <- read.csv(paste0("../../data/payments/raw/", file))$Practice.Code %>%
    unique() %>%
    length()

  # assign year
  year <- file %>%
    substr(1, nchar(.) - 4) %>%
    substr(4, nchar(.)) %>%
    paste0("20", .) %>%
    as.numeric()

  if (file == "14-15.csv") {
    n_prac_15_payments <- n_prac
  } else if (file == "22-23.csv") {
    n_prac_23_payments <- n_prac
  }

  print(paste0(year, ": ", n_prac))
}
```

    ## [1] "2015: 7959"
    ## [1] "2016: 7841"
    ## [1] "2017: 7763"
    ## [1] "2018: 7543"
    ## [1] "2019: 7279"
    ## [1] "2020: 7001"
    ## [1] "2021: 6808"
    ## [1] "2022: 6758"
    ## [1] "2023: 6669"

Over the past decade, there has been a dramatic reduction in the number
of practices from 7959 in 2015 to 6669 in 2023, according to the number
of unique practice codes in the annual [NHS Payments
data](https://github.com/camappel/HEEC/tree/main/data/payments).

The reasons for and consequences of practice closures are multifaceted.
Practices may merge with other practices for purely organisational
reasons or because one practice is failing. Mergers may be purely
administrative with limited impact on patients, or they may involve
closure of premises or redesign of services.

Practices may also close altogether, leaving thousands of patients
without access to primary care until they find another surgery. Practice
closures are costly in that they incur interruptions in services for
patients. One reason for closure is when the partnership model fails
within a practice; this partnership model is increasingly under strain
because younger doctors are less keen to become a partner due to the
financial and workload commitment.

We begin by categorising types of closures:

- Merged: Horizontal integration of 2 or more practices. The practice
  code changes to a pre-existing practice code.
  - Administrative: premises are unchanged, limited impact on workforce
    and patients
  - Estate restructure: premises close, staff are let go and patient
    satisfaction falls
- Closed: The practice code no longer exists
  - Practice no longer receives payments, has no registered payments,
    the premises close, and staff are let go.
- Open: The practice has had the same practice code since it opened and
  still exists in the most recent records

# Identifying closed practices

## NHS Payments data

The NHS Payments data contains information on practice closures, under
the column `Practice.Code.Date`.

``` r
library(dplyr)
library(tidyverse)
library(lubridate)

closure <- data.frame()

for (file in list.files("../../data/payments/raw/")) {
  df <- read.csv(paste0("../../data/payments/raw/", file))[c("Practice.Code", "Practice.Close.Date")]

  # assign file year
  year <- substr(file, 1, nchar(file) - 4)
  year <- substr(year, 4, nchar(year))
  year <- paste0("20", year)
  df$reportedYear <- year

  # replace empty values with NA
  df$Practice.Close.Date[df$Practice.Close.Date %in% c("-", "", " ")] <- NA

  # Convert Practice.Close.Date to Date format
  df <- df %>%
    mutate(Practice.Close.Date = dmy(Practice.Close.Date))

  # Extract year and update Year column
  df <- df %>%
    mutate(closureYear = year(Practice.Close.Date))

  # if Practice.Close.Date is not NA, add row to closure dataframe
  closure <- df %>%
    filter(!is.na(Practice.Close.Date)) %>%
    bind_rows(closure)
}

# remove duplicates, keeping lowest value of year
closure <- closure %>%
  group_by(Practice.Code) %>%
  filter(closureYear == min(closureYear))

# sort by closure year
closure %>%
  arrange(closureYear) %>%
  print()
```

    ## # A tibble: 1,347 × 4
    ## # Groups:   Practice.Code [1,227]
    ##    Practice.Code Practice.Close.Date reportedYear closureYear
    ##    <chr>         <date>              <chr>              <dbl>
    ##  1 Y02424        2013-07-31          2023                2013
    ##  2 J82651        2013-10-31          2022                2013
    ##  3 Y02424        2013-07-31          2022                2013
    ##  4 J82620        2014-08-31          2022                2014
    ##  5 L83132        2014-11-30          2022                2014
    ##  6 F85680        2014-10-16          2016                2014
    ##  7 N85056        2014-06-30          2015                2014
    ##  8 Y02664        2014-06-30          2015                2014
    ##  9 Y02673        2014-08-31          2015                2014
    ## 10 Y02880        2014-12-14          2015                2014
    ## # ℹ 1,337 more rows

``` r
# print number of closed practices per year
closure %>%
  group_by(closureYear) %>%
  summarise(n = n()) %>%
  print()
```

    ## # A tibble: 11 × 2
    ##    closureYear     n
    ##          <dbl> <int>
    ##  1        2013     3
    ##  2        2014    98
    ##  3        2015   152
    ##  4        2016   158
    ##  5        2017   225
    ##  6        2018   230
    ##  7        2019   135
    ##  8        2020   166
    ##  9        2021    89
    ## 10        2022    86
    ## 11        2023     5

From this, we can get a rough idea of the number of practices that have
closed each year. However, this data does not provide information about
the type of closure (merged, closed, open), and consequently the impact
on patients.

Furthermore, the Practice.Close.Date is often not consistent between
years; where duplicated, we will use the earliest reported date of
closure for each practice. `reportedYear` refers to the year in which
the closure was reported in the annual NHS Payments data, while
`closureYear` refers to the year in which the practice closed.

As such, practices that are reported to have closed in year ‘t’ still
receive payments and report patients in year ‘t+1’.

## [Sidhu et al. (2023)](https://www.journalslibrary.nihr.ac.uk/hsdr/PRWQ4012/#/bn1)

A study on the impact of vertical integration whereby acute hospitals
run primary care medical practices found that “At 31 March 2021, 26 NHS
trusts were in vertically integrated organisations, running 85 general
practices across 116 practice sites”.

## Descriptive statistics

### What is the distribution of deprivation of practices that have closed?

We can use the [Index of Multiple Deprivation
(IMD)](https://github.com/HealthEquityEvidenceCentre/HEEC/tree/main/data/IMD)
to determine the distribution of deprivation of practices that have
closed.

``` r
IMD <- read.csv("../../data/IMD/IMD_interpolated.csv")

# all practices
IMD %>%
  group_by(Year) %>%
  summarise(
    mean_IMD = mean(IMD, na.rm = TRUE),
    sd_IMD = sd(IMD, na.rm = TRUE),
    min_IMD = min(IMD, na.rm = TRUE),
    max_IMD = max(IMD, na.rm = TRUE),
    n = n()
  )
```

    ## # A tibble: 14 × 6
    ##     Year mean_IMD sd_IMD min_IMD max_IMD     n
    ##    <int>    <dbl>  <dbl>   <dbl>   <dbl> <int>
    ##  1  2010     24.2   12.8    2.60    68.9  8222
    ##  2  2011     24.2   12.6    2.77    68.4  8222
    ##  3  2012     24.1   12.4    2.95    67.9  8222
    ##  4  2013     24.1   12.2    3.13    67.4  8222
    ##  5  2014     24.1   12.1    3.31    67.0  8222
    ##  6  2015     24.1   12.0    3.21    66.5  8438
    ##  7  2016     24.1   11.9    3.21    66.5  8438
    ##  8  2017     24.1   11.9    3.21    67.1  8438
    ##  9  2018     24.1   11.9    3.21    67.9  8438
    ## 10  2019     24.1   11.9    3.21    68.7  8461
    ## 11  2020     24.1   11.9    3.21    68.7  8461
    ## 12  2021     24.1   11.9    3.21    68.7  8461
    ## 13  2022     24.1   11.9    3.21    68.7  8461
    ## 14  2023     24.1   11.9    3.21    68.7  8461

``` r
# how many practices in closure are not in IMD
closure$Practice.Code[!closure$Practice.Code %in% IMD$Practice.Code] %>% length()
```

    ## [1] 10

``` r
# merge closure with IMD by Practice.Code and reportedYear = Year
closure_IMD <- merge(closure, IMD, by.x = c("Practice.Code", "reportedYear"), by.y = c("Practice.Code", "Year"))

closure_IMD %>%
  group_by(closureYear) %>%
  summarise(
    mean_IMD = mean(IMD, na.rm = TRUE),
    sd_IMD = sd(IMD, na.rm = TRUE),
    min_IMD = min(IMD, na.rm = TRUE),
    max_IMD = max(IMD, na.rm = TRUE),
    n = n()
  )
```

    ## # A tibble: 11 × 6
    ##    closureYear mean_IMD sd_IMD min_IMD max_IMD     n
    ##          <dbl>    <dbl>  <dbl>   <dbl>   <dbl> <int>
    ##  1        2013     25.0   2.46   22.2     26.4     3
    ##  2        2014     27.3  13.1     3.31    56.6    96
    ##  3        2015     24.6  12.1     5.14    54.6   149
    ##  4        2016     27.0  11.6     5.43    55.6   157
    ##  5        2017     28.2  12.1     3.21    65.2   224
    ##  6        2018     26.0  12.8     3.49    66.5   229
    ##  7        2019     24.9  11.9     5.10    56.2   135
    ##  8        2020     23.7  12.6     5.60    53.9   166
    ##  9        2021     28.2  10.9     7.02    55.5    89
    ## 10        2022     22.4  10.1     5.89    45.6    84
    ## 11        2023     24.6   9.06   12.0     36.7     5

We perform a Welch Two Sample t-test to determine if the mean IMD of
closed practices is significantly different from the mean IMD of all
practices.

``` r
t.test(closure_IMD$IMD, IMD$IMD, alternative = "two.sided")
```

    ## 
    ##  Welch Two Sample t-test
    ## 
    ## data:  closure_IMD$IMD and IMD$IMD
    ## t = 5.4173, df = 1366.2, p-value = 7.141e-08
    ## alternative hypothesis: true difference in means is not equal to 0
    ## 95 percent confidence interval:
    ##  1.155727 2.467897
    ## sample estimates:
    ## mean of x mean of y 
    ##  25.93179  24.11997

Based on the results of the t-test, we can conclude that there *is* a
statistically significant difference between the IMD values of closed
practices and all practices. Specifically, the closed practices sample
tend to have higher IMD values.

### Notes

We could also try counting practice codes in each year that are not
present in the following years.

``` r
library(dplyr)

# read in data
df <- read.csv("../../data/payments/raw/14-15.csv")[c("Practice.Code", "Practice.Open.Date", "Practice.Close.Date")]
df$Year <- "2015"
```

<https://www.gponline.com/map-gp-practice-mergers-closures-nhs-england/article/1439293>
<https://www.gponline.com/exclusive-one-twenty-gp-contracts-terminated-three-years-closures-accelerate/article/1404813>
Subregional figures, obtained using the Freedom of Information Act,
reveal for the first time the full and escalating extent of disruptive
practice closures, mergers and takeovers.

Commissioners provided details of 140 practice contracts that had been
terminated because of a practice closure between 1 April 2013 and 21
March 2016. The number of closures more than doubled from 26 in 2013/14
to 59 the following year, with a further 55 before the end of last year.

A further 264 contracts were identified as having been merged over the
three-year period. More detailed information from some areas indicated
that around half of those practices involved in mergers may have been
closed.

The data on contract terminations revealed a further 26 practices that
had been reprocured under a new contract. Of these 11 GMS and five PMS
practices were handed to an APMS provider.

The total number of contract terminations - in which practices closed or
merged - increased from 54 in 2013/14 to 158 the next year and up to 192
before the end of 2015/16.

Common reasons for closures provided by commissioners included GP
retirements and contract resignations.
