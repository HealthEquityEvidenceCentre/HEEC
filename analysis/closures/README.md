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

``` r
# Number of practices per year
```

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

# Methods

### NHS Payments data

The NHS Payments data contains information on practice closures, under
the column `Practice.Code.Date`. However, the Practice.Close.Date is
often not consistent between years; where duplicated, we will use the
earliest reported date of closure for each practice.

``` r
library(dplyr)
library(tidyverse)
library(lubridate)

closure <- data.frame()

for (file in list.files("../../data/payments/raw/")) {
  df <- read.csv(paste0("../../data/payments/raw/", file))[c("Practice.Code", "Practice.Close.Date")]

  # # assign year
  # year <- substr(file, 1, nchar(file) - 4)
  # year <- substr(year, 4, nchar(year))
  # year <- paste0("20", year)
  # df$Year <- year

  # replace empty values with NA
  df$Practice.Close.Date[df$Practice.Close.Date %in% c("-", "", " ")] <- NA

  # Convert Practice.Close.Date to Date format
  df <- df %>%
    mutate(Practice.Close.Date = dmy(Practice.Close.Date))

  # Extract year and update Year column
  df <- df %>%
    mutate(Year = year(Practice.Close.Date))

  # if Practice.Close.Date is not NA, add row to closure dataframe
  closure <- df %>%
    filter(!is.na(Practice.Close.Date)) %>%
    select(Practice.Code, Practice.Close.Date, Year) %>%
    bind_rows(closure)
}

# remove duplicates, keeping lowest value of year
closure <- closure %>%
  group_by(Practice.Code) %>%
  filter(Year == min(Year))

# print number of closed practices per year
closure %>%
  group_by(Year) %>%
  summarise(n = n()) %>%
  print()
```

    ## # A tibble: 11 × 2
    ##     Year     n
    ##    <dbl> <int>
    ##  1  2013     3
    ##  2  2014    98
    ##  3  2015   152
    ##  4  2016   158
    ##  5  2017   225
    ##  6  2018   230
    ##  7  2019   135
    ##  8  2020   166
    ##  9  2021    89
    ## 10  2022    86
    ## 11  2023     5

While this gives us an idea of the number of practices that have closed
each year, it does not give us information about the type of closure,
and consequently the impact on patients.

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
