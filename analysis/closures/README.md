# Practice Closures Trigger Analysis

In this analysis, we attempt to quantify and categorise the number of
GPs practices that have closed in England each year from 2015 to 2023.

``` r
library(magrittr)

for (file in list.files("../../data/payments/raw/")) {
  n_prac <- read.csv(paste0("../../data/payments/raw/", file))$Practice.Code %>%
    unique() %>%
    length()

  if (file == "14-15.csv") {
    n_prac_15_payments <- n_prac
  } else if (file == "22-23.csv") {
    n_prac_23_payments <- n_prac
  }

  print(paste0(sub(".csv$", "", file), ": ", n_prac))
}
```

    ## [1] "14-15: 7959"
    ## [1] "15-16: 7841"
    ## [1] "16-17: 7763"
    ## [1] "17-18: 7543"
    ## [1] "18-19: 7279"
    ## [1] "19-20: 7001"
    ## [1] "20-21: 6808"
    ## [1] "21-22: 6758"
    ## [1] "22-23: 6669"

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
the column `Practice.Code.Date`. No further information is provided on
the type of closure.

``` r
library(dplyr)

closure <- data.frame()

for (file in list.files("../../data/payments/raw/")) {
  df <- read.csv(paste0("../../data/payments/raw/", file))[c("Practice.Code", "Practice.Open.Date", "Practice.Close.Date")]

  # assign year
  year <- substr(file, 1, nchar(file) - 4)
  year <- substr(year, 4, nchar(year))
  year <- paste0("20", year)
  df$Year <- year

  # replace empty values with NA
  df$Practice.Open.Date[df$Practice.Open.Date %in% c("-", "")] <- NA
  df$Practice.Close.Date[df$Practice.Close.Date %in% c("-", "", " ")] <- NA

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
  summarise(n = n())
```

    ## # A tibble: 9 × 2
    ##   Year      n
    ##   <chr> <int>
    ## 1 2015    144
    ## 2 2016    107
    ## 3 2017    136
    ## 4 2018    230
    ## 5 2019    203
    ## 6 2020    138
    ## 7 2021    156
    ## 8 2022     79
    ## 9 2023     34
