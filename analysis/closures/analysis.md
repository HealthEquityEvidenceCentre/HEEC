# Practice Closures Trigger Analysis

``` r
library(magrittr)
### Satisfaction analysis

### Workforce analysis
wf15 <- read.csv("../../data/workforce/raw/15_9.csv")
wf23 <- read.csv("../../data/workforce/raw/23_9.csv")

pay15 <- read.csv("../../data/payments/raw/14-15.csv")
pay23 <- read.csv("../../data/payments/raw/22-23.csv")

fte_15 <- wf15$TOTAL_GP_EXTGL_FTE %>%
  as.numeric() %>%
  sum(na.rm = TRUE)

fte_23 <- wf23$TOTAL_GP_EXTGL_FTE %>%
  as.numeric() %>%
  sum(na.rm = TRUE)
```

General practice is in crisis: patient satisfaction is at a record low;
the number of fully qualified GPs has dropped from 2.7368998^{4} in 2015
to 2.6452591^{4} in 2023 (full-time equivalent, excluding GPs in
training and locums).

``` r
n_prac_15 <- wf15$PRAC_CODE %>%
  unique() %>%
  length()
n_prac_23 <- wf23$PRAC_CODE %>%
  unique() %>%
  length()

n_prac_15_payments <- pay15$Practice.Code %>%
  unique() %>%
  length()

n_prac_23_payments <- pay23$Practice.Code %>%
  unique() %>%
  length()
```

Over the past decade there has been a move towards providing general
practice at scale to benefit from economies of scale and to ensure
practices are more sustainable. This has led to a dramatic reduction in
the number of practices from 7623 in 2015 to 6334 in 2023, according to
the workforce data.

There are some discrepancies with other data sources, however; according
to the payments data, the number of practices has dropped from 7959 in
2015 to 6669 in 2023.

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

ICBs and NHS England are often unaware of a failing practice until it is
too late to intervene. The ability to identify practices or areas of the
country which are at risk of failing general practices would help
commissioners to intervene where necessary or plan for patient care in
the event of a practice closure.

# Closed practice count

The NHS Payments data contains information on practice closures. We can
use this data to identify practices that have closed in the past decade.

Where multiple closure dates are available for a practice, we will keep
the earliest date.

``` r
library(dplyr)

agg <- data.frame()
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
