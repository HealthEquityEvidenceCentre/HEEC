# Health-related Behaviours Data

NHS Digital published selected indicators calculated for GP practices in
key areas of clinical care and public health. Includes recorded disease
prevalence, achievement rates and personalised care adjustment data.

Indicator groups include: - Lifestyle (smoking, obesity) -
Cardiovascular (Atrial fibrillation, Secondary prevention of coronary
heart disease, Cholesterol control and lipid management, Heart Failure,
Hypertension, Peripheral arterial disease, Stroke and transient
ischaemic attack)

``` r
library(magrittr)
library(dplyr)

# Smoking
smok_prev <- read.csv("raw/smok_qof_prev.csv") %>%
  filter(Area.Type == "GPs") %>%
  select(Indicator.Name, Area.Code, Area.Name, Time.period, Value)

# Obesity
obes_prev <- read.csv("raw/obesity_qof_prev.csv") %>%
  filter(Area.Type == "GPs") %>%
  select(Indicator.Name, Area.Code, Area.Name, Time.period, Value)

# Hypertension
bp_prev <- read.csv("raw/hypertension_qof_prevalence.csv") %>%
  filter(Area.Type == "GPs") %>%
  select(Indicator.Name, Area.Code, Area.Name, Time.period, Value)

prev <- bind_rows(smok_prev, obes_prev, bp_prev) %>% rename(
  Indicator = Indicator.Name,
  Practice.Code = Area.Code,
  Year = Time.period
)

# drop 3 characters following 20 in prev$Year
prev$Year <- substr(prev$Year, 1, 4) %>% as.integer() + 1

prev %<>% filter(Year > 2015)
```

``` r
source("../data_processing.R")

prev <- merge_and_assign_quintiles(
  prev,
  start_year = 2016,
  end_year = 2023
)
```

    ## [1] "Year: 2016"
    ## 
    ##    1    2    3    4    5 
    ## 2529 2529 2528 2528 2528 
    ## [1] "Year: 2017"
    ## 
    ##    1    2    3    4    5 
    ## 2514 2513 2513 2513 2513 
    ## [1] "Year: 2018"
    ## 
    ##    1    2    3    4    5 
    ## 2508 2507 2507 2507 2507 
    ## [1] "Year: 2019"
    ## 
    ##    1    2    3    4    5 
    ## 2521 2521 2520 2520 2520 
    ## [1] "Year: 2020"
    ## 
    ##    1    2    3    4    5 
    ## 2538 2538 2538 2537 2537 
    ## [1] "Year: 2021"
    ## 
    ##    1    2    3    4    5 
    ## 2539 2539 2538 2538 2538 
    ## [1] "Year: 2022"
    ## 
    ##    1    2    3    4    5 
    ## 2537 2537 2537 2537 2537 
    ## [1] "Year: 2023"
    ## 
    ##    1    2    3    4    5 
    ## 3809 3809 3809 3809 3809

``` r
prev <- assign_icb_name(
  prev
)

write.csv(prev, "behaviours.csv", row.names = FALSE)
```
