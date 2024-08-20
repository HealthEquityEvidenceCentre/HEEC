# General Practice Workforce

[NHS
Digital](https://digital.nhs.uk/data-and-information/publications/statistical/general-and-personal-medical-services)
provides [public
CSVs](https://digital.nhs.uk/data-and-information/publications/statistical/general-and-personal-medical-services)
on the primary care general practice workforce.

Data is reported annually from September 2015-2020, quarterly from
December 2020-June 2021, and monthly from July 2021 henceforth.

The practice-level headcount of fully-qualified permanent GPs (excluding
GPs in training & locums) is denoted by the TOTAL_GP_EXTGL_HC column.

``` r
workforce <- data.frame()

for (file in list.files("raw")) {
  print(file)

  df <- read.csv(paste0("raw/", file))[c("PRAC_CODE", "TOTAL_PATIENTS", "TOTAL_GP_EXTGL_HC")]

  parts <- strsplit(file, "_")[[1]]
  year <- as.numeric(parts[1])
  month <- as.numeric(gsub("\\.csv", "", parts[2]))

  df$Year <- year
  df$Month <- month

  # Determine financial year
  if (month >= 4) {
    df$fiscal_year <- year + 1
  } else {
    df$fiscal_year <- year
  }

  workforce <- rbind(workforce, df)
}
```

    ## [1] "15_9.csv"
    ## [1] "16_9.csv"
    ## [1] "17_9.csv"
    ## [1] "18_9.csv"
    ## [1] "19_9.csv"
    ## [1] "20_12.csv"
    ## [1] "20_9.csv"
    ## [1] "21_10.csv"
    ## [1] "21_11.csv"
    ## [1] "21_12.csv"
    ## [1] "21_3.csv"
    ## [1] "21_6.csv"
    ## [1] "21_7.csv"
    ## [1] "21_8.csv"
    ## [1] "21_9.csv"
    ## [1] "22_1.csv"
    ## [1] "22_10.csv"
    ## [1] "22_11.csv"
    ## [1] "22_12.csv"
    ## [1] "22_2.csv"
    ## [1] "22_3.csv"
    ## [1] "22_4.csv"
    ## [1] "22_5.csv"
    ## [1] "22_6.csv"
    ## [1] "22_7.csv"
    ## [1] "22_8.csv"
    ## [1] "22_9.csv"
    ## [1] "23_1.csv"
    ## [1] "23_10.csv"
    ## [1] "23_11.csv"
    ## [1] "23_12.csv"
    ## [1] "23_2.csv"
    ## [1] "23_3.csv"
    ## [1] "23_4.csv"
    ## [1] "23_5.csv"
    ## [1] "23_6.csv"
    ## [1] "23_7.csv"
    ## [1] "23_8.csv"
    ## [1] "23_9.csv"
    ## [1] "24_1.csv"

``` r
library(dplyr)
```

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
library(magrittr)

workforce$TOTAL_GP_EXTGL_HC %<>% as.numeric()
```

    ## Warning in workforce$TOTAL_GP_EXTGL_HC %<>% as.numeric(): NAs introduced by
    ## coercion

``` r
# Calculate average workforce across all available values in each financial year
workforce_year <- workforce %>%
  group_by(PRAC_CODE, fiscal_year) %>%
  summarise(
    TOTAL_PATIENTS = mean(TOTAL_PATIENTS, na.rm = TRUE),
    TOTAL_GP_EXTGL_HC = round(mean(TOTAL_GP_EXTGL_HC, na.rm = TRUE), 0),
  )
```

    ## `summarise()` has grouped output by 'PRAC_CODE'. You can override using the
    ## `.groups` argument.

``` r
workforce_year <- workforce_year %>%
  rename(., Practice.Code = PRAC_CODE)

write.csv(workforce_year, "workforce_year.csv", row.names = FALSE)
```
