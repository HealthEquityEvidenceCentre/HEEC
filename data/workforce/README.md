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

  df <- read.csv(paste0("raw/", file))[c("PRAC_CODE", "TOTAL_PATIENTS", "TOTAL_GP_EXTGL_HC", "TOTAL_GP_EXTGL_FTE")]

  parts <- strsplit(file, "_")[[1]]
  year <- as.numeric(parts[1])
  month <- as.numeric(gsub("\\.csv", "", parts[2]))

  df$Year <- year
  df$Month <- month

  # Determine financial year
  if (month >= 4) {
    df$fiscal_year <- paste0("20", year + 1)
  } else {
    df$fiscal_year <- paste0("20", year)
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
workforce$TOTAL_GP_EXTGL_FTE %<>% as.numeric()
```

    ## Warning in workforce$TOTAL_GP_EXTGL_FTE %<>% as.numeric(): NAs introduced by
    ## coercion

``` r
# Calculate average workforce across all available values in each financial year
workforce_year <- workforce %>%
  group_by(PRAC_CODE, fiscal_year) %>%
  summarise(
    TOTAL_PATIENTS = mean(TOTAL_PATIENTS, na.rm = TRUE),
    TOTAL_GP_EXTGL_HC = round(mean(TOTAL_GP_EXTGL_HC, na.rm = TRUE), 0),
    TOTAL_GP_EXTGL_FTE = round(mean(TOTAL_GP_EXTGL_FTE, na.rm = TRUE), 1)
  )
```

    ## `summarise()` has grouped output by 'PRAC_CODE'. You can override using the
    ## `.groups` argument.

``` r
workforce_year <- workforce_year %>%
  rename(., Practice.Code = PRAC_CODE, Year = fiscal_year)
```

``` r
source("../data_processing.R")

# Call the function to merge and assign national-level quintiles
workforce_year <- merge_and_assign_quintiles(
  data = workforce_year,
  start_year = 2016,
  end_year = 2024
)
```

    ## [1] "Year: 2016"
    ## 
    ##    1    2    3    4    5 
    ## 1524 1524 1523 1524 1524 
    ## [1] "Year: 2017"
    ## 
    ##    1    2    3    4    5 
    ## 1508 1507 1507 1507 1508 
    ## [1] "Year: 2018"
    ## 
    ##    1    2    3    4    5 
    ## 1467 1466 1466 1466 1467 
    ## [1] "Year: 2019"
    ## 
    ##    1    2    3    4    5 
    ## 1427 1426 1426 1426 1426 
    ## [1] "Year: 2020"
    ## 
    ##    1    2    3    4    5 
    ## 1372 1371 1371 1371 1371 
    ## [1] "Year: 2021"
    ## 
    ##    1    2    3    4    5 
    ## 1327 1327 1327 1327 1327 
    ## [1] "Year: 2022"
    ## 
    ##    1    2    3    4    5 
    ## 1313 1313 1312 1313 1313 
    ## [1] "Year: 2023"
    ## 
    ##    1    2    3    4    5 
    ## 1295 1295 1295 1295 1295 
    ## [1] "Year: 2024"
    ## 
    ##    1    2    3    4    5 
    ## 1273 1273 1273 1273 1273

``` r
write.csv(workforce_year, "workforce_year.csv", row.names = FALSE)
```

``` r
library(ggplot2)

agg <- workforce_year %>%
  group_by(Year, IMD_quintile) %>%
  summarise(
    TOTAL_PATIENTS = sum(TOTAL_PATIENTS, na.rm = TRUE),
    TOTAL_GP_EXTGL_FTE = sum(TOTAL_GP_EXTGL_FTE, na.rm = TRUE),
    TOTAL_GP_PER_100k_PATIENTS = sum(TOTAL_GP_EXTGL_FTE, na.rm = TRUE) / sum(TOTAL_PATIENTS, na.rm = TRUE) * 100000
  )
```

    ## `summarise()` has grouped output by 'Year'. You can override using the
    ## `.groups` argument.

``` r
colors <- c("#EF7A34", "#00A865", "#007AA8", "#531A5C", "#A80026")

ggplot(agg[!is.na(agg$IMD_quintile), ], aes(x = Year, y = TOTAL_GP_PER_100k_PATIENTS, group = IMD_quintile, color = IMD_quintile)) +
  geom_line(size = 1.5) +
  geom_point(size = 3) +
  labs(
    title = "Fully qualified GPs FTE per 100,000 patients by IMD Quintile",
    x = "Year",
    y = "Total GP FTE"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    legend.justification = "center",
    axis.text.x = element_text(size = 10),
    axis.title.x = element_text(size = 12),
    axis.line.x = element_line(size = 1),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank()
  ) +
  scale_color_manual(values = colors, labels = c("Q1 (least deprived)", "Q2", "Q3", "Q4", "Q5 (most deprived)")) +
  labs(color = "IMD quintile")
```

    ## Warning: Using `size` aesthetic for lines was deprecated in ggplot2 3.4.0.
    ## ℹ Please use `linewidth` instead.
    ## This warning is displayed once every 8 hours.
    ## Call `lifecycle::last_lifecycle_warnings()` to see where this warning was
    ## generated.

    ## Warning: The `size` argument of `element_line()` is deprecated as of ggplot2 3.4.0.
    ## ℹ Please use the `linewidth` argument instead.
    ## This warning is displayed once every 8 hours.
    ## Call `lifecycle::last_lifecycle_warnings()` to see where this warning was
    ## generated.

![](README_files/figure-markdown_github/plot%20workforce%20FTE-1.png)
