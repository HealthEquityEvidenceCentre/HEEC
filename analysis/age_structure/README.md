# Proportion of GP Registered Populations by Age Group

``` r
# Load necessary libraries
library(httr)
library(readr)
library(magrittr)
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
payments23 <- read.csv("../../data/payments/raw/22-23.csv")

print(paste0(
  "Total practices, 22,23: ",
  payments23$Practice.Code %>% unique() %>% length()
))
```

    ## [1] "Total practices, 22,23: 6669"

``` r
print(paste0(
  "Total expenditure, 22/23: £",
  format(payments23 %>%
    summarise(Total.expenditure = sum(Total.NHS.Payments.to.General.Practice, na.rm = TRUE)) %>%
    pull(Total.expenditure), digits = 1, big.mark = ",", scientific = FALSE)
))
```

    ## [1] "Total expenditure, 22/23: £10,230,094,804"

``` r
print(paste0(
  "Total patients, 22/23:",
  format(payments23 %>%
    summarise(
      Total.patients = sum(Number.of.Registered.Patients..Last.Known.Figure., na.rm = TRUE)
    ) %>%
    pull(Total.patients), digits = 1, big.mark = ",", scientific = FALSE)
))
```

    ## [1] "Total patients, 22/23:62,134,645"

``` r
average_payment <- payments23 %>%
  summarise(
    Total.expenditure = sum(Total.NHS.Payments.to.General.Practice, na.rm = TRUE),
    Total.patients = sum(Number.of.Registered.Patients..Last.Known.Figure., na.rm = TRUE)
  ) %>%
  mutate(Payment.per.patient = Total.expenditure / Total.patients) %>%
  pull(Payment.per.patient) # Extract the single value

print(paste0(
  "Average payment per patient, 22/23: £",
  format(average_payment, digits = 5, big.mark = ",", scientific = FALSE)
))
```

    ## [1] "Average payment per patient, 22/23: £164.64"

### API

This data can be accessed through the
[API](https://fingertips.phe.org.uk/api) provided by the Fingertips
platform.

``` r
# Define the URL and query parameters
base_url <- "https://fingertipsws.phe.org.uk/api/all_data/csv/by_indicator_id"
query_params <- list(
  v = "/0-c459298b/",
  # parent_area_code = "E92000001",
  # parent_area_type_id = 167,
  # child_area_type_id = 7,
  indicator_ids = 93468
)

# Make the API request
# response <- GET(base_url, query = query_params)

# # Check if the response is successful
# if (http_status(response)$category == "Success") {
#   # Write the content to a temporary file
#   temp_file <- tempfile(fileext = ".csv")
#   writeBin(content(response, "raw"), temp_file)
# 
#   # Read the CSV data
#   age <- read_csv(temp_file)
# 
#   # Display the first few rows of the data
#   print(head(age))
# } else {
#   cat("Failed to retrieve data. Status code:", status_code(response), "\n")
#   cat("Response content:", content(response, "text"), "\n")
# }
# 
# write.csv(age, "raw_age.csv")
# 
# age %<>%
#   rename(Practice.Code = `Area Code`) %>%
#   rename(Year = `Time period`) %>%
#   filter(Practice.Code != "E92000001") %>%
#   filter(Age == "65+ yrs") %>%
#   mutate(prop65_quintile = ntile(Value, 5)) %>%
#   select("Practice.Code", "Year", "Value", "prop65_quintile") %>%
#   rename(prop65 = Value)
# 
# write.csv(age, "age.csv", row.names = FALSE)
```

``` r
age <- read.csv("../../data/age_group/age.csv") %>%
  filter(Year == 2023)

df <- merge(age, payments23, by = "Practice.Code")

IMD <- read.csv("../../data/IMD/IMD_interpolated.csv") %>%
  filter(Year == 2023) %>%
  select(-Year)

df <- merge(df, IMD, by = "Practice.Code") %>%
  mutate(prop65_quintile = ntile(prop65, 5)) %>%
  mutate(IMD_quintile = ntile(IMD, 5))

df %>%
  group_by(prop65_quintile) %>%
  summarise(
    IMDm = mean(IMD, na.rm = TRUE),
    IMDsd = sd(IMD, na.rm = TRUE),
    prop65m = mean(prop65, na.rm = TRUE),
    prop65sd = sd(prop65, na.rm = TRUE),
    n = n()
  )
```

    ## # A tibble: 5 × 6
    ##   prop65_quintile  IMDm IMDsd prop65m prop65sd     n
    ##             <int> <dbl> <dbl>   <dbl>    <dbl> <int>
    ## 1               1  30.9 11.5     7.90     2.49  1276
    ## 2               2  27.7 12.2    13.8      1.37  1275
    ## 3               3  23.2 11.2    17.9      1.10  1275
    ## 4               4  19.2  9.28   21.7      1.17  1275
    ## 5               5  15.9  6.28   28.2      5.24  1275

``` r
df %>%
  group_by(IMD_quintile) %>%
  summarise(
    IMDm = mean(IMD, na.rm = TRUE),
    IMDsd = sd(IMD, na.rm = TRUE),
    prop65m = mean(prop65, na.rm = TRUE),
    prop65sd = sd(prop65, na.rm = TRUE),
    n = n()
  )
```

    ## # A tibble: 5 × 6
    ##   IMD_quintile  IMDm IMDsd prop65m prop65sd     n
    ##          <int> <dbl> <dbl>   <dbl>    <dbl> <int>
    ## 1            1  9.80  2.02    21.5     5.91  1276
    ## 2            2 15.4   1.56    21.2     7.45  1275
    ## 3            3 21.4   1.84    18.2     7.32  1275
    ## 4            4 28.6   2.38    15.2     6.95  1275
    ## 5            5 41.7   6.98    13.3     5.62  1275

About 1,000 practices are in rural areas. IMD is strongly associated
with rurality (by definition: Living Environment = 9.3% of IMD
composite): more than 98.5% of practices in the most deprived quintile
are urban, compared to about half of the least deprived quintile. Older
patients tend to live in less deprived areas, with rural practices
showing lower average deprivation.

``` r
df %>%
  group_by(Practice.Rurality) %>%
  summarise(
    IMDm = mean(IMD, na.rm = TRUE),
    IMDsd = sd(IMD, na.rm = TRUE),
    prop65m = mean(prop65, na.rm = TRUE),
    prop65sd = sd(prop65, na.rm = TRUE),
    n = n()
  )
```

    ## # A tibble: 2 × 6
    ##   Practice.Rurality  IMDm IMDsd prop65m prop65sd     n
    ##   <chr>             <dbl> <dbl>   <dbl>    <dbl> <int>
    ## 1 Rural              15.1  6.19    25.5     5.45  1064
    ## 2 Urban              25.0 11.8     16.4     6.81  5312

Why did practices with older populations receive more payments per
patient?

``` r
df %<>%
  filter(Total.NHS.Payments.to.General.Practice > 0) %>%
  filter(Average.payments.per.registered.patient < 600)

mean(df$Total.NHS.Payments.to.General.Practice, na.rm = TRUE)
```

    ## [1] 1585514

``` r
quantile(df$Total.NHS.Payments.to.General.Practice, probs = seq(0, 1, by = 0.2), na.rm = TRUE)
```

    ##          0%         20%         40%         60%         80%        100% 
    ##       50.16   777709.33  1170347.46  1573733.78  2180999.82 16455794.75

``` r
median(df$Number.of.Registered.Patients..Last.Known.Figure.)
```

    ## [1] 8420.25

``` r
quantile(df$Number.of.Registered.Patients..Last.Known.Figure., probs = seq(0, 1, by = 0.2), na.rm = TRUE)
```

    ##        0%       20%       40%       60%       80%      100% 
    ##    210.00   4944.90   7234.85   9713.80  13235.90 109484.50

``` r
df %<>% mutate(average.payment = Total.NHS.Payments.to.General.Practice / Number.of.Registered.Patients..Last.Known.Figure.)

quantile(df$average.payment, probs = seq(0, 1, by = 0.2), na.rm = TRUE)
```

    ##           0%          20%          40%          60%          80%         100% 
    ## 7.703294e-03 1.342021e+02 1.472735e+02 1.606986e+02 1.837331e+02 5.938784e+02

``` r
df %>%
  # group_by(Practice.Rurality) %>%
  summarise(
    IMDm = mean(IMD),
    IMDsd = sd(IMD),
    prop65m = mean(prop65),
    # prop65sd = sd(prop65),
    average.paymentm = mean(average.payment),
    average.paymentsd = sd(average.payment)
  ) %>% pull(average.paymentsd)
```

    ## [1] 52.6076
