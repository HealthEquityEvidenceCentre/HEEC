# Quality and Outcomes Framework (QOF) Data

The Quality and Outcomes Framework (QOF) is a voluntary annual reward
and incentive programme for all GP practices in England, detailing
practice achievement results. It is not about performance management but
resourcing and rewarding good practice.

### API

This data can be accessed through the
[API](https://fingertips.phe.org.uk/api) provided by the Fingertips
platform.

``` r
# Load necessary libraries
library(httr)
library(readr)
library(magrittr)
library(dplyr)

# Define the URL and query parameters
base_url <- "https://fingertipsws.phe.org.uk/api/all_data/csv/by_indicator_id"
query_params <- list(
  v = "/0-c459298b/",
  parent_area_code = "E92000001",
  parent_area_type_id = 167,
  child_area_type_id = 7,
  indicator_ids = 295
)

# Make the API request
response <- GET(base_url, query = query_params)

# Check if the response is successful
if (http_status(response)$category == "Success") {
  # Write the content to a temporary file
  temp_file <- tempfile(fileext = ".csv")
  writeBin(content(response, "raw"), temp_file)

  # Read the CSV data
  QOF <- read_csv(temp_file)

  # Display the first few rows of the data
  print(head(QOF))
} else {
  cat("Failed to retrieve data. Status code:", status_code(response), "\n")
  cat("Response content:", content(response, "text"), "\n")
}
```

    ## # A tibble: 6 × 27
    ##   `Indicator ID` `Indicator Name`      `Parent Code` `Parent Name` `Area Code`
    ##            <dbl> <chr>                 <chr>         <chr>         <chr>      
    ## 1            295 % QOF points achieved <NA>          <NA>          E92000001  
    ## 2            295 % QOF points achieved E92000001     England       E38000006  
    ## 3            295 % QOF points achieved E92000001     England       E38000007  
    ## 4            295 % QOF points achieved E92000001     England       E38000008  
    ## 5            295 % QOF points achieved E92000001     England       E38000014  
    ## 6            295 % QOF points achieved E92000001     England       E38000015  
    ## # ℹ 22 more variables: `Area Name` <chr>, `Area Type` <chr>, Sex <chr>,
    ## #   Age <chr>, `Category Type` <lgl>, Category <lgl>, `Time period` <chr>,
    ## #   Value <dbl>, `Lower CI 95.0 limit` <lgl>, `Upper CI 95.0 limit` <lgl>,
    ## #   `Lower CI 99.8 limit` <lgl>, `Upper CI 99.8 limit` <lgl>, Count <dbl>,
    ## #   Denominator <dbl>, `Value note` <chr>, `Recent Trend` <chr>,
    ## #   `Compared to England value or percentiles` <chr>,
    ## #   `Compared to CCGs (from Apr 2021) value or percentiles` <chr>, …

``` r
head(QOF)
```

    ## # A tibble: 6 × 27
    ##   `Indicator ID` `Indicator Name`      `Parent Code` `Parent Name` `Area Code`
    ##            <dbl> <chr>                 <chr>         <chr>         <chr>      
    ## 1            295 % QOF points achieved <NA>          <NA>          E92000001  
    ## 2            295 % QOF points achieved E92000001     England       E38000006  
    ## 3            295 % QOF points achieved E92000001     England       E38000007  
    ## 4            295 % QOF points achieved E92000001     England       E38000008  
    ## 5            295 % QOF points achieved E92000001     England       E38000014  
    ## 6            295 % QOF points achieved E92000001     England       E38000015  
    ## # ℹ 22 more variables: `Area Name` <chr>, `Area Type` <chr>, Sex <chr>,
    ## #   Age <chr>, `Category Type` <lgl>, Category <lgl>, `Time period` <chr>,
    ## #   Value <dbl>, `Lower CI 95.0 limit` <lgl>, `Upper CI 95.0 limit` <lgl>,
    ## #   `Lower CI 99.8 limit` <lgl>, `Upper CI 99.8 limit` <lgl>, Count <dbl>,
    ## #   Denominator <dbl>, `Value note` <chr>, `Recent Trend` <chr>,
    ## #   `Compared to England value or percentiles` <chr>,
    ## #   `Compared to CCGs (from Apr 2021) value or percentiles` <chr>, …

``` r
write.csv(QOF, "qof.csv")
```
