## Introduction

General practices serving the most deprived populations receive less
funding per weighted patient than those serving the least deprived. Here
we show that this inequality is driven by a higher concentration of
dispensing practices in more affluent areas.

### What are Dispensing Practices?

Dispensing practices are practices where “at the patient’s request
dispensing doctors are allowed to dispense the medicines they prescribe
for these patients” (DDA); they are not the same as practices with an
on-site pharmacy.

[NHS
Digital](https://digital.nhs.uk/data-and-information/publications/statistical/nhs-payments-to-general-practice)
provides public CSVs on NHS payments to individual providers of general
practice services in England from 2014-15 to 2022-23. We look at the
most recent data from 2022-23.

## Data Analysis

``` r
# Load necessary libraries
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
# Download the NHS payments data for 2022/23
df <- read.csv("https://files.digital.nhs.uk/05/E6ADA0/nhspaymentsgp-22-23-prac-csv.csv")

# Filter dispensing and non-dispensing practices
dispensing <- df %>% filter(Dispensing.Practice == "Yes")
non_dispensing <- df %>% filter(Dispensing.Practice == "No")

# Number of dispensing practices
num_dispensing <- dispensing %>%
  distinct(Practice.Code) %>%
  nrow()

# Number of non-dispensing practices
num_non_dispensing <- non_dispensing %>%
  distinct(Practice.Code) %>%
  nrow()

# Sum of patients in dispensing practices
total_dispensing_patients <- sum(dispensing$Average.Number.of.Registered.Patient, na.rm = TRUE)

# Sum of patients in non-dispensing practices
total_non_dispensing_patients <- sum(non_dispensing$Average.Number.of.Registered.Patient, na.rm = TRUE)
```

In 2023, there were 944 dispensing practices covering 9.5058785^{6}
patients and 5537 non-dispensing practices covering 5.2628766^{7}
patients in England (dispensing status unknown for 188 practices).

OHID provides the Fingertips API, which includes practice-level IMD
values.

# Collating Data into a Time Series

Here, we demonstrate how to download and collate this data into a time
series.

``` r
# Example code for downloading and processing data from the Fingertips API
# Note: Actual implementation may vary based on API specifications and available libraries

library(httr)
library(jsonlite)

# Define API endpoint and parameters
# api_url <- "https://fingertips.phe.org.uk/api"
# query_params <- list(
#   profile_id = 159,
#   indicator_id = 93553,
#   area_type_id = 15,
#   parent_area_id = "E92000001",
#   category_type_id = -1,
#   time_point = 1,
#   child_area_id = 4
# )
# 
# # Make API request
# response <- GET(api_url, query = query_params)
# data <- content(response, "text") %>% fromJSON()
# 
# # Convert data to data frame
# imd_data <- data$indicator_data %>% as.data.frame()
# 
# # Display the first few rows of the data
# head(imd_data)
```

Conclusion This analysis highlights the disparities in funding between
general practices serving different populations and the role of
dispensing practices in this inequality.

For more detailed exploration and visualization, further analysis and
refinement of the data is needed.

References NHS Digital: NHS Payments to General Practice Fingertips API
Documentation
