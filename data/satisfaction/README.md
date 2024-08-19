# GP Patient Survey Data

The GP Patient Survey is an independent survey run by Ipsos MORI on
behalf of NHS England on the [GP Patient Survey
website](https://gp-patient.co.uk/). The survey is sent out to over a
million people across the UK and covers a range of topics related to the
quality of care provided by GP practices. The data from the survey is
used to assess patient experience and satisfaction with GP services.

Annual practice-level .csv files are available \[here\].

Execute the following code to merge the data to create a single
time-series dataset:

``` r
library(magrittr)
library(dplyr)

practice_information <- c("Practice_Code", "Practice_Name", "CCG_Code", "CCG_Name", "ICS_Code", "ICS_Name")

# Define the variable mappings as a named list
variable_mappings <- list(
  access_pct = "Q3_12pct",
  continuity_pct = "Q9_12pct",
  overall_pct = "Q28_12pct",
  trust_pct_default = "Q89_12pct",
  trust_pct_2017 = "Q22_12pct"
)

# Initialize empty data frames
satisfaction <- data.frame()
trust <- data.frame()

# Function to load and process a single file
process_file <- function(file, variables) {
  df <- read.csv(paste0("raw/", file))

  # Select CCG or ICB Code
  dataset_columns <- colnames(df)
  selected_column_names <- intersect(practice_information, dataset_columns)
  df <- df[, c(selected_column_names, variables)]

  # Assign year
  year <- substr(file, 1, nchar(file) - 4)
  df$Year <- as.numeric(year)

  return(df)
}

# Process all files for satisfaction
for (file in list.files("raw/")) {
  df <- process_file(file, unlist(variable_mappings[-c(4, 5)])) # Exclude trust variables, as the variable code is not consistent; it will be collated separately
  satisfaction <- bind_rows(satisfaction, df)
}

# Process a subset of files for trust
for (file in list.files("raw/")) {
  year <- as.numeric(substr(file, 1, nchar(file) - 4))
  trust_variable <- ifelse(year == 2017, variable_mappings$trust_pct_2017, variable_mappings$trust_pct_default)
  df <- process_file(file, c("Practice_Code", trust_variable))

  # Rename the trust column to a common name for merging
  colnames(df)[colnames(df) == trust_variable] <- "trust_pct"

  trust <- bind_rows(trust, df)
}

# Keep only the relevant columns in trust
trust <- trust[, c("Practice_Code", "trust_pct", "Year")]

# Merge satisfaction and trust data frames
satisfaction <- merge(satisfaction, trust, by = c("Practice_Code", "Year"), all.x = TRUE)

# Rename columns based on variable_mappings
for (new_name in names(variable_mappings)[-c(4, 5)]) { # Exclude trust variables
  old_name <- variable_mappings[[new_name]]
  colnames(satisfaction)[colnames(satisfaction) == old_name] <- new_name
}

# Rename Practice_Code and Practice_Name
satisfaction <- satisfaction %>%
  rename(
    `Practice.Code` = Practice_Code,
    `Practice.Name` = Practice_Name
  )

# drop rows where pct is negative
satisfaction$overall_pct <- ifelse(satisfaction$overall_pct < 0, NA, satisfaction$overall_pct)
satisfaction$access_pct <- ifelse(satisfaction$access_pct < 0, NA, satisfaction$access_pct)
satisfaction$continuity_pct <- ifelse(satisfaction$continuity_pct < 0, NA, satisfaction$continuity_pct)
satisfaction$trust_pct <- ifelse(satisfaction$trust_pct < 0, NA, satisfaction$trust_pct)

head(satisfaction)
```

    ##   Practice.Code Year       Practice.Name CCG_Code
    ## 1        A81001 2017 THE DENSHAM SURGERY      00K
    ## 2        A81001 2018 THE DENSHAM SURGERY      00K
    ## 3        A81001 2019 THE DENSHAM SURGERY      00K
    ## 4        A81001 2020 THE DENSHAM SURGERY      16C
    ## 5        A81001 2021 THE DENSHAM SURGERY      16C
    ## 6        A81001 2022 THE DENSHAM SURGERY     <NA>
    ##                                  CCG_Name access_pct continuity_pct overall_pct
    ## 1 NHS Hartlepool and Stockton-on-Tees CCG  0.2830746      0.7742824   0.6852379
    ## 2 NHS HARTLEPOOL AND STOCKTON-ON-TEES CCG  0.4064632      0.7515997   0.8362756
    ## 3 NHS HARTLEPOOL AND STOCKTON-ON-TEES CCG  0.4654917      0.5346624   0.8148972
    ## 4                     NHS TEES VALLEY CCG  0.5293993      0.5355755   0.9185904
    ## 5                     NHS TEES VALLEY CCG  0.5622530      0.5961309   0.8811130
    ## 6                                    <NA>  0.5061685      0.5122277   0.8506803
    ##   ICS_Code                                            ICS_Name trust_pct
    ## 1     <NA>                                                <NA> 0.8424006
    ## 2     <NA>                                                <NA> 0.9281149
    ## 3     <NA>                                                <NA> 0.9396292
    ## 4     <NA>                                                <NA> 0.9824796
    ## 5     <NA>                                                <NA> 0.9473904
    ## 6      QHM North East and North Cumbria Integrated Care System 0.9780686

``` r
satisfaction %>%
  group_by(Year) %>%
  summarise(
    mean_overall = mean(overall_pct, na.rm = TRUE),
    sd_overall = sd(overall_pct, na.rm = TRUE),
    min_overall = min(overall_pct, na.rm = TRUE),
    max_overall = max(overall_pct, na.rm = TRUE),
    n = n()
  )
```

    ## # A tibble: 8 × 6
    ##    Year mean_overall sd_overall min_overall max_overall     n
    ##   <dbl>        <dbl>      <dbl>       <dbl>       <dbl> <int>
    ## 1  2017        0.849     0.0972       0.289           1  7522
    ## 2  2018        0.840     0.0982       0.373           1  7254
    ## 3  2019        0.834     0.0984       0.322           1  6999
    ## 4  2020        0.824     0.103        0.370           1  6821
    ## 5  2021        0.834     0.0943       0.296           1  6658
    ## 6  2022        0.734     0.131        0.276           1  6507
    ## 7  2023        0.727     0.134        0.112           1  6418
    ## 8  2024        0.754     0.123        0.244           1  6307

Use the merge_and_assign_quintiles function in the data_processing.R
script to merge the data with the IMD data and assign quintiles:

``` r
source("../data_processing.R")

satisfaction <- read.csv("satisfaction.csv")

# Call the function to merge and assign national-level quintiles
satisfaction <- merge_and_assign_quintiles(
  data = satisfaction
)
```

    ##   Practice.Code Year       Practice.Name CCG_Code
    ## 1        A81001 2017 THE DENSHAM SURGERY      00K
    ## 2        A81001 2018 THE DENSHAM SURGERY      00K
    ## 3        A81001 2019 THE DENSHAM SURGERY      00K
    ## 4        A81001 2020 THE DENSHAM SURGERY      16C
    ## 5        A81001 2021 THE DENSHAM SURGERY      16C
    ## 6        A81001 2022 THE DENSHAM SURGERY     <NA>
    ##                                  CCG_Name access_pct continuity_pct overall_pct
    ## 1 NHS Hartlepool and Stockton-on-Tees CCG  0.2830746      0.7742824   0.6852379
    ## 2 NHS HARTLEPOOL AND STOCKTON-ON-TEES CCG  0.4064632      0.7515997   0.8362756
    ## 3 NHS HARTLEPOOL AND STOCKTON-ON-TEES CCG  0.4654917      0.5346624   0.8148972
    ## 4                     NHS TEES VALLEY CCG  0.5293993      0.5355755   0.9185904
    ## 5                     NHS TEES VALLEY CCG  0.5622530      0.5961309   0.8811130
    ## 6                                    <NA>  0.5061685      0.5122277   0.8506803
    ##   ICS_Code                                            ICS_Name trust_pct
    ## 1     <NA>                                                <NA> 0.8424006
    ## 2     <NA>                                                <NA> 0.9281149
    ## 3     <NA>                                                <NA> 0.9396292
    ## 4     <NA>                                                <NA> 0.9824796
    ## 5     <NA>                                                <NA> 0.9473904
    ## 6      QHM North East and North Cumbria Integrated Care System 0.9780686
    ##        IMD
    ## 1 30.75062
    ## 2 31.55578
    ## 3 32.36094
    ## 4 32.36094
    ## 5 32.36094
    ## 6 32.36094
    ## [1] "Processing Year: 2017"
    ## [1] "Processing Year: 2018"
    ## [1] "Processing Year: 2019"
    ## [1] "Processing Year: 2020"
    ## [1] "Processing Year: 2021"
    ## [1] "Processing Year: 2022"
    ## [1] "Processing Year: 2023"

``` r
write.csv(satisfaction, "satisfaction.csv", row.names = FALSE)
```
