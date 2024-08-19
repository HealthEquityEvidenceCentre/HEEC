# Index of Multiple Deprivation (IMD) Data

The Office for Health Improvement and Disparities (OHID), part of the
Department of Health and Social Care (DHSC), provides the [Index of
Multiple Deprivation (IMD)
data](https://fingertips.phe.org.uk/search/deprivation%20index#page/4/gid/1/pat/159/par/K02000001/ati/15/are/E92000001/iid/93553/age/1/sex/4/cat/-1/ctp/-1/yrr/1/cid/4/tbm/1) -
a measure of relative deprivation - at the practice level. This data is
used to assess the socio-economic status of the population served by
each practice.

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
  indicator_ids = 93553
)

# Make the API request
response <- GET(base_url, query = query_params)

# Check if the response is successful
if (http_status(response)$category == "Success") {
  # Write the content to a temporary file
  temp_file <- tempfile(fileext = ".csv")
  writeBin(content(response, "raw"), temp_file)

  # Read the CSV data
  IMD <- read_csv(temp_file)

  # Display the first few rows of the data
  print(head(IMD))
} else {
  cat("Failed to retrieve data. Status code:", status_code(response), "\n")
  cat("Response content:", content(response, "text"), "\n")
}
```

    ## # A tibble: 6 × 27
    ##   `Indicator ID` `Indicator Name`        `Parent Code` `Parent Name` `Area Code`
    ##            <dbl> <chr>                   <chr>         <chr>         <chr>      
    ## 1          93553 Deprivation score (IMD… <NA>          <NA>          E92000001  
    ## 2          93553 Deprivation score (IMD… E92000001     England       E38000217  
    ## 3          93553 Deprivation score (IMD… E38000247     NHS Tees Val… A81001     
    ## 4          93553 Deprivation score (IMD… E38000247     NHS Tees Val… A81002     
    ## 5          93553 Deprivation score (IMD… E38000247     NHS Tees Val… A81004     
    ## 6          93553 Deprivation score (IMD… E38000247     NHS Tees Val… A81005     
    ## # ℹ 22 more variables: `Area Name` <chr>, `Area Type` <chr>, Sex <chr>,
    ## #   Age <chr>, `Category Type` <lgl>, Category <lgl>, `Time period` <dbl>,
    ## #   Value <dbl>, `Lower CI 95.0 limit` <lgl>, `Upper CI 95.0 limit` <lgl>,
    ## #   `Lower CI 99.8 limit` <lgl>, `Upper CI 99.8 limit` <lgl>, Count <lgl>,
    ## #   Denominator <lgl>, `Value note` <lgl>, `Recent Trend` <chr>,
    ## #   `Compared to England value or percentiles` <chr>,
    ## #   `Compared to CCGs (from Apr 2021) value or percentiles` <chr>, …

``` r
head(IMD)
```

    ## # A tibble: 6 × 27
    ##   `Indicator ID` `Indicator Name`        `Parent Code` `Parent Name` `Area Code`
    ##            <dbl> <chr>                   <chr>         <chr>         <chr>      
    ## 1          93553 Deprivation score (IMD… <NA>          <NA>          E92000001  
    ## 2          93553 Deprivation score (IMD… E92000001     England       E38000217  
    ## 3          93553 Deprivation score (IMD… E38000247     NHS Tees Val… A81001     
    ## 4          93553 Deprivation score (IMD… E38000247     NHS Tees Val… A81002     
    ## 5          93553 Deprivation score (IMD… E38000247     NHS Tees Val… A81004     
    ## 6          93553 Deprivation score (IMD… E38000247     NHS Tees Val… A81005     
    ## # ℹ 22 more variables: `Area Name` <chr>, `Area Type` <chr>, Sex <chr>,
    ## #   Age <chr>, `Category Type` <lgl>, Category <lgl>, `Time period` <dbl>,
    ## #   Value <dbl>, `Lower CI 95.0 limit` <lgl>, `Upper CI 95.0 limit` <lgl>,
    ## #   `Lower CI 99.8 limit` <lgl>, `Upper CI 99.8 limit` <lgl>, Count <lgl>,
    ## #   Denominator <lgl>, `Value note` <lgl>, `Recent Trend` <chr>,
    ## #   `Compared to England value or percentiles` <chr>,
    ## #   `Compared to CCGs (from Apr 2021) value or percentiles` <chr>, …

### FingertipsR Package

The IMD data can also be accessed using the `fingertipsR`
[package](https://github.com/ropensci/fingertipsR).

``` r
# Load necessary library
library(fingertipsR)

# Get the profile ID for the Public Health Outcomes Framework
profiles_data <- profiles()
phof_profile <- profiles_data[profiles_data$ProfileName == "Public Health Outcomes Framework", ]
profile_id <- phof_profile$ProfileID[1]

# Print the profile details (optional)
print(phof_profile)
```

    ## # A tibble: 6 × 4
    ##   ProfileID ProfileName                        DomainID DomainName              
    ##       <int> <chr>                                 <int> <chr>                   
    ## 1        19 Public Health Outcomes Framework    1000049 A. Overarching indicato…
    ## 2        19 Public Health Outcomes Framework    1000041 B. Wider determinants o…
    ## 3        19 Public Health Outcomes Framework    1000042 C. Health improvement   
    ## 4        19 Public Health Outcomes Framework    1000043 D. Health protection    
    ## 5        19 Public Health Outcomes Framework    1000044 E. Healthcare and prema…
    ## 6        19 Public Health Outcomes Framework 1938132983 Supporting information

``` r
# Get indicators for the Public Health Outcomes Framework profile
indicators_data <- indicators(ProfileID = profile_id)

# Find the IndicatorID for "Deprivation score (IMD 2019)"
indicator_id <- indicators_data$IndicatorID[indicators_data$IndicatorName == "Deprivation score (IMD 2019)"]

# Print the indicator details (optional)
print(indicators_data[indicators_data$IndicatorName == "Deprivation score (IMD 2019)", ])
```

    ## # A tibble: 1 × 6
    ##   IndicatorID IndicatorName            DomainID DomainName ProfileID ProfileName
    ##         <int> <fct>                       <int> <chr>          <int> <chr>      
    ## 1       93553 Deprivation score (IMD …   1.94e9 Supportin…        19 Public Hea…

``` r
# Get the data for the "Deprivation score (IMD 2019)" indicator
IMD <- fingertips_data(IndicatorID = indicator_id, AreaTypeID = 7) # AreaTypeID 7 is for CCGs

# Display the first few rows of the data
head(IMD)
```

    ##   IndicatorID                IndicatorName ParentCode                ParentName
    ## 1       93553 Deprivation score (IMD 2019)       <NA>                      <NA>
    ## 2       93553 Deprivation score (IMD 2019)     U89141              Stockton PCN
    ## 3       93553 Deprivation score (IMD 2019)     U07032        North Stockton PCN
    ## 4       93553 Deprivation score (IMD 2019)     U02671 Greater Middlesbrough PCN
    ## 5       93553 Deprivation score (IMD 2019)     U07842        East Cleveland PCN
    ## 6       93553 Deprivation score (IMD 2019)     U07032        North Stockton PCN
    ##    AreaCode                        AreaName AreaType     Sex      Age
    ## 1 E92000001                         England  England Persons All ages
    ## 2    A81001             The Densham Surgery      GPs Persons All ages
    ## 3    A81002      Queens Park Medical Centre      GPs Persons All ages
    ## 4    A81004           Acklam Medical Centre      GPs Persons All ages
    ## 5    A81005              Springwood Surgery      GPs Persons All ages
    ## 6    A81006 Tennant Street Medical Practice      GPs Persons All ages
    ##   CategoryType Category Timeperiod    Value LowerCI95.0limit UpperCI95.0limit
    ## 1         <NA>     <NA>       2010 21.69383               NA               NA
    ## 2         <NA>     <NA>       2010 25.07512               NA               NA
    ## 3         <NA>     <NA>       2010 27.70068               NA               NA
    ## 4         <NA>     <NA>       2010 33.05193               NA               NA
    ## 5         <NA>     <NA>       2010 14.55969               NA               NA
    ## 6         <NA>     <NA>       2010 29.14449               NA               NA
    ##   LowerCI99.8limit UpperCI99.8limit Count Denominator Valuenote RecentTrend
    ## 1               NA               NA    NA          NA      <NA>        <NA>
    ## 2               NA               NA    NA          NA      <NA>        <NA>
    ## 3               NA               NA    NA          NA      <NA>        <NA>
    ## 4               NA               NA    NA          NA      <NA>        <NA>
    ## 5               NA               NA    NA          NA      <NA>        <NA>
    ## 6               NA               NA    NA          NA      <NA>        <NA>
    ##   ComparedtoEnglandvalueorpercentiles
    ## 1                        Not compared
    ## 2                        Not compared
    ## 3                        Not compared
    ## 4                        Not compared
    ## 5                        Not compared
    ## 6                        Not compared
    ##   ComparedtoPCNs(v.26/04/24)valueorpercentiles TimeperiodSortable Newdata
    ## 1                                 Not compared           20100000    <NA>
    ## 2                                 Not compared           20100000    <NA>
    ## 3                                 Not compared           20100000    <NA>
    ## 4                                 Not compared           20100000    <NA>
    ## 5                                 Not compared           20100000    <NA>
    ## 6                                 Not compared           20100000    <NA>
    ##   Comparedtogoal Timeperiodrange
    ## 1           <NA>              1y
    ## 2           <NA>              1y
    ## 3           <NA>              1y
    ## 4           <NA>              1y
    ## 5           <NA>              1y
    ## 6           <NA>              1y

### Direct download

However, this data only provides values for practices that are still
active in 2019. To get the data for all practices, we obtained the CSV
directly from OHID via email, which is available
[here](https://github.com/camappel/HEEC/blob/main/data/IMD/IMD_raw.csv)
as `IMD_raw.csv`.

IMD values are only provided for 2010, 2015, and 2019. In order to have
a continuous time series, we interpolate the values between these years,
and extrapolate the values for 2020-2024. The resultant data is saved
[here](https://github.com/camappel/HEEC/blob/main/data/IMD/IMD_interpolated.csv)
as `IMD_interpolated.csv`.

``` r
IMD <- read.csv("IMD_raw.csv")

IMD <- IMD[, c("AreaCode", "Value", "Year")]
IMD %<>% rename(., Practice.Code = AreaCode)
IMD %<>% rename(., IMD = Value)

### interpolate IMD
result_df <- data.frame()

for (i in unique(IMD$Practice.Code)) {
  has_2010 <- any(IMD$Practice.Code == i & IMD$Year == 2010)
  has_2015 <- any(IMD$Practice.Code == i & IMD$Year == 2015)
  has_2019 <- any(IMD$Practice.Code == i & IMD$Year == 2019)

  # if data is only available for 2010, extrapolate to 2011-2024
  if (has_2010 & !has_2015) {
    for (year in 2011:2024) {
      new_row <- data.frame(
        Year = year,
        Practice.Code = i,
        IMD = IMD[IMD$Practice.Code == i & IMD$Year == 2010, ]$IMD
      )

      # Append the new row to the result data frame
      result_df <- rbind(result_df, new_row)
    }
  }

  # if data is available for 2010 and 2015, interpolate to 2011-2014
  if (has_2010 & has_2015) {
    for (year in 2011:2014) {
      y_new <- approx(
        IMD[IMD$Practice.Code == i, ]$Year,
        IMD[IMD$Practice.Code == i, ]$IMD,
        xout = year
      )
      # Create a new row for the current year and AreaCode
      new_row <- data.frame(
        Year = year,
        Practice.Code = i,
        IMD = y_new$y
      )

      # Append the new row to the result data frame
      result_df <- rbind(result_df, new_row)
    }
  }

  # if data is available for 2015 but not 2019, extrapolate to 2016-2024
  if (has_2015 & !has_2019) {
    for (year in 2016:2024) {
      new_row <- data.frame(
        Year = year,
        Practice.Code = i,
        IMD = IMD[IMD$Practice.Code == i & IMD$Year == 2015, ]$IMD
      )

      # Append the new row to the result data frame
      result_df <- rbind(result_df, new_row)
    }
  }

  # if data is available for 2015 and 2019, interpolate to 2016-2018
  if (has_2015 & has_2019) {
    for (year in 2016:2018) {
      y_new <- approx(
        IMD[IMD$Practice.Code == i, ]$Year,
        IMD[IMD$Practice.Code == i, ]$IMD,
        xout = year
      )
      # Create a new row for the current year and AreaCode
      new_row <- data.frame(
        Year = year,
        Practice.Code = i,
        IMD = y_new$y
      )

      # Append the new row to the result data frame
      result_df <- rbind(result_df, new_row)
    }
  }

  # if data is available for 2019, extrapolate to 2020-2024
  if (has_2019) {
    for (year in 2020:2024) {
      new_row <- data.frame(
        Year = year,
        Practice.Code = i,
        IMD = IMD[IMD$Practice.Code == i & IMD$Year == 2019, ]$IMD
      )

      # Append the new row to the result data frame
      result_df <- rbind(result_df, new_row)
    }
  }
}

IMD <- rbind(IMD, result_df)

IMD <- IMD[order(IMD$Practice.Code, IMD$Year), ]
write.csv(IMD, "IMD_interpolated.csv", row.names = FALSE)

head(IMD)
```

    ##       Practice.Code      IMD Year
    ## 1            A81001 25.07512 2010
    ## 23081        A81001 25.88816 2011
    ## 23082        A81001 26.70120 2012
    ## 23083        A81001 27.51424 2013
    ## 23084        A81001 28.32727 2014
    ## 2            A81001 29.14031 2015

``` r
IMD %>%
  group_by(Year) %>%
  summarise(
    mean_IMD = mean(IMD, na.rm = TRUE),
    sd_IMD = sd(IMD, na.rm = TRUE),
    min_IMD = min(IMD, na.rm = TRUE),
    max_IMD = max(IMD, na.rm = TRUE),
    n = n()
  )
```

    ## # A tibble: 15 × 6
    ##     Year mean_IMD sd_IMD min_IMD max_IMD     n
    ##    <int>    <dbl>  <dbl>   <dbl>   <dbl> <int>
    ##  1  2010     24.2   12.8    2.60    68.9  8222
    ##  2  2011     24.2   12.6    2.77    68.4  8222
    ##  3  2012     24.1   12.4    2.95    67.9  8222
    ##  4  2013     24.1   12.2    3.13    67.4  8222
    ##  5  2014     24.1   12.1    3.31    67.0  8222
    ##  6  2015     24.1   12.0    3.21    66.5  8438
    ##  7  2016     24.1   11.9    3.21    66.5  8438
    ##  8  2017     24.1   11.9    3.21    67.1  8438
    ##  9  2018     24.1   11.9    3.21    67.9  8438
    ## 10  2019     24.1   11.9    3.21    68.7  8461
    ## 11  2020     24.1   11.9    3.21    68.7  8461
    ## 12  2021     24.1   11.9    3.21    68.7  8461
    ## 13  2022     24.1   11.9    3.21    68.7  8461
    ## 14  2023     24.1   11.9    3.21    68.7  8461
    ## 15  2024     24.1   11.9    3.21    68.7  8461
