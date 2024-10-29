# Primary Care Network
Cameron Appel
2024-08-20

# Primary Care Network

``` r
# List all relevant files from the raw directory
files <- list.files("raw", pattern = "Primary Care Networks.*Individual Level.csv", full.names = TRUE)

# Load and combine all files
pcn <- lapply(files, function(file) {
  df <- read.csv(file)
  df <- df %>% 
    select(CENSUS_YEAR, CENSUS_MONTH,
           PCN_CODE, PCN_NAME, 
           ICB_CODE, ICB_NAME, STAFF_GROUP, 
           FTE)
  return(df)
}) %>% 
  bind_rows()

pcn %<>% group_by(CENSUS_YEAR, PCN_CODE, ICB_NAME, STAFF_GROUP) %>% summarise(FTE = sum(FTE, na.rm = TRUE)) %>% rename(Year = CENSUS_YEAR)
```

    `summarise()` has grouped output by 'CENSUS_YEAR', 'PCN_CODE', 'ICB_NAME'. You
    can override using the `.groups` argument.

``` r
pcn %>% head()
```

    # A tibble: 6 × 5
    # Groups:   Year, PCN_CODE, ICB_NAME [4]
       Year PCN_CODE ICB_NAME                                      STAFF_GROUP   FTE
      <int> <chr>    <chr>                                         <chr>       <dbl>
    1  2020 U00070   NHS South West London ICB                     Other Dire… 6    
    2  2020 U00254   NHS South East London ICB                     Directors   0.800
    3  2020 U00254   NHS South East London ICB                     Other Dire… 6    
    4  2020 U00351   NHS Birmingham and Solihull ICB               Other Admi… 2.24 
    5  2020 U00351   NHS Birmingham and Solihull ICB               Other Dire… 2.64 
    6  2020 U00402   NHS Bristol, North Somerset and South Glouce… Other Admi… 0.8  

# Get IMD from fingertipsR

``` r
# Load necessary library
library(fingertipsR)

# Get the profile ID for the Public Health Outcomes Framework
profiles_data <- profiles()
phof_profile <- profiles_data[profiles_data$ProfileName == "Public Health Outcomes Framework", ]
profile_id <- phof_profile$ProfileID[1]

# Get indicators for the Public Health Outcomes Framework profile
indicators_data <- indicators(ProfileID = profile_id)

# Find the IndicatorID for "Deprivation score (IMD 2019)"
indicator_id <- indicators_data$IndicatorID[indicators_data$IndicatorName == "Deprivation score (IMD 2019)"]

# Get the data for the "Deprivation score (IMD 2019)" indicator
IMD <- fingertips_data(IndicatorID = indicator_id, AreaTypeID = 7) %>%
  select(c("ParentCode","Value")) %>% group_by(ParentCode) %>% summarise(IMD = mean(Value))

IMD %<>% rename(PCN_CODE = ParentCode)
# Display the first few rows of the data
head(IMD)
```

    # A tibble: 6 × 2
      PCN_CODE   IMD
      <chr>    <dbl>
    1 U00000   22.0 
    2 U00070    8.41
    3 U00147   27.6 
    4 U00203   33.2 
    5 U00254   26.2 
    6 U00351   18.2 

``` r
pcn <- merge(pcn, IMD, by = "PCN_CODE") %>%
  group_by(Year) %>%
  mutate(IMD_quintile = ntile(IMD, 5))

# View the updated data frame
head(pcn)
```

    # A tibble: 6 × 7
    # Groups:   Year [5]
      PCN_CODE  Year ICB_NAME                  STAFF_GROUP    FTE   IMD IMD_quintile
      <chr>    <int> <chr>                     <chr>        <dbl> <dbl>        <int>
    1 U00070    2022 NHS South West London ICB Other Direc…  16.7  8.41            1
    2 U00070    2023 NHS South West London ICB Other Direc…  80.0  8.41            1
    3 U00070    2024 NHS South West London ICB Other Direc…  22.0  8.41            1
    4 U00070    2022 NHS South West London ICB Other Admin…   4    8.41            1
    5 U00070    2021 NHS South West London ICB Other Direc…  13.7  8.41            1
    6 U00070    2020 NHS South West London ICB Other Direc…   6    8.41            1

``` r
write.csv(pcn, "pcn_workforce.csv")
```
