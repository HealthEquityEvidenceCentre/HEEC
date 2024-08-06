# Exploring the impact of dispensing practicing on equity in NHS payments to general practices

General practices serving the most deprived populations receive less
funding per weighted patient than those serving the least deprived. Here
we show that this inequality is driven by a higher concentration of
dispensing practices in more affluent areas.

### What are Dispensing Practices?

Dispensing practices are practices where “at the patient’s request
dispensing doctors are allowed to dispense the medicines they prescribe
for these patients” (DDA); they are not the same as practices with an
on-site pharmacy.

## Data Analysis

[NHS
Digital](https://digital.nhs.uk/data-and-information/publications/statistical/nhs-payments-to-general-practice)
provides public CSVs on NHS payments to individual providers of general
practice services in England from 2014-15 to 2022-23. We look at the
most recent data from 2022-23.

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

num_unknown_dispensing <- df[df$Dispensing.Practice == "Unknown", ] %>%
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

``` r
# Load necessary libraries
library(httr)
library(readr)

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
  df <- read_csv(temp_file)

  # Display the first few rows of the data
  print(head(df))
} else {
  cat("Failed to retrieve data. Status code:", status_code(response), "\n")
  cat("Response content:", content(response, "text"), "\n")
}
```

    ## Warning: One or more parsing issues, call `problems()` on your data frame for details,
    ## e.g.:
    ##   dat <- vroom(...)
    ##   problems(dat)

    ## Rows: 19047 Columns: 27
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr (12): Indicator Name, Parent Code, Parent Name, Area Code, Area Name, Ar...
    ## dbl  (4): Indicator ID, Time period, Value, Time period Sortable
    ## lgl (11): Category Type, Category, Lower CI 95.0 limit, Upper CI 95.0 limit,...
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

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

As well as the the fingertipsR package, which provides an R interface to
the Fingertips API.

``` r
# Enable repository from rOpenSci
options(repos = c(
  ropensci = "https://ropensci.r-universe.dev",
  CRAN = "https://cloud.r-project.org"
))

# Download and install fingertipsR in R
install.packages("fingertipsR")
```

    ## Installing package into '/Users/qmul/Library/R/arm64/4.3/library'
    ## (as 'lib' is unspecified)

    ## 
    ## The downloaded binary packages are in
    ##  /var/folders/xc/27k2c2sd09b0hlzxd45g2fcm0000gp/T//RtmpPHcuh5/downloaded_packages

``` r
# Alternatively, install the latest development version from GitHub
# install.packages("remotes")
remotes::install_github("rOpenSci/fingertipsR",
  build_vignettes = TRUE,
  dependencies = "suggests"
)
```

    ## Downloading GitHub repo rOpenSci/fingertipsR@HEAD

    ## ── R CMD build ─────────────────────────────────────────────────────────────────
    ##      checking for file ‘/private/var/folders/xc/27k2c2sd09b0hlzxd45g2fcm0000gp/T/RtmpPHcuh5/remotes611b52268ff4/ropensci-fingertipsR-309c4ae/DESCRIPTION’ ...  ✔  checking for file ‘/private/var/folders/xc/27k2c2sd09b0hlzxd45g2fcm0000gp/T/RtmpPHcuh5/remotes611b52268ff4/ropensci-fingertipsR-309c4ae/DESCRIPTION’
    ##   ─  preparing ‘fingertipsR’:
    ##      checking DESCRIPTION meta-information ...  ✔  checking DESCRIPTION meta-information
    ##   ─  installing the package to build vignettes
    ##      creating vignettes ...  ✔  creating vignettes (3.1s)
    ##   ─  checking for LF line-endings in source and make files and shell scripts
    ##   ─  checking for empty or unneeded directories
    ##      Omitted ‘LazyData’ from DESCRIPTION
    ##   ─  building ‘fingertipsR_1.0.12.tar.gz’
    ##      
    ## 

    ## Installing package into '/Users/qmul/Library/R/arm64/4.3/library'
    ## (as 'lib' is unspecified)

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
data <- fingertips_data(IndicatorID = indicator_id, AreaTypeID = 7) # AreaTypeID 7 is for CCGs

# Display the first few rows of the data
head(data)
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

Conclusion This analysis highlights the disparities in funding between
general practices serving different populations and the role of
dispensing practices in this inequality.

For more detailed exploration and visualization, further analysis and
refinement of the data is needed.

References NHS Digital: NHS Payments to General Practice Fingertips API
Documentation
