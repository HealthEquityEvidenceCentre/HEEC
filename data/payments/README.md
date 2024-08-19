# NHS Payments to General Practice

NHS Digital provides annual data on payments made to general practices
in England, starting from 2014/15.

Annual practice-level .csv files are available \[here\]; some
preprocessing has been done to clean the data and ensure consistency in
variable names across years.

Execute the following code to merge the data to create a single
time-series dataset:

``` r
library(magrittr)
library(dplyr)

nhs_payments <- data.frame()

for (file in list.files("raw")[1:6]) {
  df <- read.csv(paste0("raw/", file))

  # assign year
  df$Year <- file %>%
    substr(1, nchar(.) - 4) %>%
    substr(4, nchar(.)) %>%
    paste0("20", .) %>%
    as.numeric()

  df$Average.payments.per.registered.patient %<>% as.numeric()
  df$Average.payments.per.weighted.patient %<>% as.numeric()

  nhs_payments <- bind_rows(nhs_payments, df)
}

# Add 2021 with COVID + PCN (incl deductions)
# Includes Total.NHS.Payments.to.General.Practice, Total.NHS.Payments.including.covid.vaccination.and.covid.support.and.expansion.payments, and Total.NHS.Payments.including.PCN.Workforce..Leadership.and.Support
# Total.NHS.Payments should include PCN and COVID payments
payments2021 <- read.csv("raw/20-21.csv")
payments2021$Year <- 2021

payments2021$Total.COVID.Payments <- payments2021$Total.NHS.Payments.including.covid.vaccination.and.covid.support.and.expansion.payments - payments2021$Total.NHS.Payments.to.General.Practice
payments2021$Total.PCN.Payments <- payments2021$Total.NHS.Payments.including.PCN.Workforce..Leadership.and.Support - payments2021$Total.NHS.Payments.to.General.Practice
payments2021$Total.NHS.Payments.to.General.Practice <- payments2021$Total.NHS.Payments.to.General.Practice + payments2021$Total.COVID.Payments + payments2021$Total.PCN.Payments
payments2021$Total.NHS.Payments.to.General.Practice.Minus.Deductions <- payments2021$Total.NHS.Payments.to.General.Practice - payments2021$Deductions.for.Pensions..Levies.and.Prescription.Charge.Income

# Add 2022 with COVID + PCN
# Includes Total.NHS.Payments.to.General.Practice.including.Covid.and.PCN.payments as well as Total.NHS.Payments.to.General.Practice
payments2022 <- read.csv("raw/21-22.csv")
payments2022$Year <- 2022

payments2022$Total.COVID.Payments <- payments2022$Total.NHS.Payments.to.General.Practice.including.covid.vaccination..covid.support.and.long.covid.payments - payments2022$Total.NHS.Payments.to.General.Practice
payments2022$Total.PCN.Payments <- payments2022$Total.NHS.Payments.including.PCN.Workforce..Leadership.and.Support - payments2022$Total.NHS.Payments.to.General.Practice
payments2022$Total.NHS.Payments.to.General.Practice <- payments2022$Total.NHS.Payments.to.General.Practice.including.Covid.and.PCN.payments
payments2022$Total.NHS.Payments.to.General.Practice.Minus.Deductions <- payments2022$Total.NHS.Payments.to.General.Practice - payments2022$Deductions.for.Pensions..Levies.and.Prescription.Charge.Income

# Add 2023
# Includes Total.NHS.Payments.to.General.Practice.including.Covid.and.PCN.payments as well as Total.NHS.Payments.to.General.Practice
payments2023 <- read.csv("raw/22-23.csv")
payments2023$Year <- 2023

payments2023$Total.COVID.Payments <- payments2023$Total.NHS.Payments.to.General.Practice.including.covid.vaccination..covid.support.and.long.covid.payments - payments2023$Total.NHS.Payments.to.General.Practice
payments2023$Total.PCN.Payments <- payments2023$Total.NHS.Payments.including.PCN.Workforce..Leadership.and.Support - payments2023$Total.NHS.Payments.to.General.Practice
payments2023$Total.NHS.Payments.to.General.Practice <- payments2023$Total.NHS.Payments.to.General.Practice.including.Covid.and.PCN.payments
payments2023$Total.NHS.Payments.to.General.Practice.Minus.Deductions <- payments2023$Total.NHS.Payments.to.General.Practice - payments2023$Deductions.for.Pensions..Levies.and.Prescription.Charge.Income

nhs_payments <- bind_rows(nhs_payments, payments2021, payments2022, payments2023)

nhs_payments %>%
  group_by(Year) %>%
  summarise(
    total_payments = sum(Total.NHS.Payments.to.General.Practice),
    total_patients = sum(Number.of.Registered.Patients..Last.Known.Figure.),
    n = n()
  )
```

    ## # A tibble: 9 × 4
    ##    Year total_payments total_patients     n
    ##   <dbl>          <dbl>          <dbl> <int>
    ## 1  2015    7990324226       56633982   7959
    ## 2  2016    8182561838.      57371518   7841
    ## 3  2017    8883780328.      58688866   7763
    ## 4  2018    9050596202.      59527981   7543
    ## 5  2019    9261391490.      59824330.  7279
    ## 6  2020    9377079859.      60316398.  7001
    ## 7  2021   10309607071.      60671585.  6808
    ## 8  2022   11510115126.      61570004.  6758
    ## 9  2023   11011986630.      62134645.  6669

``` r
CCG_ICB <- read.csv("../CCG_ICB_code.csv")

# match CCG.Code in df with CCG.Code in ccg_icb and return ICB.Name
nhs_payments$ICB.NAME <- CCG_ICB[match(nhs_payments$CCG.Code, CCG_ICB$CCG.Code), ]$ICB.NAME

# In nhs_payments$ICB.NAME, remove "NHS " from the beginning of each string and " Integrated Care Board" from the end of each string
nhs_payments <- nhs_payments %>%
  mutate(ICB.NAME = ICB.NAME %>%
    gsub("^NHS ", "", .) %>%
    gsub(" Integrated Care Board$", "", .))
```

``` r
standardise_columns <- function(nhs_payments) {
  # Helper function to merge columns and keep the first non-NA value
  merge_columns <- function(df, cols, new_col) {
    df[[new_col]] <- df %>%
      select(all_of(cols)) %>%
      apply(1, function(x) {
        x <- x[!is.na(x)]
        if (length(x) == 0) {
          return(NA)
        }
        return(x[1])
      }) %>%
      as.character()

    # Drop the original columns but keep the new column
    df <- df %>% select(-all_of(cols), all_of(new_col))
    return(df)
  }

  # Merge "PracticeType", "Practice.Type", "Practice.type" into one column called Practice.Type
  nhs_payments <- merge_columns(nhs_payments, c("PracticeType", "Practice.Type", "Practice.type"), "Practice.Type")

  # Merge "PracticeRurality", "Practice.Rurality" into one column called Practice.Rurality
  nhs_payments <- merge_columns(nhs_payments, c("PracticeRurality", "Practice.Rurality"), "Practice.Rurality")

  # Merge "AtypicalCharacteristics", "Atypical.Characteristics", "Atypical.characteristics" into one column called Atypical.Characteristics
  nhs_payments <- merge_columns(nhs_payments, c("AtypicalCharacteristics", "Atypical.Characteristics", "Atypical.characteristics"), "Atypical.Characteristics")

  # Merge "Code" into "PCN.Code"
  nhs_payments <- merge_columns(nhs_payments, c("PCN.Code", "Code"), "PCN.Code")

  return(nhs_payments)
}

nhs_payments <- standardise_columns(nhs_payments)

# drop Total.NHS.Payments.including.covid.vaccination.and.covid.support.and.expansion.payments, Total.NHS.Payments.including.PCN.Workforce..Leadership.and.Support, Total.NHS.Payments.to.General.Practice.including.Covid.and.PCN.payments, Total.NHS.Payments.to.General.Practice.including.Covid.and.PCN.payments.minus.deductions, Total.NHS.Payments.to.General.Practice.including.covid.vaccination..covid.support.and.long.covid.payments
nhs_payments <- nhs_payments %>%
  select(-Total.NHS.Payments.including.covid.vaccination.and.covid.support.and.expansion.payments, -Total.NHS.Payments.including.PCN.Workforce..Leadership.and.Support, -Total.NHS.Payments.to.General.Practice.including.Covid.and.PCN.payments, -Total.NHS.Payments.to.General.Practice.including.Covid.and.PCN.payments.minus.deductions, -Total.NHS.Payments.to.General.Practice.including.covid.vaccination..covid.support.and.long.covid.payments)
```

``` r
# The following lists should contain all columns in NHS payments
practice_information <- c(
  "NHS.England..Region..code", "NHS.England..Region..Name",
  "CCG.Code", "CCG.NAME",
  "Practice.Code", "Practice.Name",
  "Practice.Address", "Practice.Postcode",
  "Practice.Open.Date", "Practice.Close.Date",
  "Contract.Type", "Dispensing.Practice",
  "Quarter.used.for.patient.data",
  # PracticeType = 2017, Practice.Type = 2018:2021, Practice.type = 2022:2023, None = 2015:2016
  # "PracticeType", "Practice.type",
  "Practice.Type",
  # PracticeRurality = 2017, Practice.Rurality = 2018:2023, None = 2015:2016
  #  "PracticeRurality",
  "Practice.Rurality",
  # AtypicalCharacteristics = 2017, Atypical.Characteristics = 2018:2020, Atypical.characteristics: 2021:2023, None = 2015:2016
  # "AtypicalCharacteristics", "Atypical.characteristics",
  "Atypical.Characteristics",
  # Code = 2023
  # "Code",
  "PCN.Code",
  "PCN.Name",
  "ICB.NAME", "Sub.ICB.NAME",
  "Year"
)

population_information <- c(
  "Number.of.Registered.Patients..Last.Known.Figure.",
  "Number.of.Weighted.Patients..Last.Known.Figure.",
  "Average.payments.per.registered.patient",
  "Average.payments.per.weighted.patient",
  "Average.payments.per.registered.patient.including.PCN.Workforce..Leadership.and.Support",
  "Average.payments.per.weighted.patient.including.PCN.Workforce..Leadership.and.Support",
  "Average.payments.per.registered.patient.including.covid.vaccination.and.covid.support.and.expansion.payments",
  "Average.payments.per.weighted.patient.including.covid.vaccination.and.covid.support.and.expansion.payments",
  "Average.payments.per.registered.patient.including.covid.vaccination..covid.support.and.long.covid.payments",
  "Average.payments.per.weighted.patient.including.covid.vaccination..covid.support.and.long.covid.payments"
)

totals <- c(
  "Total.NHS.Payments.to.General.Practice",
  # "Total.NHS.Payments.to.General.Practice.including.Covid.and.PCN.payments" is now Total.NHS.Payments.to.General.Practice
  # Total.NHS.Payments.including.covid.vaccination.and.covid.support.and.expansion.payments = 2021, Total.NHS.Payments.to.General.Practice.including.covid.vaccination..covid.support.and.long.covid.payments = 2022:2023
  # "Total.NHS.Payments.to.General.Practice.including.covid.vaccination..covid.support.and.long.covid.payments",
  # "Total.NHS.Payments.including.covid.vaccination.and.covid.support.and.expansion.payments",
  # "Total.NHS.Payments.including.PCN.Workforce..Leadership.and.Support",
  "Deductions.for.Pensions..Levies.and.Prescription.Charge.Income",
  "Total.NHS.Payments.to.General.Practice.Minus.Deductions",
  # "Total.NHS.Payments.to.General.Practice.including.Covid.and.PCN.payments.minus.deductions",
  "Total.COVID.Payments", "Total.PCN.Payments"
)

globalSum <- c(
  # 2015
  "Global.Sum", "MPIG.Correction.factor", "Balance.of.PMS.Expenditure"
)

ITPremises <- c(
  # 2015
  "Premises.Payments", "Information.Management.and.Technology"
)

# Seniority payments were made to principal GPs in recognition of their years of NHS reckonable service.
# The scheme closed to new applicants on the 1 April, 2014 and the last payments were made at the end of March 2020.
PCO <- c(
  # 2015
  "Seniority", "Doctors.Retainer.Scheme.Payments", "Total.Locum.Allowances",
  "Appraisal.Costs",
  "PCO.Admin.Other", "Other.Payments",
  "General.Practice.Forward.View",
  "General.Practice.Transformation", "winter.Access.Fund",
  "Prolonged.Study.Leave",
  "Appraisal...Appraiser.Costs.in.Respect.of.Locums",
  "PCN.Participation", "PCN.Leadership", "PCN.Support",
  "PCN.Extended.Hours.Access", "PCN.Workforce",
  "PCN.Investment.and.impact.Fund",
  "PCN.Care.Home.Premium", "PCN.Enhanced.Access"
)

QOF <- c("Total.QOF.Payments")

contractedServices <- c(
  "Alcohol", "Childhood.Vaccination.and.Immunisation.Scheme",
  "GP.Extended.Hours.Access",
  "Facilitating.Timely.Diagnosis.and.Support.for.People.with.Dementia",
  "Improving.Patient.Online.Access",
  "Influenza.and.Pneumococcal.Immunisations", "Learning.Disabilities",
  "Minor.Surgery", "Patient.Participation", "Remote.Care.Monitoring",
  "Risk.Profiling.and.Case.Management", "Rotavirus.and.Shingles.Immunisation",
  "Services.for.Violent.Patients", "Unplanned.Admissions",
  "Total.National.Enhanced.Services", "Total.Local.Enhanced.Services",
  "Non.DES.Item.Pneumococcal.Vaccine..Childhood.Immunisation.Main.Programme",
  # 2016
  "Out.Of.Area.in.Hours.Urgent.Care", "Meningitis",
  # These 2 total to make Total.Local.Enhances.Services in 2016:
  #    "LocalEnhancedServices_NHAIS_", "LocalEnhancedServices_ISFE_",
  "Pertussis",
  "Total.Local.Incentive.Schemes", "Local.Incentive.Schemes",
  "Weight.Management.Service",
  "Medical.Assessment.Reviews"
  # "Covid.Immunisation", "Covid.Support.and.Expansion",
  # "Long.Covid"
)

COVID <- c(
  "Covid.Immunisation", "Covid.Support.and.Expansion",
  "Long.Covid"
)

Prescribing <- c(
  "Prescribing.Fee.Payments", "Dispensing.Fee.Payments",
  "Reimbursement.of.Drugs"
)

setdiff(names(nhs_payments), c(practice_information, population_information, totals, globalSum, ITPremises, PCO, QOF, contractedServices, Prescribing, COVID))
```

    ## [1] "LocalEnhancedServices_NHAIS_" "LocalEnhancedServices_ISFE_"

``` r
setdiff(c(practice_information, population_information, totals, globalSum, ITPremises, PCO, QOF, contractedServices, Prescribing), names(nhs_payments))
```

    ## character(0)

``` r
nhs_payments$Total.Global.Sum <- rowSums(nhs_payments[, globalSum], na.rm = TRUE)
nhs_payments$Total.IT.Premises <- rowSums(nhs_payments[, ITPremises], na.rm = TRUE)
nhs_payments$Total.PCO <- rowSums(nhs_payments[, PCO], na.rm = TRUE)
nhs_payments$Total.Contracted.Services <- rowSums(nhs_payments[, contractedServices], na.rm = TRUE)
nhs_payments$Total.Prescribing <- rowSums(nhs_payments[, Prescribing], na.rm = TRUE)
nhs_payments$Total.COVID <- rowSums(nhs_payments[, COVID], na.rm = TRUE)
```

``` r
source("../data_processing.R")

# Call the function to merge and assign national-level quintiles
nhs_payments <- merge_and_assign_quintiles(
  data = nhs_payments,
  start_year = 2015,
  end_year = 2023
)
```

    ## [1] "Year: 2015"
    ## 
    ##    1    2    3    4    5 
    ## 1589 1588 1588 1588 1589 
    ## [1] "Year: 2016"
    ## 
    ##    1    2    3    4    5 
    ## 1565 1565 1564 1565 1565 
    ## [1] "Year: 2017"
    ## 
    ##    1    2    3    4    5 
    ## 1547 1546 1546 1546 1546 
    ## [1] "Year: 2018"
    ## 
    ##    1    2    3    4    5 
    ## 1502 1502 1501 1502 1502 
    ## [1] "Year: 2019"
    ## 
    ##    1    2    3    4    5 
    ## 1453 1452 1453 1452 1453 
    ## [1] "Year: 2020"
    ## 
    ##    1    2    3    4    5 
    ## 1397 1396 1396 1396 1396 
    ## [1] "Year: 2021"
    ## 
    ##    1    2    3    4    5 
    ## 1357 1357 1357 1357 1357 
    ## [1] "Year: 2022"
    ## 
    ##    1    2    3    4    5 
    ## 1347 1347 1346 1347 1347 
    ## [1] "Year: 2023"
    ## 
    ##    1    2    3    4    5 
    ## 1329 1328 1328 1328 1329

``` r
# Count missing IMD values per year
nhs_payments %>%
  group_by(Year) %>%
  summarise(
    missing_imd = sum(is.na(IMD))
  )
```

    ## # A tibble: 9 × 2
    ##    Year missing_imd
    ##   <dbl>       <int>
    ## 1  2015          17
    ## 2  2016          17
    ## 3  2017          32
    ## 4  2018          34
    ## 5  2019          16
    ## 6  2020          20
    ## 7  2021          23
    ## 8  2022          24
    ## 9  2023          27

``` r
write.csv(nhs_payments, "payments.csv", row.names = FALSE)
```

``` r
# ### 2014/15
# # Calculate the row sums for columns 18 to 50
# row_sums <- nhs_payments[nhs_payments$Year == 2015, 18:50] %>% rowSums()
# total <- nhs_payments[nhs_payments$Year == 2015, ]$Total.NHS.Payments.to.General.Practice

# # Compare the two sets of values, allowing a tolerance of 5
# comparison <- abs(row_sums - total) <= 5
# all(comparison)

# ### 2015/16
# df <- read.csv("nhs_payments/raw/15-16.csv")
# df %>% colnames()
```
