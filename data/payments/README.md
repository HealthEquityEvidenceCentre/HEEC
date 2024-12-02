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
    gsub(" Integrated Care Board$", "", .) %>%
    gsub(" ICB", "", .))
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
    ## 1589 1589 1588 1588 1588 
    ## [1] "Year: 2016"
    ## 
    ##    1    2    3    4    5 
    ## 1565 1565 1565 1565 1564 
    ## [1] "Year: 2017"
    ## 
    ##    1    2    3    4    5 
    ## 1547 1546 1546 1546 1546 
    ## [1] "Year: 2018"
    ## 
    ##    1    2    3    4    5 
    ## 1502 1502 1502 1502 1501 
    ## [1] "Year: 2019"
    ## 
    ##    1    2    3    4    5 
    ## 1453 1453 1453 1452 1452 
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
    ## 1347 1347 1347 1347 1346 
    ## [1] "Year: 2023"
    ## 
    ##    1    2    3    4    5 
    ## 1329 1329 1328 1328 1328

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
library(ggplot2)

nhs_payments <- read.csv("payments.csv")
nhs_payments$IMD_quintile <- as.factor(nhs_payments$IMD_quintile)

nhs_payments %>% colnames()
```

    ##   [1] "Practice.Code"                                                                                               
    ##   [2] "Year"                                                                                                        
    ##   [3] "NHS.England..Region..code"                                                                                   
    ##   [4] "NHS.England..Region..Name"                                                                                   
    ##   [5] "CCG.Code"                                                                                                    
    ##   [6] "CCG.NAME"                                                                                                    
    ##   [7] "Practice.Name"                                                                                               
    ##   [8] "Practice.Address"                                                                                            
    ##   [9] "Practice.Postcode"                                                                                           
    ##  [10] "Practice.Open.Date"                                                                                          
    ##  [11] "Practice.Close.Date"                                                                                         
    ##  [12] "Contract.Type"                                                                                               
    ##  [13] "Dispensing.Practice"                                                                                         
    ##  [14] "Quarter.used.for.patient.data"                                                                               
    ##  [15] "Number.of.Registered.Patients..Last.Known.Figure."                                                           
    ##  [16] "Number.of.Weighted.Patients..Last.Known.Figure."                                                             
    ##  [17] "Average.payments.per.registered.patient"                                                                     
    ##  [18] "Average.payments.per.weighted.patient"                                                                       
    ##  [19] "Global.Sum"                                                                                                  
    ##  [20] "MPIG.Correction.factor"                                                                                      
    ##  [21] "Premises.Payments"                                                                                           
    ##  [22] "Seniority"                                                                                                   
    ##  [23] "Doctors.Retainer.Scheme.Payments"                                                                            
    ##  [24] "Total.Locum.Allowances"                                                                                      
    ##  [25] "Prolonged.Study.Leave"                                                                                       
    ##  [26] "Appraisal.Costs"                                                                                             
    ##  [27] "PCO.Admin.Other"                                                                                             
    ##  [28] "Total.QOF.Payments"                                                                                          
    ##  [29] "Alcohol"                                                                                                     
    ##  [30] "Childhood.Vaccination.and.Immunisation.Scheme"                                                               
    ##  [31] "GP.Extended.Hours.Access"                                                                                    
    ##  [32] "Facilitating.Timely.Diagnosis.and.Support.for.People.with.Dementia"                                          
    ##  [33] "Improving.Patient.Online.Access"                                                                             
    ##  [34] "Influenza.and.Pneumococcal.Immunisations"                                                                    
    ##  [35] "Learning.Disabilities"                                                                                       
    ##  [36] "Minor.Surgery"                                                                                               
    ##  [37] "Patient.Participation"                                                                                       
    ##  [38] "Remote.Care.Monitoring"                                                                                      
    ##  [39] "Risk.Profiling.and.Case.Management"                                                                          
    ##  [40] "Rotavirus.and.Shingles.Immunisation"                                                                         
    ##  [41] "Services.for.Violent.Patients"                                                                               
    ##  [42] "Unplanned.Admissions"                                                                                        
    ##  [43] "Total.National.Enhanced.Services"                                                                            
    ##  [44] "Total.Local.Enhanced.Services"                                                                               
    ##  [45] "Information.Management.and.Technology"                                                                       
    ##  [46] "Balance.of.PMS.Expenditure"                                                                                  
    ##  [47] "Non.DES.Item.Pneumococcal.Vaccine..Childhood.Immunisation.Main.Programme"                                    
    ##  [48] "Prescribing.Fee.Payments"                                                                                    
    ##  [49] "Dispensing.Fee.Payments"                                                                                     
    ##  [50] "Reimbursement.of.Drugs"                                                                                      
    ##  [51] "Other.Payments"                                                                                              
    ##  [52] "Total.NHS.Payments.to.General.Practice"                                                                      
    ##  [53] "Deductions.for.Pensions..Levies.and.Prescription.Charge.Income"                                              
    ##  [54] "Total.NHS.Payments.to.General.Practice.Minus.Deductions"                                                     
    ##  [55] "Out.Of.Area.in.Hours.Urgent.Care"                                                                            
    ##  [56] "Meningitis"                                                                                                  
    ##  [57] "LocalEnhancedServices_NHAIS_"                                                                                
    ##  [58] "LocalEnhancedServices_ISFE_"                                                                                 
    ##  [59] "General.Practice.Forward.View"                                                                               
    ##  [60] "Pertussis"                                                                                                   
    ##  [61] "Total.Local.Incentive.Schemes"                                                                               
    ##  [62] "PCN.Name"                                                                                                    
    ##  [63] "PCN.Participation"                                                                                           
    ##  [64] "PCN.Leadership"                                                                                              
    ##  [65] "PCN.Support"                                                                                                 
    ##  [66] "PCN.Extended.Hours.Access"                                                                                   
    ##  [67] "PCN.Workforce"                                                                                               
    ##  [68] "PCN.Investment.and.impact.Fund"                                                                              
    ##  [69] "PCN.Care.Home.Premium"                                                                                       
    ##  [70] "Covid.Immunisation"                                                                                          
    ##  [71] "Covid.Support.and.Expansion"                                                                                 
    ##  [72] "Average.payments.per.registered.patient.including.PCN.Workforce..Leadership.and.Support"                     
    ##  [73] "Average.payments.per.weighted.patient.including.PCN.Workforce..Leadership.and.Support"                       
    ##  [74] "Average.payments.per.registered.patient.including.covid.vaccination.and.covid.support.and.expansion.payments"
    ##  [75] "Average.payments.per.weighted.patient.including.covid.vaccination.and.covid.support.and.expansion.payments"  
    ##  [76] "Total.COVID.Payments"                                                                                        
    ##  [77] "Total.PCN.Payments"                                                                                          
    ##  [78] "Medical.Assessment.Reviews"                                                                                  
    ##  [79] "Weight.Management.Service"                                                                                   
    ##  [80] "General.Practice.Transformation"                                                                             
    ##  [81] "winter.Access.Fund"                                                                                          
    ##  [82] "Long.Covid"                                                                                                  
    ##  [83] "Average.payments.per.registered.patient.including.covid.vaccination..covid.support.and.long.covid.payments"  
    ##  [84] "Average.payments.per.weighted.patient.including.covid.vaccination..covid.support.and.long.covid.payments"    
    ##  [85] "Sub.ICB.NAME"                                                                                                
    ##  [86] "Local.Incentive.Schemes"                                                                                     
    ##  [87] "Appraisal...Appraiser.Costs.in.Respect.of.Locums"                                                            
    ##  [88] "PCN.Enhanced.Access"                                                                                         
    ##  [89] "ICB.NAME"                                                                                                    
    ##  [90] "Practice.Type"                                                                                               
    ##  [91] "Practice.Rurality"                                                                                           
    ##  [92] "Atypical.Characteristics"                                                                                    
    ##  [93] "PCN.Code"                                                                                                    
    ##  [94] "Total.Global.Sum"                                                                                            
    ##  [95] "Total.IT.Premises"                                                                                           
    ##  [96] "Total.PCO"                                                                                                   
    ##  [97] "Total.Contracted.Services"                                                                                   
    ##  [98] "Total.Prescribing"                                                                                           
    ##  [99] "Total.COVID"                                                                                                 
    ## [100] "IMD"                                                                                                         
    ## [101] "IMD_quintile"

``` r
nhs_payments$Total.NHS.Payments.Minus.COVID.PCN <-
  nhs_payments$Total.NHS.Payments.to.General.Practice -
  ifelse(is.na(nhs_payments$Total.COVID.Payments), 0, nhs_payments$Total.COVID.Payments) -
  ifelse(is.na(nhs_payments$Total.PCN.Payments), 0, nhs_payments$Total.PCN.Payments)

### Create graph of average payments per patient by IMD decile
agg <- nhs_payments %>%
  # agg <- nhs_payments[nhs_payments$ICB.NAME == ICB, ] %>% # for specific ICB
  group_by(Year, IMD_quintile) %>%
  summarise(
    Number.of.Registered.Patients..Last.Known.Figure. = sum(Number.of.Registered.Patients..Last.Known.Figure., na.rm = TRUE),
    Number.of.Weighted.Patients..Last.Known.Figure. = sum(Number.of.Weighted.Patients..Last.Known.Figure., na.rm = TRUE),
    Total.NHS.Payments.to.General.Practice = sum(Total.NHS.Payments.Minus.COVID.PCN, na.rm = TRUE),
  ) %>%
  mutate(
    Average.payments.per.registered.patient = Total.NHS.Payments.to.General.Practice / Number.of.Registered.Patients..Last.Known.Figure.,
    Average.payments.per.weighted.patient = Total.NHS.Payments.to.General.Practice / Number.of.Weighted.Patients..Last.Known.Figure.
  )

colors <- c("#EF7A34", "#00A865", "#007AA8", "#531A5C", "#A80026")

agg[!is.na(agg$IMD_quintile), ] %>%
  ggplot(aes(x = Year, y = Average.payments.per.weighted.patient, group = IMD_quintile, colour = IMD_quintile)) +
  geom_line(size = 1.5) + # Adjust the size as needed
  geom_point(size = 3) +
  labs(x = "", y = "Average payment (£)", title = "Total NHS payments per weighted patient by IMD quintile (England)") +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    legend.justification = "center", # Adjust as needed
    axis.text.x = element_text(size = 10),
    axis.title.x = element_text(size = 12),
    axis.line.x = element_line(size = 1), # Adjust x-axis line thickness
    panel.grid.minor.x = element_blank(), # Remove minor vertical grid lines
    panel.grid.minor.y = element_blank(), # Remove minor horizontal grid lines
  ) +
  scale_color_manual(values = colors, labels = c("Q1 (least deprived)", "Q2", "Q3", "Q4", "Q5 (most deprived)")) +
  labs(color = "IMD quintile") +
  scale_x_continuous(breaks = unique(agg$Year)) # Show every year on the x-axis
```

![](README_files/figure-markdown_github/sanity%20check-1.png)
