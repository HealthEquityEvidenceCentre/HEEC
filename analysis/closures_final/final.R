library(magrittr)
library(dplyr)

### Payments data
payments19 <- read.csv("data/payments/raw/19-20.csv") %>%
  mutate(Practice.Postcode = toupper(gsub("[[:space:]]+", "", Practice.Postcode)))
open_pay19 <- payments19$Practice.Code %>%
  unique()
open_pay19 %>% length()

payments22 <- read.csv("data/payments/raw/22-23.csv") %>%
  mutate(Practice.Postcode = toupper(gsub("[[:space:]]+", "", Practice.Postcode)))
open_pay22 <- payments22$Practice.Code %>%
  unique()
open_pay22 %>% length()

setdiff(open_pay19, open_pay22) %>% length()
setdiff(open_pay22, open_pay19) %>% length()

missing_payments <- setdiff(open_pay19, open_pay22)


### Workforce data
wf19 <- read.csv("data/workforce/raw/19_09.csv")
open_wf19 <- wf19$PRAC_CODE %>%
  unique()
open_wf19 %>% length()

wf19$TOTAL_PATIENTS %>% sum()

wf22 <- read.csv("data/workforce/raw/22_09.csv")
open_wf22 <- wf22$PRAC_CODE %>%
  unique()
open_wf22 %>% length()


### Combined
open19 <- c(open_pay19, open_wf19) %>%
  unique()
open19 %>% length()
open22 <- c(open_pay22, open_wf22) %>%
  unique()
open22 %>% length()

setdiff(open19, open22) %>% length()
setdiff(open22, open19) %>% length()

missing_combined <- setdiff(open19, open22)


### Postcode match
payments19 %>%
  filter(Practice.Code %in% missing_combined) %>%
  filter(Practice.Postcode %in% payments22$Practice.Postcode) %>%
  nrow()



### Extend
wf24 <- read.csv("data/workforce/raw/24_09.csv")
open_wf24 <- wf24$PRAC_CODE %>%
  unique()
open_wf24 %>% length()

wf24$TOTAL_PATIENTS %>% sum()

# missing
setdiff(open19, open_wf24) %>% length()
# new
setdiff(open_wf24, open19) %>% length()

missing24 <- setdiff(open19, open_wf24)



### Dataset
# Step 1: Start with the full set of 804 missing practices
outcomes <- data.frame(
  Practice.Code = missing24,
  stringsAsFactors = FALSE
)

# Step 2: Get codes with matching postcodes in 2022 payments (n = 99)
merged_codes <- payments19 %>%
  filter(Practice.Code %in% missing_combined) %>%
  filter(Practice.Postcode %in% payments22$Practice.Postcode) %>%
  pull(Practice.Code)

# Step 3: Assign "Merged" if postcode matched, else NA
outcomes <- outcomes %>%
  mutate(
    Outcome = ifelse(Practice.Code %in% merged_codes, "Merged", NA)
  )

write.csv(outcomes, "final.csv", row.names = FALSE)



### Latest reliable figures
final <- read.csv("analysis/closures_final/final.csv")
wf <- read.csv("data/workforce/workforce_year.csv")

# List size
wf %>%
  filter(Year == 2019) %>%
  filter(!is.na(TOTAL_PATIENTS), TOTAL_PATIENTS > 0) %>%
  summarise(
    patients = mean(TOTAL_PATIENTS),
    dist = sd(TOTAL_PATIENTS)
  )

wf %>%
  filter(Practice.Code %in% final[final$Outcome2 == "Closed", ]$Practice.Code) %>%
  select(Practice.Code, Year, TOTAL_PATIENTS) %>%
  group_by(Practice.Code) %>%
  mutate(
    annual_change = TOTAL_PATIENTS - lag(TOTAL_PATIENTS),
    percent_change = annual_change / lag(TOTAL_PATIENTS) * 100
  ) %>%
  filter(percent_change > -33) %>%
  slice_max(Year, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  summarise(
    patients = mean(TOTAL_PATIENTS),
    dist = sd(TOTAL_PATIENTS),
    displaced = sum(TOTAL_PATIENTS)
  )

wf %>%
  filter(Practice.Code %in% final[final$Outcome2 == "Merged", ]$Practice.Code) %>%
  select(Practice.Code, Year, TOTAL_PATIENTS) %>%
  group_by(Practice.Code) %>%
  mutate(
    annual_change = TOTAL_PATIENTS - lag(TOTAL_PATIENTS),
    percent_change = annual_change / lag(TOTAL_PATIENTS) * 100
  ) %>%
  filter(percent_change > -33) %>%
  slice_max(Year, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  summarise(
    patients = mean(TOTAL_PATIENTS),
    dist = sd(TOTAL_PATIENTS)
  )

wf %>%
  filter(!Practice.Code %in% missing24) %>%
  filter(Year == 2024) %>%
  filter(!is.na(TOTAL_PATIENTS), TOTAL_PATIENTS > 0) %>%
  summarise(
    patients = mean(TOTAL_PATIENTS),
    dist = sd(TOTAL_PATIENTS)
  )


# GPs
wf %>%
  filter(Year == 2019) %>%
  filter(!is.na(TOTAL_GP_EXTGL_FTE)) %>%
  filter(!is.na(TOTAL_PATIENTS), TOTAL_PATIENTS > 0) %>%
  mutate(
    GP_per_10000 = TOTAL_GP_EXTGL_FTE / (TOTAL_PATIENTS) * 10000
  ) %>%
  filter(TOTAL_PATIENTS >= 300) %>%  # <- new line to exclude small practices
  summarise(
    GPs = mean(TOTAL_GP_EXTGL_FTE),
    dist = sd(TOTAL_GP_EXTGL_FTE),
    GPs_per_10000 = mean(GP_per_10000),
    dist2 = sd(GP_per_10000)
  )

wf %>%
  filter(Practice.Code %in% final[final$Outcome2 == "Closed", ]$Practice.Code) %>%
  select(Practice.Code, Year, TOTAL_PATIENTS, TOTAL_GP_EXTGL_FTE) %>%
  filter(!is.na(TOTAL_GP_EXTGL_FTE)) %>%
  filter(!is.na(TOTAL_PATIENTS), TOTAL_PATIENTS > 0) %>%
  group_by(Practice.Code) %>%
  mutate(
    annual_change = TOTAL_GP_EXTGL_FTE - lag(TOTAL_GP_EXTGL_FTE),
    percent_change = annual_change / lag(TOTAL_GP_EXTGL_FTE) * 100,
    GP_per_10000 = TOTAL_GP_EXTGL_FTE / (TOTAL_PATIENTS) * 10000
  ) %>%
  filter(is.na(percent_change) | percent_change > -33) %>%
  slice_max(Year, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  filter(TOTAL_PATIENTS >= 300) %>%  # <- new line to exclude small practices
  summarise(
    GPs = mean(TOTAL_GP_EXTGL_FTE),
    dist = sd(TOTAL_GP_EXTGL_FTE),
    GPs_per_10000 = mean(GP_per_10000),
    dist2 = sd(GP_per_10000)
  )

wf %>%
  filter(Practice.Code %in% final[final$Outcome2 == "Merged", ]$Practice.Code) %>%
  select(Practice.Code, Year, TOTAL_PATIENTS, TOTAL_GP_EXTGL_FTE) %>%
  filter(!is.na(TOTAL_GP_EXTGL_FTE)) %>%
  filter(!is.na(TOTAL_PATIENTS), TOTAL_PATIENTS > 0) %>%
  group_by(Practice.Code) %>%
  mutate(
    annual_change = TOTAL_GP_EXTGL_FTE - lag(TOTAL_GP_EXTGL_FTE),
    percent_change = annual_change / lag(TOTAL_GP_EXTGL_FTE) * 100,
    GP_per_10000 = TOTAL_GP_EXTGL_FTE / (TOTAL_PATIENTS) * 10000
  ) %>%
  filter(is.na(percent_change) | percent_change > -33) %>%
  slice_max(Year, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  select(Practice.Code, Year, TOTAL_PATIENTS, TOTAL_GP_EXTGL_FTE, GP_per_10000) %>%
  filter(TOTAL_PATIENTS >= 300) %>%  # <- new line to exclude small practices
  summarise(
    GPs = mean(TOTAL_GP_EXTGL_FTE),
    dist = sd(TOTAL_GP_EXTGL_FTE),
    GPs_per_10000 = mean(GP_per_10000),
    dist2 = sd(GP_per_10000)
  )

wf %>%
  filter(!Practice.Code %in% missing24) %>%
  filter(Year == 2024) %>%
  filter(!is.na(TOTAL_GP_EXTGL_FTE)) %>%
  filter(!is.na(TOTAL_PATIENTS), TOTAL_PATIENTS > 0) %>%
  mutate(GP_per_10000 = TOTAL_GP_EXTGL_FTE / (TOTAL_PATIENTS) * 10000) %>%
  filter(TOTAL_PATIENTS >= 300) %>%  # <- new line to exclude small practices
  summarise(
    GPs = mean(TOTAL_GP_EXTGL_FTE),
    dist = sd(TOTAL_GP_EXTGL_FTE),
    GPs_per_10000 = mean(GP_per_10000),
    dist2 = sd(GP_per_10000)
  )


# Partners
partners <- wf %>%
  filter(!Practice.Code %in% missing24) %>%
  mutate(GP_Partners = TOTAL_GP_SEN_PTNR_HC + TOTAL_GP_PTNR_PROV_HC) %>%
  group_by(Practice.Code) %>%
  mutate(
    prev_partners = lag(GP_Partners),
    annual_change = GP_Partners - prev_partners,
    percent_change = annual_change / prev_partners * 100
  ) %>%
  filter(is.na(percent_change) | percent_change > -33) %>%
  slice_max(Year, n = 1, with_ties = FALSE) %>%
  ungroup()

partners %>% nrow()
partners[partners$GP_Partners == 1, ] %>% nrow()
# 22 missing practices are not in workforce data (identified in payments data)
