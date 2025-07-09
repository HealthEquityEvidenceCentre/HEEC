library(dplyr)
library(readr)

### 1. Load 2019 and 2024 workforce snapshots
wf20 <- read.csv("../../data/workforce/raw/19_09.csv")
wf24 <- read.csv("../../data/workforce/raw/24_09.csv")

cat("2019 practices:", length(unique(wf20$PRAC_CODE)), "\n")
cat("2024 practices:", length(unique(wf24$PRAC_CODE)), "\n")

### 2. Load closure/merger outcomes
closed_merged <- read.csv("closed_merged.csv")

# Counts by outcome
closed_merged %>%
  count(Outcome)

# Practices with unspecified outcome
closed_merged %>%
  filter(!Outcome %in% c("Closed", "Merged"))

cat("Total unique practices in list:", nrow(closed_merged), "\n")

### 3. Load full workforce timeseries
wf <- read_csv("../../data/workforce/workforce_year.csv")

### 4. Get reliable patient counts (remove years with >10% drop)
wf_trends <- wf %>%
  filter(Practice.Code %in% closed_merged$Practice.Code) %>%
  filter(!is.na(TOTAL_PATIENTS), TOTAL_PATIENTS > 0) %>%
  arrange(Practice.Code, Year) %>%
  group_by(Practice.Code) %>%
  mutate(
    prev_patient = lag(TOTAL_PATIENTS),
    annual_change = TOTAL_PATIENTS - prev_patient,
    percent_change = annual_change / prev_patient * 100
  ) %>%
  ungroup()

# Keep only latest stable year
latest_reliable_wf <- wf_trends %>%
  group_by(Practice.Code) %>%
  filter(is.na(percent_change) | percent_change > -10) %>%
  slice_max(Year, n = 1, with_ties = FALSE) %>%
  ungroup()

### 5. Join reliable counts to master dataframe
closed_merged <- closed_merged %>%
  left_join(
    latest_reliable_wf %>%
      select(Practice.Code, TOTAL_PATIENTS),
    by = "Practice.Code"
  )

# Identify practices not found in workforce data
missing <- closed_merged %>%
  filter(is.na(TOTAL_PATIENTS)) %>%
  pull(Practice.Code)

cat("Practices missing from workforce data:", length(missing), "\n")

### 6. Total patients affected by closure or merger
closed_merged %>%
  group_by(Outcome) %>%
  summarise(
    total_patients = sum(TOTAL_PATIENTS, na.rm = TRUE),
    mean_patients = mean(TOTAL_PATIENTS, na.rm = TRUE),
    .groups = "drop"
  )

### 7. Get latest reliable GP partner counts (FTE)
partner_trends <- wf %>%
  filter(Practice.Code %in% closed_merged$Practice.Code) %>%
  mutate(GP_partners = TOTAL_GP_SEN_PTNR_FTE + TOTAL_GP_PTNR_PROV_FTE) %>%
  filter(!is.na(GP_partners)) %>%
  arrange(Practice.Code, Year) %>%
  group_by(Practice.Code) %>%
  mutate(
    prev_partners = lag(GP_partners),
    annual_change = GP_partners - prev_partners,
    percent_change = annual_change / prev_partners * 100
  ) %>%
  filter(is.na(percent_change) | percent_change > -33) %>%
  slice_max(Year, n = 1, with_ties = FALSE) %>%
  ungroup()

# Join partner count to closed_merged
closed_merged <- closed_merged %>%
  left_join(
    partner_trends %>%
      select(Practice.Code, GP_partners),
    by = "Practice.Code"
  )

# Summary of partner counts
closed_merged %>%
  group_by(Outcome) %>%
  summarise(
    mean_partners = mean(GP_partners, na.rm = TRUE),
    median_partners = median(GP_partners, na.rm = TRUE),
    .groups = "drop"
  )

### 8. Get latest reliable total GP counts (FTE)
gp_trends <- wf %>%
  filter(Practice.Code %in% closed_merged$Practice.Code) %>%
  filter(!is.na(TOTAL_GP_EXTGL_FTE)) %>%
  arrange(Practice.Code, Year) %>%
  group_by(Practice.Code) %>%
  mutate(
    prev_gp = lag(TOTAL_GP_EXTGL_FTE),
    annual_change = TOTAL_GP_EXTGL_FTE - prev_gp,
    percent_change = annual_change / prev_gp * 100
  ) %>%
  filter(is.na(percent_change) | percent_change > -33) %>%
  slice_max(Year, n = 1, with_ties = FALSE) %>%
  ungroup()

# Join total GP count to closed_merged
closed_merged <- closed_merged %>%
  left_join(
    gp_trends %>%
      select(Practice.Code, TOTAL_GP_EXTGL_FTE),
    by = "Practice.Code"
  )

closed_merged %>%
  group_by(Outcome) %>%
  summarise(
    mean_gps = mean(TOTAL_GP_EXTGL_FTE, na.rm = TRUE),
    median_gps = median(TOTAL_GP_EXTGL_FTE, na.rm = TRUE),
    .groups = "drop"
  )

### 9. Analyse practices not in closed_merged (i.e. open practices)
open_practice_codes <- setdiff(unique(wf$Practice.Code), closed_merged$Practice.Code)

# Filter workforce data to open practices
wf_open <- wf %>%
  filter(Practice.Code %in% open_practice_codes)

## --- PATIENTS --- ##
open_patient_trends <- wf_open %>%
  filter(!is.na(TOTAL_PATIENTS), TOTAL_PATIENTS > 0) %>%
  arrange(Practice.Code, Year) %>%
  group_by(Practice.Code) %>%
  mutate(
    prev_patient = lag(TOTAL_PATIENTS),
    annual_change = TOTAL_PATIENTS - prev_patient,
    percent_change = annual_change / prev_patient * 100
  ) %>%
  filter(is.na(percent_change) | percent_change > -10) %>%
  slice_max(Year, n = 1, with_ties = FALSE) %>%
  ungroup()


## --- GP PARTNERS --- ##
open_partner_trends <- wf_open %>%
  mutate(GP_partners = TOTAL_GP_SEN_PTNR_FTE + TOTAL_GP_PTNR_PROV_FTE) %>%
  filter(!is.na(GP_partners)) %>%
  arrange(Practice.Code, Year) %>%
  group_by(Practice.Code) %>%
  mutate(
    prev_partners = lag(GP_partners),
    annual_change = GP_partners - prev_partners,
    percent_change = annual_change / prev_partners * 100
  ) %>%
  filter(is.na(percent_change) | percent_change > -33) %>%
  slice_max(Year, n = 1, with_ties = FALSE) %>%
  ungroup()

## --- TOTAL GPs --- ##
open_gp_trends <- wf_open %>%
  filter(!is.na(TOTAL_GP_EXTGL_FTE)) %>%
  arrange(Practice.Code, Year) %>%
  group_by(Practice.Code) %>%
  mutate(
    prev_gp = lag(TOTAL_GP_EXTGL_FTE),
    annual_change = TOTAL_GP_EXTGL_FTE - prev_gp,
    percent_change = annual_change / prev_gp * 100
  ) %>%
  filter(is.na(percent_change) | percent_change > -33) %>%
  slice_max(Year, n = 1, with_ties = FALSE) %>%
  ungroup()

open_summary <- open_patient_trends %>%
  select(Practice.Code, TOTAL_PATIENTS) %>%
  left_join(open_partner_trends %>% select(Practice.Code, GP_partners), by = "Practice.Code") %>%
  left_join(open_gp_trends %>% select(Practice.Code, TOTAL_GP_EXTGL_FTE), by = "Practice.Code")

open_summary %>%
  summarise(
    mean_patients = mean(TOTAL_PATIENTS, na.rm = TRUE),
    mean_partners = mean(GP_partners, na.rm = TRUE),
    mean_total_gps = mean(TOTAL_GP_EXTGL_FTE, na.rm = TRUE),
    partner_ratio = mean(GP_partners / TOTAL_GP_EXTGL_FTE, na.rm = TRUE)
  )

  #Patients in 2019
wf20 %>% summarise( 
  mean_patients = mean(TOTAL_PATIENTS, na.rm=TRUE)
)

### 10. Flag single-handed practices (GP FTE ≤ 1)
closed_merged <- closed_merged %>%
  mutate(
    is_single_handed = TOTAL_GP_EXTGL_FTE <= 1 & !is.na(TOTAL_GP_EXTGL_FTE)
  )

# Summary: single-handedness by outcome
closed_merged %>%
  group_by(Outcome) %>%
  summarise(
    single_handed_count = sum(is_single_handed, na.rm = TRUE),
    total = n(),
    percent_single_handed = single_handed_count / total * 100,
    .groups = "drop"
  )

open_summary <- open_summary %>%
  mutate(
    is_single_handed = TOTAL_GP_EXTGL_FTE <= 1 & !is.na(TOTAL_GP_EXTGL_FTE)
  )

open_summary %>%
  summarise(
    single_handed_count = sum(is_single_handed),
    total = n(),
    percent_single_handed = single_handed_count / total * 100
  )
