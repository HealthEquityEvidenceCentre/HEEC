library(googlesheets4)
library(dplyr)

sheet_id <- "1Z4s88OQTTxKvePag9ugUZantq3F9CRpJL0ASMX1Kkcc"

sheet_names <- sheet_names(sheet_id)

sheets_list <- lapply(sheet_names, function(sheet) {
  read_sheet(sheet_id, sheet = sheet) %>%
    mutate(Region = sheet)
})

combined_df <- bind_rows(sheets_list) # Add column for sheet names

### Compare with payments closure date
payments <- read.csv("../../data/payments/payments.csv")
payments$Practice.Close.Date

closed_list <- combined_df[combined_df$Outcome == "Closed", ]$`Practice Code`
merged_list <- combined_df[combined_df$Outcome == "Merged", ]$`Practice Code`

payments[payments$Practice.Code %in% closed_list, ]$Practice.Code %>% unique()
payments[payments$Practice.Code %in% merged_list & !(payments$Practice.Close.Date %in% c("-", "", " ")), ]$Practice.Code %>% unique()
# Manual inspection confirmed that 'merger' is the correct categorisation (as there is still a practice located at the location),
# meaning that Practice.Close.Date does not distinguish between Closure and Merger

### Counts
combined_df$`Practice Code` %>%
  unique() %>%
  length()

(
  combined_df[combined_df$Outcome == "Closed", ]$`Practice Code` %>%
    unique() %>%
    length()
) /
  (
    combined_df$`Practice Code` %>%
      unique() %>%
      length()
  ) * 100

for (region in unique(combined_df$Region)) {
  print(region)
  combined_df[combined_df$Region == region & combined_df$Outcome == "Closed", ] %>%
    nrow() %>%
    print()
  combined_df[combined_df$Region == region & combined_df$Outcome == "Merged", ] %>%
    nrow() %>%
    print()
}

combined_df[combined_df$Outcome == "Closed", ] %>%
  nrow() %>%
  print()
combined_df[combined_df$Outcome == "Merged", ] %>%
  nrow() %>%
  print()

### Closed analysis
closed_df <- combined_df[combined_df$Outcome == "Closed", ]$`Practice Code`
merged_df <- combined_df[combined_df$Outcome == "Merged", ]$`Practice Code`

# Rurality/Dispesning
combined_df[combined_df$Outcome == "Closed" & combined_df$Rurality == "Rural", ] %>% nrow()
combined_df[combined_df$Outcome == "Closed" & combined_df$Rurality == "Urban", ] %>% nrow()

combined_df[combined_df$Outcome == "Closed" & combined_df$Dispensing == "Yes", ] %>% nrow()
combined_df[combined_df$Outcome == "Closed" & combined_df$Dispensing == "No", ] %>% nrow()

# Payments
payments <- read.csv("../../data/payments/payments.csv")
payments[payments$Practice.Code %in% closed_df, ]

payments %>%
  filter(Practice.Code %in% closed_df) %>% # Keep only closed practices
  filter(!is.na(Number.of.Registered.Patients..Last.Known.Figure.)) %>%
  filter(Number.of.Registered.Patients..Last.Known.Figure. > 0) %>% # Ignore 0 counts
  group_by(Practice.Code) %>%
  slice_max(Year, n = 1, with_ties = FALSE) %>%
  select("Practice.Code", "Year", "Number.of.Registered.Patients..Last.Known.Figure.")

# Closed
patient_trends <- payments %>%
  filter(Practice.Code %in% closed_df) %>%
  filter(!is.na(Number.of.Registered.Patients..Last.Known.Figure.)) %>%
  filter(Number.of.Registered.Patients..Last.Known.Figure. > 0) %>% # Ignore 0 counts
  arrange(Practice.Code, Year) %>% # Ensure chronological order
  group_by(Practice.Code) %>%
  mutate(
    prev_patients = lag(Number.of.Registered.Patients..Last.Known.Figure.), # Previous year patient count
    annual_change = Number.of.Registered.Patients..Last.Known.Figure. - prev_patients,
    percent_change = annual_change / prev_patients * 100
  ) %>%
  ungroup()

latest_reliable_patients <- patient_trends %>%
  group_by(Practice.Code) %>%
  filter(percent_change > -33) %>% # Exclude final year if large drop
  slice_max(Year, n = 1, with_ties = FALSE) %>% # Take latest stable year
  select(Practice.Code, Year, Number.of.Registered.Patients..Last.Known.Figure., Average.payments.per.weighted.patient, percent_change)

latest_reliable_patients$Number.of.Registered.Patients..Last.Known.Figure. %>% sum()
latest_reliable_patients$Number.of.Registered.Patients..Last.Known.Figure. %>% mean()
latest_reliable_patients$Average.payments.per.weighted.patient %>% mean()

# Merged
patient_trends <- payments %>%
  filter(Practice.Code %in% merged_df) %>%
  filter(!is.na(Number.of.Registered.Patients..Last.Known.Figure.)) %>%
  filter(Number.of.Registered.Patients..Last.Known.Figure. > 0) %>% # Ignore 0 counts
  arrange(Practice.Code, Year) %>% # Ensure chronological order
  group_by(Practice.Code) %>%
  mutate(
    prev_patients = lag(Number.of.Registered.Patients..Last.Known.Figure.), # Previous year patient count
    annual_change = Number.of.Registered.Patients..Last.Known.Figure. - prev_patients,
    percent_change = annual_change / prev_patients * 100
  ) %>%
  ungroup()

latest_reliable_patients <- patient_trends %>%
  group_by(Practice.Code) %>%
  filter(percent_change > -33) %>% # Exclude final year if large drop
  slice_max(Year, n = 1, with_ties = FALSE) %>% # Take latest stable year
  select(Practice.Code, Year, Number.of.Registered.Patients..Last.Known.Figure., Average.payments.per.weighted.patient, percent_change)

latest_reliable_patients$Number.of.Registered.Patients..Last.Known.Figure. %>% mean()
latest_reliable_patients$Average.payments.per.weighted.patient %>% mean()

### Average list size: before/after
payments20 <- read.csv("../../data/payments/raw/19-20.csv")
payments23 <- read.csv("../../data/payments/raw/22-23.csv")

payments20 %>%
  filter(!is.na(Number.of.Registered.Patients..Last.Known.Figure.)) %>%
  filter(Number.of.Registered.Patients..Last.Known.Figure. > 0) %>%
  filter(Total.NHS.Payments.to.General.Practice > 0) %>%
  filter(Average.payments.per.registered.patient < 600) %>%
  summarise(
    mean_value = mean(Number.of.Registered.Patients..Last.Known.Figure., na.rm = TRUE),
    mean_payments = mean(as.numeric(Average.payments.per.weighted.patient), na.rm = TRUE)
  )

payments20$Number.of.Registered.Patients..Last.Known.Figure. %>% sum()

payments23 %>%
  filter(!is.na(Number.of.Registered.Patients..Last.Known.Figure.)) %>%
  filter(Number.of.Registered.Patients..Last.Known.Figure. > 0) %>%
  summarise(mean_value = mean(Number.of.Registered.Patients..Last.Known.Figure., na.rm = TRUE))

### Workforce
wf15 <- read.csv("../../data/workforce/raw/15_09.csv")
wf23 <- read.csv("../../data/workforce/raw/23_09.csv")

practice_codes_not_in_wf20 <- setdiff(wf15$PRAC_CODE, wf23$PRAC_CODE)

setdiff(practice_codes_not_in_wf20, unique(combined_df$`Practice Code`))
setdiff(unique(combined_df$`Practice Code`), practice_codes_not_in_wf20)

# raw data 2015-20
workforce <- data.frame()

read.csv("../../data/workforce/raw/19_09.csv") %>% colnames()
list.files("../../data/workforce/raw")[5:36]

for (file in sort(list.files("../../data/workforce/raw"))[1:36]) {
  print(file)

  df <- read.csv(paste0("../../data/workforce/raw/", file)) %>%
    select(
      PRAC_CODE, TOTAL_GP_HC, TOTAL_GP_FTE
    ) %>%
    mutate(
      Date = gsub(".csv", "", file)
    )

  workforce <- rbind(workforce, df)
}

workforce$Date <- workforce$Date %>%
  gsub("_", "", .)

closed_wf <- workforce[workforce$PRAC_CODE %in% closed_list, ]

closed_wf[max(closed_wf$TOTAL_GP_HC) == 0, ]
closed_wf[closed_wf$TOTAL_GP_HC <= 1 | closed_wf$TOTAL_GP_FTE < 1, ] %>% arrange(PRAC_CODE)
single_closed <- closed_wf[closed_wf$TOTAL_GP_HC <= 1 | closed_wf$TOTAL_GP_FTE < 1, ]$PRAC_CODE %>%
  unique() %>%
  sort()

workforce[workforce$PRAC_CODE %in% single_closed, ] %>% arrange(PRAC_CODE)

workforce[workforce$PRAC_CODE %in% closed_list, ]$PRAC_CODE %>%
  unique() %>%
  length()

single_handed <- workforce %>%
  arrange(PRAC_CODE, Date) %>% # Ensure data is sorted correctly
  group_by(PRAC_CODE) %>%
  filter(
    TOTAL_GP_HC <= 1 & TOTAL_GP_FTE <= 1 &
      lag(TOTAL_GP_HC, 1) <= 1 & lag(TOTAL_GP_FTE, 1) <= 1 &
      lag(TOTAL_GP_HC, 2) <= 1 & lag(TOTAL_GP_FTE, 2) <= 1
  ) %>%
  group_by(PRAC_CODE) %>%
  filter(Date == max(Date)) %>%
  ungroup()

workforce %>%
  arrange(PRAC_CODE, Date) %>% # Ensure data is sorted correctly
  group_by(PRAC_CODE) %>%
  mutate(
    last_3_valid = sum(!is.na(TOTAL_GP_HC) & TOTAL_GP_HC <= 1 & TOTAL_GP_FTE <= 1, na.rm = TRUE) # Count valid months
  ) %>%
  filter(last_3_valid == 3) %>% # Ensure last 3 reported months meet single-handed criteria
  filter(Date == max(Date, na.rm = TRUE)) %>% # Keep only the latest entry per practice
  ungroup()

closed_list %in% unique(single_handed$PRAC_CODE)

# closed
wf_trends <- wf %>%
  filter(Practice.Code %in% closed_df) %>%
  filter(!is.na(TOTAL_GP_EXTGL_FTE)) %>%
  filter(TOTAL_GP_EXTGL_FTE > 0) %>% # Ignore 0 counts
  arrange(Practice.Code, Year)

group_by(Practice.Code) %>%
  mutate(
    prev_GP = lag(TOTAL_GP_EXTGL_FTE), # Previous year patient count
    annual_change = TOTAL_GP_EXTGL_FTE - prev_GP,
    percent_change = annual_change / prev_GP * 100
  ) %>%
  ungroup()

latest_reliable_wf <- wf_trends %>%
  group_by(Practice.Code) %>%
  filter(percent_change > -33) %>% # Exclude final year if large drop
  slice_max(Year, n = 1, with_ties = FALSE) %>% # Take latest stable year
  select(Practice.Code, Year, TOTAL_GP_EXTGL_FTE, percent_change)

latest_reliable_wf %>%
  inner_join(latest_reliable_patients[, -which(names(latest_reliable_patients) == "Year")], by = "Practice.Code") %>%
  mutate(GP_per_10k = (TOTAL_GP_EXTGL_FTE / Number.of.Registered.Patients..Last.Known.Figure. * 10000))
pull(GP_per_10k) %>%
  mean(., na.rm = TRUE)

# merged
wf_trends <- wf %>%
  filter(Practice.Code %in% merged_df) %>%
  filter(!is.na(TOTAL_GP_EXTGL_FTE)) %>%
  filter(TOTAL_GP_EXTGL_FTE > 0) %>% # Ignore 0 counts
  arrange(Practice.Code, Year) %>% # Ensure chronological order
  group_by(Practice.Code) %>%
  mutate(
    prev_GP = lag(TOTAL_GP_EXTGL_FTE), # Previous year patient count
    annual_change = TOTAL_GP_EXTGL_FTE - prev_GP,
    percent_change = annual_change / prev_GP * 100
  ) %>%
  ungroup()

latest_reliable_wf <- wf_trends %>%
  group_by(Practice.Code) %>%
  filter(percent_change > -33) %>% # Exclude final year if large drop
  slice_max(Year, n = 1, with_ties = FALSE) %>% # Take latest stable year
  select(Practice.Code, Year, TOTAL_GP_EXTGL_FTE, percent_change)

latest_reliable_wf %>%
  inner_join(latest_reliable_patients[, -which(names(latest_reliable_patients) == "Year")], by = "Practice.Code") %>%
  mutate(GP_per_10k = (TOTAL_GP_EXTGL_FTE / Number.of.Registered.Patients..Last.Known.Figure. * 10000)) %>%
  pull(GP_per_10k) %>%
  mean(., na.rm = TRUE)

# All
wf %>%
  filter(Year == 2020) %>%
  filter(!(Practice.Code %in% closed_df & Practice.Code %in% merged_df)) %>%
  filter(!is.na(TOTAL_GP_EXTGL_FTE)) %>%
  filter(TOTAL_GP_EXTGL_FTE > 0) %>%
  inner_join(payments20, by = "Practice.Code") %>%
  mutate(GP_per_10k = (TOTAL_GP_EXTGL_FTE / Number.of.Registered.Patients..Last.Known.Figure. * 10000)) %>%
  filter(GP_per_10k != Inf) %>%
  pull(GP_per_10k) %>%
  mean(., na.rm = TRUE)
