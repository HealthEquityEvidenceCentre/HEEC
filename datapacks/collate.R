pks <- c(
  "fingertipsR", "tidyverse", "purrr", "tibble",
  "spatstat", "lubridate", "readODS", "janitor", "patchwork", "magrittr"
)

sapply(pks, library, character.only = T)

"%ni%" <- Negate("%in%")

df <- data.frame()

### quintile counts ---------------------------------------------------------
payments <- read.csv("../data/payments/payments.csv")

england <- payments %>%
  filter(Year == 2019) %>%
  group_by(Year) %>%
  summarise(total = n(), .groups = "drop")

# Calculate total count by Year, ICB.NAME
total_counts <- payments %>%
  filter(Year == 2019) %>%
  group_by(Year, ICB.NAME) %>%
  summarise(total = n(), .groups = "drop")

# Calculate count and percentage by IMD_quintile for all ICBs
result <- payments %>%
  filter(Year == 2019) %>%
  group_by(Year, IMD_quintile, ICB.NAME) %>%
  summarise(n = n(), .groups = "drop") %>%
  left_join(total_counts, by = c("Year", "ICB.NAME")) %>%
  mutate(perc = (n / total) * 100) %>%
  select(Year, ICB.NAME, IMD_quintile, n, perc)

result

### Life expectancy ---------------------------------------------------------
le <- read.csv("../data/life_expectancy/life_expectancy.csv") %>%
  pivot_longer(
    cols = c(Life_Expectancy_Male, Life_Expectancy_Female),
    names_to = "Indicator",
    values_to = "Value"
  ) %>%
  filter(!is.na(Value), !is.na(IMD_quintile))

avg <- le %>%
  group_by(Indicator, Year) %>%
  summarise(
    avg = median(Value, na.rm = TRUE),
  )

le_england <- le %>%
  group_by(Indicator, Year, IMD_quintile) %>%
  summarise(
    ICB.NAME = "England",
    Value = median(Value, na.rm = TRUE),
  ) %>%
  ungroup()

le_agg <- le %>%
  group_by(Indicator, Year, IMD_quintile, ICB.NAME) %>%
  summarise(
    Value = median(Value, na.rm = TRUE)
  ) %>%
  ungroup()

le <- bind_rows(le_england, le_agg) %>%
  pivot_wider(
    names_from = IMD_quintile,
    values_from = Value,
    names_prefix = "quin_"
  ) %>%
  merge(., avg, by = c("Year", "Indicator"))

df <- bind_rows(df, le)

df$ICB.NAME %>%
  unique() %>%
  length()

df %>%
  filter(Year == 2023) %>%
  filter(is.na(quin_1)) %>%
  select(Year, Indicator, ICB.NAME) %>%
  arrange(ICB.NAME)

df %>%
  filter(Year == 2023) %>%
  filter(is.na(quin_5)) %>%
  select(Year, Indicator, ICB.NAME) %>%
  arrange(ICB.NAME)

write.csv(df, "final_data.csv", row.names = FALSE)

### Payments to General Practice -----------------------------------
payments <- read.csv("../data/payments/payments.csv") %>%
  select(Practice.Code, Year, IMD_quintile, ICB.NAME, Dispensing.Practice, Total.NHS.Payments.to.General.Practice, Number.of.Weighted.Patients..Last.Known.Figure.)

# All practices
avg_all <- payments %>%
  group_by(Year) %>%
  summarise(
    Total.Payments = sum(Total.NHS.Payments.to.General.Practice, na.rm = TRUE),
    Number.of.Weighted.Patients = sum(Number.of.Weighted.Patients..Last.Known.Figure., na.rm = TRUE)
  ) %>%
  mutate(
    avg = Total.Payments / Number.of.Weighted.Patients,
    Indicator = "payment_per_patient_all"
  ) %>%
  select(-c(Total.Payments, Number.of.Weighted.Patients))

payments_england_all <- payments %>%
  group_by(Year, IMD_quintile) %>%
  summarise(
    Total.Payments = sum(Total.NHS.Payments.to.General.Practice, na.rm = TRUE),
    Number.of.Weighted.Patients = sum(Number.of.Weighted.Patients..Last.Known.Figure., na.rm = TRUE)
  ) %>%
  mutate(Value = Total.Payments / Number.of.Weighted.Patients) %>%
  select(-c(Total.Payments, Number.of.Weighted.Patients)) %>%
  group_by(Year, IMD_quintile) %>%
  summarise(
    ICB.NAME = "England",
    Indicator = "payment_per_patient_all",
    Value = median(Value, na.rm = TRUE),
  ) %>%
  ungroup() %>%
  filter(!is.na(IMD_quintile))

payments_agg_all <- payments %>%
  group_by(Year, ICB.NAME, IMD_quintile) %>%
  summarise(
    Total.Payments = sum(Total.NHS.Payments.to.General.Practice, na.rm = TRUE),
    Number.of.Weighted.Patients = sum(Number.of.Weighted.Patients..Last.Known.Figure., na.rm = TRUE)
  ) %>%
  mutate(Value = Total.Payments / Number.of.Weighted.Patients) %>%
  select(-c(Total.Payments, Number.of.Weighted.Patients)) %>%
  group_by(Year, ICB.NAME, IMD_quintile) %>%
  summarise(
    Indicator = "payment_per_patient_all",
    Value = median(Value, na.rm = TRUE),
  ) %>%
  ungroup() %>%
  filter(!is.na(IMD_quintile))

payments_all <- bind_rows(payments_england_all, payments_agg_all) %>%
  pivot_wider(
    names_from = IMD_quintile,
    values_from = Value,
    names_prefix = "quin_"
  ) %>%
  merge(., avg_all, by = c("Year", "Indicator"))

df <- bind_rows(df, payments_all)

# Dispensing practices
avg_disp <- payments[payments$Dispensing.Practice == "Yes", ] %>%
  group_by(Year) %>%
  summarise(
    Total.Payments = sum(Total.NHS.Payments.to.General.Practice, na.rm = TRUE),
    Number.of.Weighted.Patients = sum(Number.of.Weighted.Patients..Last.Known.Figure., na.rm = TRUE)
  ) %>%
  mutate(
    avg = Total.Payments / Number.of.Weighted.Patients,
    Indicator = "payment_per_patient_disp"
  ) %>%
  select(-c(Total.Payments, Number.of.Weighted.Patients))

payments_england_disp <- payments[payments$Dispensing.Practice == "Yes", ] %>%
  group_by(Year, IMD_quintile) %>%
  summarise(
    Total.Payments = sum(Total.NHS.Payments.to.General.Practice, na.rm = TRUE),
    Number.of.Weighted.Patients = sum(Number.of.Weighted.Patients..Last.Known.Figure., na.rm = TRUE)
  ) %>%
  mutate(Value = Total.Payments / Number.of.Weighted.Patients) %>%
  select(-c(Total.Payments, Number.of.Weighted.Patients)) %>%
  group_by(Year, IMD_quintile) %>%
  summarise(
    ICB.NAME = "England",
    Indicator = "payment_per_patient_disp",
    Value = median(Value, na.rm = TRUE),
  ) %>%
  ungroup() %>%
  filter(!is.na(IMD_quintile))

payments_agg_disp <- payments[payments$Dispensing.Practice == "Yes", ] %>%
  group_by(Year, ICB.NAME, IMD_quintile) %>%
  summarise(
    Total.Payments = sum(Total.NHS.Payments.to.General.Practice, na.rm = TRUE),
    Number.of.Weighted.Patients = sum(Number.of.Weighted.Patients..Last.Known.Figure., na.rm = TRUE)
  ) %>%
  mutate(Value = Total.Payments / Number.of.Weighted.Patients) %>%
  select(-c(Total.Payments, Number.of.Weighted.Patients)) %>%
  group_by(Year, ICB.NAME, IMD_quintile) %>%
  summarise(
    Indicator = "payment_per_patient_disp",
    Value = median(Value, na.rm = TRUE),
  ) %>%
  ungroup() %>%
  filter(!is.na(IMD_quintile))

payments_disp <- bind_rows(payments_england_disp, payments_agg_disp) %>%
  pivot_wider(
    names_from = IMD_quintile,
    values_from = Value,
    names_prefix = "quin_"
  ) %>%
  merge(., avg_disp, by = c("Year", "Indicator"))

df <- bind_rows(df, payments_disp)

# Non-dispensing
avg_non_disp <- payments[payments$Dispensing.Practice == "No", ] %>%
  group_by(Year) %>%
  summarise(
    Total.Payments = sum(Total.NHS.Payments.to.General.Practice, na.rm = TRUE),
    Number.of.Weighted.Patients = sum(Number.of.Weighted.Patients..Last.Known.Figure., na.rm = TRUE)
  ) %>%
  mutate(
    avg = Total.Payments / Number.of.Weighted.Patients,
    Indicator = "payment_per_patient_non_disp"
  ) %>%
  select(-c(Total.Payments, Number.of.Weighted.Patients))

payments_england_non_disp <- payments[payments$Dispensing.Practice == "No", ] %>%
  group_by(Year, IMD_quintile) %>%
  summarise(
    Total.Payments = sum(Total.NHS.Payments.to.General.Practice, na.rm = TRUE),
    Number.of.Weighted.Patients = sum(Number.of.Weighted.Patients..Last.Known.Figure., na.rm = TRUE)
  ) %>%
  mutate(Value = Total.Payments / Number.of.Weighted.Patients) %>%
  select(-c(Total.Payments, Number.of.Weighted.Patients)) %>%
  group_by(Year, IMD_quintile) %>%
  summarise(
    ICB.NAME = "England",
    Indicator = "payment_per_patient_non_disp",
    Value = median(Value, na.rm = TRUE),
  ) %>%
  ungroup() %>%
  filter(!is.na(IMD_quintile))

payments_agg_non_disp <- payments[payments$Dispensing.Practice == "No", ] %>%
  group_by(Year, ICB.NAME, IMD_quintile) %>%
  summarise(
    Total.Payments = sum(Total.NHS.Payments.to.General.Practice, na.rm = TRUE),
    Number.of.Weighted.Patients = sum(Number.of.Weighted.Patients..Last.Known.Figure., na.rm = TRUE)
  ) %>%
  mutate(Value = Total.Payments / Number.of.Weighted.Patients) %>%
  select(-c(Total.Payments, Number.of.Weighted.Patients)) %>%
  group_by(Year, ICB.NAME, IMD_quintile) %>%
  summarise(
    Indicator = "payment_per_patient_non_disp",
    Value = median(Value, na.rm = TRUE),
  ) %>%
  ungroup() %>%
  filter(!is.na(IMD_quintile))

payments_non_disp <- bind_rows(payments_england_non_disp, payments_agg_non_disp) %>%
  pivot_wider(
    names_from = IMD_quintile,
    values_from = Value,
    names_prefix = "quin_"
  ) %>%
  merge(., avg_non_disp, by = c("Year", "Indicator"))

df <- bind_rows(df, payments_non_disp)

write.csv(df, "final_data.csv", row.names = FALSE)

### Workforce ---------------------------------------------------------
workforce <- read.csv("../data/workforce/workforce_year.csv") %>%
  select(-IMD) %>%
  rename(ICB.NAME = ICB_NAME)

n_w_patients <- payments[, c("Practice.Code", "Year", "Number.of.Weighted.Patients..Last.Known.Figure.")] %>%
  filter(Year == 2023) %>%
  group_by(Year, Practice.Code) %>%
  summarise(
    Number.of.Weighted.Patients..Last.Known.Figure. = sum(Number.of.Weighted.Patients..Last.Known.Figure., na.rm = TRUE)
  )

avg <- workforce %>%
  filter(Year == 2023) %>%
  merge(., n_w_patients, by = c("Practice.Code", "Year")) %>%
  group_by(Year) %>%
  summarise(
    TOTAL_PATIENTS = sum(Number.of.Weighted.Patients..Last.Known.Figure., na.rm = TRUE),
    TOTAL_GP_EXTGL_FTE = sum(TOTAL_GP_EXTGL_FTE, na.rm = TRUE),
    TOTAL_LOCUUM_TRN_FTE = sum(TOTAL_LOCUUM_TRN_FTE, na.rm = TRUE),
    TOTAL_NURSES_FTE = sum(TOTAL_NURSES_FTE, na.rm = TRUE),
    TOTAL_ADMIN_FTE = sum(TOTAL_ADMIN_FTE, na.rm = TRUE),
    TOTAL_DPC_FTE = sum(TOTAL_DPC_FTE, na.rm = TRUE),
  ) %>%
  ungroup() %>%
  mutate(
    TOTAL_GP_EXTGL_FTE = TOTAL_GP_EXTGL_FTE / TOTAL_PATIENTS * 10000,
    TOTAL_LOCUUM_TRN_FTE = TOTAL_LOCUUM_TRN_FTE / TOTAL_PATIENTS * 10000,
    TOTAL_NURSES_FTE = TOTAL_NURSES_FTE / TOTAL_PATIENTS * 10000,
    TOTAL_ADMIN_FTE = TOTAL_ADMIN_FTE / TOTAL_PATIENTS * 10000,
    TOTAL_DPC_FTE = TOTAL_DPC_FTE / TOTAL_PATIENTS * 10000
  ) %>%
  select(-TOTAL_PATIENTS) %>%
  pivot_longer(
    cols = c(TOTAL_GP_EXTGL_FTE, TOTAL_LOCUUM_TRN_FTE, TOTAL_NURSES_FTE, TOTAL_ADMIN_FTE, TOTAL_DPC_FTE),
    names_to = "Indicator",
    values_to = "avg"
  )

workforce_england <- workforce %>%
  filter(Year == 2023) %>%
  merge(., n_w_patients, by = c("Practice.Code", "Year")) %>%
  group_by(Year, IMD_quintile) %>%
  summarise(
    ICB.NAME = "England",
    TOTAL_PATIENTS = sum(Number.of.Weighted.Patients..Last.Known.Figure., na.rm = TRUE),
    TOTAL_GP_EXTGL_FTE = sum(TOTAL_GP_EXTGL_FTE, na.rm = TRUE),
    TOTAL_LOCUUM_TRN_FTE = sum(TOTAL_LOCUUM_TRN_FTE, na.rm = TRUE),
    TOTAL_NURSES_FTE = sum(TOTAL_NURSES_FTE, na.rm = TRUE),
    TOTAL_ADMIN_FTE = sum(TOTAL_ADMIN_FTE, na.rm = TRUE),
    TOTAL_DPC_FTE = sum(TOTAL_DPC_FTE, na.rm = TRUE),
  ) %>%
  ungroup() %>%
  mutate(
    TOTAL_GP_EXTGL_FTE = TOTAL_GP_EXTGL_FTE / TOTAL_PATIENTS * 10000,
    TOTAL_LOCUUM_TRN_FTE = TOTAL_LOCUUM_TRN_FTE / TOTAL_PATIENTS * 10000,
    TOTAL_NURSES_FTE = TOTAL_NURSES_FTE / TOTAL_PATIENTS * 10000,
    TOTAL_ADMIN_FTE = TOTAL_ADMIN_FTE / TOTAL_PATIENTS * 10000,
    TOTAL_DPC_FTE = TOTAL_DPC_FTE / TOTAL_PATIENTS * 10000
  ) %>%
  select(-TOTAL_PATIENTS) %>%
  filter(!is.na(IMD_quintile))

workforce_agg <- workforce %>%
  filter(Year == 2023) %>%
  merge(., n_w_patients, by = c("Practice.Code", "Year")) %>%
  group_by(Year, ICB.NAME, IMD_quintile) %>%
  summarise(
    TOTAL_PATIENTS = sum(TOTAL_PATIENTS, na.rm = TRUE),
    TOTAL_GP_EXTGL_FTE = sum(TOTAL_GP_EXTGL_FTE, na.rm = TRUE),
    TOTAL_LOCUUM_TRN_FTE = sum(TOTAL_LOCUUM_TRN_FTE, na.rm = TRUE),
    TOTAL_NURSES_FTE = sum(TOTAL_NURSES_FTE, na.rm = TRUE),
    TOTAL_ADMIN_FTE = sum(TOTAL_ADMIN_FTE, na.rm = TRUE),
    TOTAL_DPC_FTE = sum(TOTAL_DPC_FTE, na.rm = TRUE),
  ) %>%
  ungroup() %>%
  mutate(
    TOTAL_GP_EXTGL_FTE = TOTAL_GP_EXTGL_FTE / TOTAL_PATIENTS * 10000,
    TOTAL_LOCUUM_TRN_FTE = TOTAL_LOCUUM_TRN_FTE / TOTAL_PATIENTS * 10000,
    TOTAL_NURSES_FTE = TOTAL_NURSES_FTE / TOTAL_PATIENTS * 10000,
    TOTAL_ADMIN_FTE = TOTAL_ADMIN_FTE / TOTAL_PATIENTS * 10000,
    TOTAL_DPC_FTE = TOTAL_DPC_FTE / TOTAL_PATIENTS * 10000
  ) %>%
  filter(!is.na(IMD_quintile))

workforce <- bind_rows(workforce_england, workforce_agg) %>%
  select(-TOTAL_PATIENTS) %>%
  pivot_longer(
    cols = c(TOTAL_GP_EXTGL_FTE, TOTAL_LOCUUM_TRN_FTE, TOTAL_NURSES_FTE, TOTAL_ADMIN_FTE, TOTAL_DPC_FTE),
    names_to = "Indicator",
    values_to = "Value"
  ) %>%
  filter(!is.na(Value), !is.na(IMD_quintile)) %>%
  pivot_wider(
    names_from = IMD_quintile,
    values_from = Value,
    names_prefix = "quin_"
  ) %>%
  merge(., avg, by = c("Year", "Indicator"))

df <- bind_rows(df, workforce)

### PCN Workforce -----------------
pcn <- read.csv("../data/pcn_workforce/pcn_workforce.csv") %>% select(-IMD)

n_w_patients <- read.csv("../data/payments/payments.csv") %>%
  select(Practice.Code, Year, Number.of.Weighted.Patients..Last.Known.Figure., PCN.Name) %>%
  rename(PCN_NAME = PCN.Name) %>%
  filter(Year == 2022) %>%
  group_by(Year, PCN_NAME) %>%
  summarise(Number.of.Weighted.Patients..Last.Known.Figure. = sum(Number.of.Weighted.Patients..Last.Known.Figure., na.rm = TRUE)) %>%
  filter(!is.na(PCN_NAME)) %>%
  mutate(Year = 2023)

pcn <- merge(pcn, n_w_patients, by = c("Year", "PCN_NAME"))

avg <- pcn %>%
  group_by(Year) %>%
  summarise(
    Number.of.Weighted.Patients..Last.Known.Figure. = sum(Number.of.Weighted.Patients..Last.Known.Figure., na.rm = TRUE),
    FTE = sum(FTE, na.rm = TRUE)
  ) %>%
  mutate(avg = FTE / Number.of.Weighted.Patients..Last.Known.Figure. * 10000) %>%
  select(-c(FTE, Number.of.Weighted.Patients..Last.Known.Figure.))

pcn_england <- pcn %>%
  group_by(Year, IMD_quintile) %>%
  summarise(
    Number.of.Weighted.Patients..Last.Known.Figure. = sum(Number.of.Weighted.Patients..Last.Known.Figure., na.rm = TRUE),
    Value = sum(FTE, na.rm = TRUE)
  ) %>%
  mutate(
    ICB_NAME = "England",
    Value = Value / Number.of.Weighted.Patients..Last.Known.Figure. * 10000
  ) %>%
  select(-Number.of.Weighted.Patients..Last.Known.Figure.)

pcn_agg <- pcn %>%
  group_by(Year, ICB_NAME, IMD_quintile) %>%
  summarise(
    Number.of.Weighted.Patients..Last.Known.Figure. = sum(Number.of.Weighted.Patients..Last.Known.Figure., na.rm = TRUE),
    Value = sum(FTE, na.rm = TRUE)
  ) %>%
  mutate(
    Value = Value / Number.of.Weighted.Patients..Last.Known.Figure. * 10000
  ) %>%
  select(-Number.of.Weighted.Patients..Last.Known.Figure.)

pcn <- bind_rows(pcn_england, pcn_agg) %>%
  mutate(Indicator = "PCN_staff") %>%
  pivot_wider(
    names_from = IMD_quintile,
    values_from = Value,
    names_prefix = "quin_"
  ) %>%
  merge(., avg, by = "Year") %>%
  rename(ICB.NAME = ICB_NAME)

pcn[pcn$ICB.NAME == "Cornwall and The Isles Of Scilly", ]$ICB.NAME <- "Cornwall and the Isles of Scilly"
pcn[pcn$ICB.NAME == "Hampshire and Isle Of Wight", ]$ICB.NAME <- "Hampshire and Isle of Wight"

df <- bind_rows(df, pcn)

write.csv(df, "final_data.csv", row.names = FALSE)

### Behaviours ---------------------------------------------------------
# Smoking
prev <- read.csv("../data/behaviours/behaviours.csv") %>%
  select(-c(IMD, Area.Name, Practice.Code)) %>%
  rename(ICB.NAME = ICB_NAME) %>%
  filter(!is.na(ICB.NAME))

avg <- prev %>%
  group_by(Indicator, Year) %>%
  summarise(avg = median(Value, na.rm = TRUE))

prev_england <- prev %>%
  group_by(Indicator, Year, IMD_quintile) %>%
  summarise(Value = median(Value, na.rm = TRUE)) %>%
  mutate(ICB.NAME = "England")

prev_agg <- prev %>%
  group_by(Indicator, Year, IMD_quintile, ICB.NAME) %>%
  summarise(Value = median(Value, na.rm = TRUE))

prev_all <- bind_rows(prev_england, prev_agg) %>%
  filter(!is.na(IMD_quintile)) %>%
  pivot_wider(
    names_from = IMD_quintile,
    values_from = Value,
    names_prefix = "quin_"
  ) %>%
  merge(., avg, by = c("Year", "Indicator"))

df <- bind_rows(df, prev_all)

write.csv(df, "final_data.csv", row.names = FALSE)

### Prevalence ---------------------------------------------------------
prev <- read.csv("../data/prevalence/ltc_prevalence.csv") %>%
  select(-c(IMD, area_name, Practice.Code)) %>%
  rename(ICB.NAME = ICB_NAME)

avg <- prev %>%
  group_by(Indicator, Year) %>%
  summarise(avg = median(Value, na.rm = TRUE))

prev_england <- prev %>%
  group_by(Indicator, Year, IMD_quintile) %>%
  summarise(Value = median(Value, na.rm = TRUE)) %>%
  mutate(ICB.NAME = "England")

prev_agg <- prev %>%
  group_by(Indicator, Year, IMD_quintile, ICB.NAME) %>%
  summarise(Value = median(Value, na.rm = TRUE))

prev <- bind_rows(prev_england, prev_agg) %>%
  filter(!is.na(IMD_quintile)) %>%
  pivot_wider(
    names_from = IMD_quintile,
    values_from = Value,
    names_prefix = "quin_"
  ) %>%
  merge(., avg, by = c("Year", "Indicator"))

df <- bind_rows(df, prev)

write.csv(df, "final_data.csv", row.names = FALSE)

### Service quality ---------------------------------------------------------
qof_points <- read.csv("../data/qof/total_qof_points.csv") %>%
  rename(ICB.NAME = ICB_NAME) %>%
  select(-c(Practice.Code, Area.Name, IMD)) %>%
  mutate(Value = as.numeric(Value)) %>%
  filter(!is.na(ICB.NAME))

avg <- qof_points %>%
  group_by(Indicator, Year) %>%
  summarise(avg = median(Value, na.rm = TRUE))

qof_england <- qof_points %>%
  group_by(Indicator, Year, IMD_quintile) %>%
  summarise(Value = median(Value, na.rm = TRUE)) %>%
  mutate(ICB.NAME = "England")

qof_agg <- qof_points %>%
  group_by(Indicator, Year, IMD_quintile, ICB.NAME) %>%
  summarise(Value = median(Value, na.rm = TRUE))

qof <- bind_rows(qof_england, qof_agg) %>%
  filter(!is.na(IMD_quintile)) %>%
  pivot_wider(
    names_from = IMD_quintile,
    values_from = Value,
    names_prefix = "quin_"
  ) %>%
  merge(., avg, by = c("Year", "Indicator"))

df <- bind_rows(df, qof)

write.csv(df, "final_data.csv", row.names = FALSE)

### Patient experience ---------------------------------------------------------
gpps <- read.csv("../data/satisfaction/satisfaction.csv") %>% select(-c(Practice.Code, Practice.Name))

avg <- gpps %>%
  group_by(Year) %>%
  summarise(
    continuity_pct = median(continuity_pct, na.rm = TRUE),
    access_pct = median(access_pct, na.rm = TRUE),
    overall_pct = median(overall_pct, na.rm = TRUE),
    trust_pct = median(trust_pct, na.rm = TRUE),
  ) %>%
  pivot_longer(
    cols = c(overall_pct, continuity_pct, access_pct, trust_pct),
    names_to = "Indicator",
    values_to = "avg"
  )

gpps_england <- gpps %>%
  group_by(Year, IMD_quintile) %>%
  summarise(
    continuity_pct = median(continuity_pct, na.rm = TRUE),
    access_pct = median(access_pct, na.rm = TRUE),
    overall_pct = median(overall_pct, na.rm = TRUE),
    trust_pct = median(trust_pct, na.rm = TRUE),
  ) %>%
  mutate(ICB.NAME = "England")

gpps_agg <- gpps %>%
  group_by(Year, IMD_quintile, ICB.NAME) %>%
  summarise(
    continuity_pct = median(continuity_pct, na.rm = TRUE),
    access_pct = median(access_pct, na.rm = TRUE),
    overall_pct = median(overall_pct, na.rm = TRUE),
    trust_pct = median(trust_pct, na.rm = TRUE),
  )

# Check if quintiles 1 and 5 are present in the data
quintile_1_present <- any(gpps_agg$IMD_quintile == 1)
quintile_5_present <- any(gpps_agg$IMD_quintile == 5)

gpps_edge_handled <- gpps_agg %>%
  # Then use the flags to conditionally replace quintiles
  mutate(
    IMD_quintile = case_when(
      IMD_quintile == 2 & !quintile_1_present ~ 1, # Replace quintile 2 with 1 if 1 is missing
      IMD_quintile == 4 & !quintile_5_present ~ 5, # Replace quintile 4 with 5 if 5 is missing
      TRUE ~ IMD_quintile # Otherwise, keep the original quintile
    )
  ) %>%
  ungroup()

gpps <- bind_rows(gpps_england, gpps_edge_handled)

gpps %<>%
  pivot_longer(
    cols = c(overall_pct, continuity_pct, access_pct, trust_pct),
    names_to = "Indicator",
    values_to = "Value"
  )

gpps %<>%
  filter(IMD_quintile %in% c(1, 5)) %>%
  pivot_wider(
    names_from = IMD_quintile,
    values_from = Value,
    names_prefix = "quin_"
  )

gpps <- merge(gpps, avg, by = c("Year", "Indicator"))

df <- bind_rows(df, gpps)

write.csv(df, "final_data.csv", row.names = FALSE)

### CQC ------------------------------------------------------------
cqc <- read.csv("../data/cqc/cqc.csv") %>% select(-c(Year, IMD, service_population_group, domain, inherited_rating_y_n, location_name))

avg <- cqc %>%
  filter(latest_rating %in% c("Good", "Outstanding")) %>%
  summarise(Value = n() / nrow(cqc))

cqc_england <- cqc %>%
  group_by(IMD_quintile) %>%
  summarise(Value = sum(latest_rating %in% c("Good", "Outstanding")) / n()) %>%
  mutate(ICB_NAME = "England")

cqc_agg <- cqc %>%
  group_by(IMD_quintile, ICB_NAME) %>%
  summarise(Value = sum(latest_rating %in% c("Good", "Outstanding")) / n())

# Check if quintiles 1 and 5 are present in the data
quintile_1_present <- any(cqc_agg$IMD_quintile == 1)
quintile_5_present <- any(cqc_agg$IMD_quintile == 5)

cqc_edge_handled <- cqc_agg %>%
  # Then use the flags to conditionally replace quintiles
  mutate(
    IMD_quintile = case_when(
      IMD_quintile == 2 & !quintile_1_present ~ 1, # Replace quintile 2 with 1 if 1 is missing
      IMD_quintile == 4 & !quintile_5_present ~ 5, # Replace quintile 4 with 5 if 5 is missing
      TRUE ~ IMD_quintile # Otherwise, keep the original quintile
    )
  ) %>%
  ungroup()

cqc <- bind_rows(cqc_england, cqc_edge_handled)

cqc %<>%
  filter(IMD_quintile %in% c(1, 5)) %>%
  pivot_wider(
    names_from = IMD_quintile,
    values_from = Value,
    names_prefix = "quin_"
  ) %>%
  mutate(avg = avg$Value) %>%
  mutate(Indicator = "cqc_rating") %>%
  mutate(Year = 2023) %>%
  rename(ICB.NAME = ICB_NAME)

df <- bind_rows(df, cqc)

write.csv(df, "final_data.csv", row.names = FALSE)

### Appointments ---------------------------------------------------------
appt <- read.csv("../data/appointments/appointments.csv")

n_w_patients <- payments[, c("Practice.Code", "Year", "Number.of.Weighted.Patients..Last.Known.Figure.")] %>%
  mutate(Year = ifelse(Year == 2023, 2024, Year))

appt <- merge(appt, n_w_patients, by = c("Year", "Practice.Code"))

appt_mar <- appt %>% filter(Month == 3)

avg <- appt_mar %>%
  mutate(APPT_MODE = case_when(
    APPT_STATUS == "DNA" ~ "DNA",
    TRUE ~ as.character(APPT_MODE)
  )) %>%
  group_by(Practice.Code, APPT_MODE) %>%
  summarise(
    count = sum(COUNT_OF_APPOINTMENTS),
    patients = unique(Number.of.Weighted.Patients..Last.Known.Figure.)
  ) %>%
  group_by(APPT_MODE) %>%
  summarise(
    count = sum(count),
    patients = sum(patients, na.rm = TRUE),
    avg = (count / patients) * 10000
  ) %>%
  select(-c(count, patients)) %>%
  rename(Indicator = APPT_MODE) %>%
  mutate(Year = 2024)

appt_england <- appt_mar %>%
  mutate(APPT_MODE = case_when(
    APPT_STATUS == "DNA" ~ "DNA",
    TRUE ~ as.character(APPT_MODE)
  )) %>%
  group_by(Practice.Code, APPT_MODE, IMD_quintile) %>%
  summarise(
    count = sum(COUNT_OF_APPOINTMENTS),
    patients = unique(Number.of.Weighted.Patients..Last.Known.Figure.)
  ) %>%
  group_by(APPT_MODE, IMD_quintile) %>%
  summarise(
    count = sum(count),
    patients = sum(patients, na.rm = TRUE),
    Value = (count / patients) * 10000
  ) %>%
  select(-c(count, patients)) %>%
  rename(Indicator = APPT_MODE) %>%
  mutate(Year = 2024, ICB_NAME = "England")

appt_agg <- appt_mar %>%
  mutate(APPT_MODE = case_when(
    APPT_STATUS == "DNA" ~ "DNA",
    TRUE ~ as.character(APPT_MODE)
  )) %>%
  group_by(Practice.Code, APPT_MODE, IMD_quintile, ICB_NAME) %>%
  summarise(
    count = sum(COUNT_OF_APPOINTMENTS),
    patients = unique(Number.of.Weighted.Patients..Last.Known.Figure.)
  ) %>%
  group_by(APPT_MODE, IMD_quintile, ICB_NAME) %>%
  summarise(
    count = sum(count),
    patients = sum(patients, na.rm = TRUE),
    Value = (count / patients) * 10000
  ) %>%
  select(-c(count, patients)) %>%
  rename(Indicator = APPT_MODE) %>%
  mutate(Year = 2024)

# Check if quintiles 1 and 5 are present in the data
quintile_1_present <- any(appt_agg$IMD_quintile == 1)
quintile_5_present <- any(appt_agg$IMD_quintile == 5)

appt_edge_handled <- appt_agg %>%
  # Then use the flags to conditionally replace quintiles
  mutate(
    IMD_quintile = case_when(
      IMD_quintile == 2 & !quintile_1_present ~ 1, # Replace quintile 2 with 1 if 1 is missing
      IMD_quintile == 4 & !quintile_5_present ~ 5, # Replace quintile 4 with 5 if 5 is missing
      TRUE ~ IMD_quintile # Otherwise, keep the original quintile
    )
  ) %>%
  ungroup()

appt <- bind_rows(appt_england, appt_edge_handled)

appt %<>%
  filter(IMD_quintile %in% c(1, 5)) %>%
  pivot_wider(
    names_from = IMD_quintile,
    values_from = Value,
    names_prefix = "quin_"
  ) %>%
  rename(ICB.NAME = ICB_NAME)

appt <- merge(appt, avg, by = c("Year", "Indicator"))

df <- bind_rows(df, appt)

write.csv(df, "final_data.csv", row.names = FALSE)

### Secondary care impact ---------------------------------------------------------
sec <- read.csv("../data/secondary_care/secondary_care.csv") %>%
  rename(ICB.NAME = ICB_NAME) %>%
  select(-c(Practice.Code, Area.Name, IMD)) %>%
  mutate(Value = as.numeric(Value))

avg <- sec %>%
  group_by(Year, Indicator) %>%
  summarise(avg = median(Value, na.rm = TRUE))

sec_england <- sec %>%
  group_by(Year, Indicator, IMD_quintile) %>%
  summarise(Value = median(Value, na.rm = TRUE)) %>%
  mutate(ICB.NAME = "England")

sec_agg <- sec %>%
  group_by(Year, Indicator, IMD_quintile, ICB.NAME) %>%
  summarise(Value = median(Value, na.rm = TRUE))

# Check if quintiles 1 and 5 are present in the data
quintile_1_present <- any(sec_agg$IMD_quintile == 1)
quintile_5_present <- any(sec_agg$IMD_quintile == 5)

sec_edge_handled <- sec_agg %>%
  # Then use the flags to conditionally replace quintiles
  mutate(
    IMD_quintile = case_when(
      IMD_quintile == 2 & !quintile_1_present ~ 1, # Replace quintile 2 with 1 if 1 is missing
      IMD_quintile == 4 & !quintile_5_present ~ 5, # Replace quintile 4 with 5 if 5 is missing
      TRUE ~ IMD_quintile # Otherwise, keep the original quintile
    )
  ) %>%
  ungroup()

sec <- bind_rows(sec_england, sec_edge_handled)

sec %<>%
  filter(IMD_quintile %in% c(1, 5)) %>%
  pivot_wider(
    names_from = IMD_quintile,
    values_from = Value,
    names_prefix = "quin_"
  )

sec <- merge(sec, avg, by = c("Year", "Indicator"))

df <- bind_rows(df, sec)

write.csv(df, "final_data.csv", row.names = FALSE)

### Render ---------------------------------------------------------
df <- read.csv("final_data.csv")

ICBs <- df$ICB.NAME %>% unique()
ICBs %>% length()

# Specify the folder name
folder_name <- "ICB Reports"

# Check if the folder exists
if (!dir.exists(folder_name)) {
  # Create the folder if it doesn't exist
  dir.create(folder_name)
  print(paste("Folder", folder_name, "created."))
} else {
  print(paste("Folder", folder_name, "already exists."))
}

for (i in 1:length(ICBs)) {
  rmarkdown::render(
    input = "slides.Rmd",
    output_file = str_glue("ICB Reports/{ICBs[i]}.pptx"),
    params = list(ICB_NAME = ICBs[i])
  )
}
