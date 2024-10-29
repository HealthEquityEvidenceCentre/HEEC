pks <- c(
  "fingertipsR", "tidyverse", "purrr", "tibble",
  "spatstat", "lubridate", "readODS", "janitor", "patchwork"
)

sapply(pks, library, character.only = T)

"%ni%" <- Negate("%in%")

### Life expectancy ---------------------------------------------------------
le <- read.csv("../data/life_expectancy/life_expectancy.csv")

le <- le %>% filter(!is.na(IMD_quintile))

le_england <- le %>%
  group_by(Year, IMD_quintile) %>%
  summarise(
    ICB.NAME = "England",
    Life_Expectancy_Male = mean(Life_Expectancy_Male, na.rm = TRUE),
    Life_Expectancy_Female = mean(Life_Expectancy_Female, na.rm = TRUE)
  ) %>%
  ungroup()

# Step 1: Aggregate both male and female life expectancy
le_agg <- le %>%
  group_by(Year, ICB.NAME, IMD_quintile) %>%
  summarise(
    Life_Expectancy_Male = mean(Life_Expectancy_Male, na.rm = TRUE),
    Life_Expectancy_Female = mean(Life_Expectancy_Female, na.rm = TRUE)
  ) %>%
  ungroup()

# Step 3: Combine the regional data with the national aggregate for England
le_agg <- bind_rows(le_agg, le_england)

# Step 2: Pivot longer to create an 'Indicator' column for Male and Female
le_long <- le_agg %>%
  pivot_longer(
    cols = c(Life_Expectancy_Male, Life_Expectancy_Female),
    names_to = "Indicator",
    values_to = "Value"
  )

# Step 3: Calculate the average life expectancy per Year and Indicator
le_long <- le_long %>%
  group_by(Year, Indicator) %>%
  mutate(avg = median(Value, na.rm = TRUE)) %>%
  ungroup()

# for each indicator and year, calculate the correlation coefficient between the life expectancy and the IMD quintile
# corr <- le_long %>%
#   group_by(Year, ICB.NAME, Indicator) %>%
#   dplyr::summarise(cor(Value, IMD_quintile, use = "pairwise.complete.obs")) %>%
#   rename(corr_coeff = 4)

# Step 4: Handle edge cases by replacing quintile 2 with 1 (if 1 is missing) and 4 with 5 (if 5 is missing)
le_edge_handled <- le_long %>%
  group_by(Year, ICB.NAME, Indicator) %>%
  # First, we identify if quintile 1 or 5 is missing, and replace them with 2 or 4, respectively.
  mutate(
    IMD_quintile = case_when(
      IMD_quintile == 2 & !any(IMD_quintile == 1) ~ 1, # Replace quintile 2 with 1 if 1 is missing
      IMD_quintile == 4 & !any(IMD_quintile == 5) ~ 5, # Replace quintile 4 with 5 if 5 is missing
      TRUE ~ IMD_quintile # Otherwise, keep the original quintile
    )
  ) %>%
  ungroup()

# Step 5: Pivot wider, creating two columns for the selected quin 1 (or 2) and quin 5 (or 4)
le_wide <- le_edge_handled %>%
  filter(IMD_quintile %in% c(1, 5)) %>%
  pivot_wider(
    names_from = IMD_quintile,
    values_from = Value,
    names_prefix = "quin_"
  )

# where corr is greater than 0.2 or less than -0.2, set logic to TRUE
# le_wide <- le_wide %>%
#   left_join(corr) %>%
#   mutate(logic = case_when(
#     corr_coeff > 0.2 | corr_coeff < -0.2 ~ TRUE,
#     corr_coeff < 0.2 | corr_coeff > -0.2 ~ FALSE
#   ))

df <- le_wide

write.csv(df, "final_data.csv", row.names = FALSE)

### Payments to General Practice -----------------------------------
payments <- read.csv("../data/payments/payments.csv")

# All practices
payments <- payments %>% filter(!is.na(IMD_quintile))

payments_england <- payments %>%
  group_by(Year, IMD_quintile) %>%
  summarise(
    ICB.NAME = "England",
    total_payments_all = sum(Total.NHS.Payments.to.General.Practice, na.rm = TRUE),
    weighted_patients_all = sum(Number.of.Weighted.Patients..Last.Known.Figure., na.rm = TRUE),
  ) %>%
  ungroup() %>%
  mutate(
    payment_per_patient_all = total_payments_all / weighted_patients_all
  )

payments_agg <- payments %>%
  group_by(Year, ICB.NAME, IMD_quintile) %>%
  summarise(
    total_payments_all = sum(Total.NHS.Payments.to.General.Practice, na.rm = TRUE),
    weighted_patients_all = sum(Number.of.Weighted.Patients..Last.Known.Figure., na.rm = TRUE),
  ) %>%
  ungroup() %>%
  mutate(
    payment_per_patient_all = total_payments_all / weighted_patients_all
  )

payments_agg <- bind_rows(payments_agg, payments_england)

payments_long <- payments_agg %>%
  pivot_longer(
    cols = c(payment_per_patient_all, total_payments_all, weighted_patients_all),
    names_to = "Indicator",
    values_to = "Value"
  )

payments_long <- payments_long %>%
  group_by(Year, Indicator) %>%
  mutate(avg = median(Value, na.rm = TRUE)) %>%
  ungroup()

payments_edge_handled <- payments_long %>%
  group_by(Year, ICB.NAME, Indicator) %>%
  # First, we identify if quintile 1 or 5 is missing, and replace them with 2 or 4, respectively.
  mutate(
    IMD_quintile = case_when(
      IMD_quintile == 2 & !any(IMD_quintile == 1) ~ 1, # Replace quintile 2 with 1 if 1 is missing
      IMD_quintile == 4 & !any(IMD_quintile == 5) ~ 5, # Replace quintile 4 with 5 if 5 is missing
      TRUE ~ IMD_quintile # Otherwise, keep the original quintile
    )
  ) %>%
  ungroup()

payments_wide <- payments_edge_handled %>%
  filter(IMD_quintile %in% c(1, 5)) %>%
  pivot_wider(
    names_from = IMD_quintile,
    values_from = Value,
    names_prefix = "quin_"
  )

df <- bind_rows(df, payments_wide)

# Dispensing practices
payments_dispensing <- payments[payments$Dispensing.Practice == "Yes", ]

payments_england <- payments_dispensing %>%
  group_by(Year, IMD_quintile) %>%
  summarise(
    ICB.NAME = "England",
    total_payments_dispensing = sum(Total.NHS.Payments.to.General.Practice, na.rm = TRUE),
    weighted_patients_dispensing = sum(Number.of.Weighted.Patients..Last.Known.Figure., na.rm = TRUE),
  ) %>%
  ungroup() %>%
  mutate(
    payment_per_patient_dispensing = total_payments_dispensing / weighted_patients_dispensing
  )

payments_agg <- payments_dispensing %>%
  group_by(Year, ICB.NAME, IMD_quintile) %>%
  summarise(
    total_payments_dispensing = sum(Total.NHS.Payments.to.General.Practice, na.rm = TRUE),
    weighted_patients_dispensing = sum(Number.of.Weighted.Patients..Last.Known.Figure., na.rm = TRUE),
  ) %>%
  ungroup() %>%
  mutate(
    payment_per_patient_dispensing = total_payments_dispensing / weighted_patients_dispensing
  )

payments_agg <- bind_rows(payments_agg, payments_england)

payments_long <- payments_agg %>%
  pivot_longer(
    cols = c(payment_per_patient_dispensing, total_payments_dispensing, weighted_patients_dispensing),
    names_to = "Indicator",
    values_to = "Value"
  )

payments_long <- payments_long %>%
  group_by(Year, Indicator) %>%
  mutate(avg = median(Value, na.rm = TRUE)) %>%
  ungroup()

payments_edge_handled <- payments_long %>%
  group_by(Year, ICB.NAME, Indicator) %>%
  # First, we identify if quintile 1 or 5 is missing, and replace them with 2 or 4, respectively.
  mutate(
    IMD_quintile = case_when(
      IMD_quintile == 2 & !any(IMD_quintile == 1) ~ 1, # Replace quintile 2 with 1 if 1 is missing
      IMD_quintile == 4 & !any(IMD_quintile == 5) ~ 5, # Replace quintile 4 with 5 if 5 is missing
      TRUE ~ IMD_quintile # Otherwise, keep the original quintile
    )
  ) %>%
  ungroup()

payments_wide <- payments_edge_handled %>%
  filter(IMD_quintile %in% c(1, 5)) %>%
  pivot_wider(
    names_from = IMD_quintile,
    values_from = Value,
    names_prefix = "quin_"
  )

df <- bind_rows(df, payments_wide)

# Non-dispensing
payments_non_dispensing <- payments[payments$Dispensing.Practice == "No", ]

payments_england <- payments_non_dispensing %>%
  group_by(Year, IMD_quintile) %>%
  summarise(
    ICB.NAME = "England",
    total_payments_non_dispensing = sum(Total.NHS.Payments.to.General.Practice, na.rm = TRUE),
    weighted_patients_non_dispensing = sum(Number.of.Weighted.Patients..Last.Known.Figure., na.rm = TRUE),
  ) %>%
  ungroup() %>%
  mutate(
    payment_per_patient_non_dispensing = total_payments_non_dispensing / weighted_patients_non_dispensing
  )

payments_agg <- payments_non_dispensing %>%
  group_by(Year, ICB.NAME, IMD_quintile) %>%
  summarise(
    total_payments_non_dispensing = sum(Total.NHS.Payments.to.General.Practice, na.rm = TRUE),
    weighted_patients_non_dispensing = sum(Number.of.Weighted.Patients..Last.Known.Figure., na.rm = TRUE),
  ) %>%
  ungroup() %>%
  mutate(
    payment_per_patient_non_dispensing = total_payments_non_dispensing / weighted_patients_non_dispensing
  )

payments_agg <- bind_rows(payments_agg, payments_england)

payments_long <- payments_agg %>%
  pivot_longer(
    cols = c(payment_per_patient_non_dispensing, total_payments_non_dispensing, weighted_patients_non_dispensing),
    names_to = "Indicator",
    values_to = "Value"
  )

payments_long <- payments_long %>%
  group_by(Year, Indicator) %>%
  mutate(avg = median(Value, na.rm = TRUE)) %>%
  ungroup()

payments_edge_handled <- payments_long %>%
  group_by(Year, ICB.NAME, Indicator) %>%
  # First, we identify if quintile 1 or 5 is missing, and replace them with 2 or 4, respectively.
  mutate(
    IMD_quintile = case_when(
      IMD_quintile == 2 & !any(IMD_quintile == 1) ~ 1, # Replace quintile 2 with 1 if 1 is missing
      IMD_quintile == 4 & !any(IMD_quintile == 5) ~ 5, # Replace quintile 4 with 5 if 5 is missing
      TRUE ~ IMD_quintile # Otherwise, keep the original quintile
    )
  ) %>%
  ungroup()

payments_wide <- payments_edge_handled %>%
  filter(IMD_quintile %in% c(1, 5)) %>%
  pivot_wider(
    names_from = IMD_quintile,
    values_from = Value,
    names_prefix = "quin_"
  )

df <- bind_rows(df, payments_wide)

write.csv(df, "final_data.csv", row.names = FALSE)

### Workforce ---------------------------------------------------------
workforce <- read.csv("../data/workforce/workforce_year.csv") %>% select(-IMD)

workforce <- workforce %>% filter(!is.na(IMD_quintile))

workforce_england <- workforce %>%
  group_by(Year, IMD_quintile) %>%
  summarise(
    ICB.NAME = "England",
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
  )

workforce <- workforce %>% rename(ICB.NAME = ICB_NAME)

workforce_agg <- workforce %>%
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
  )

workforce_agg <- bind_rows(workforce_agg, workforce_england)

workforce_long <- workforce_agg %>%
  select(-TOTAL_PATIENTS) %>%
  pivot_longer(
    cols = c(TOTAL_GP_EXTGL_FTE, TOTAL_LOCUUM_TRN_FTE, TOTAL_NURSES_FTE, TOTAL_ADMIN_FTE, TOTAL_DPC_FTE),
    names_to = "Indicator",
    values_to = "Value"
  )

workforce_long <- workforce_long %>%
  group_by(Year, Indicator) %>%
  mutate(avg = median(Value, na.rm = TRUE)) %>%
  ungroup()

workforce_edge_handled <- workforce_long %>%
  group_by(Year, ICB.NAME, Indicator) %>%
  # First, we identify if quintile 1 or 5 is missing, and replace them with 2 or 4, respectively.
  mutate(
    IMD_quintile = case_when(
      IMD_quintile == 2 & !any(IMD_quintile == 1) ~ 1, # Replace quintile 2 with 1 if 1 is missing
      IMD_quintile == 4 & !any(IMD_quintile == 5) ~ 5, # Replace quintile 4 with 5 if 5 is missing
      TRUE ~ IMD_quintile # Otherwise, keep the original quintile
    )
  ) %>%
  ungroup()

workforce_wide <- workforce_edge_handled %>%
  filter(IMD_quintile %in% c(1, 5)) %>%
  pivot_wider(
    names_from = IMD_quintile,
    values_from = Value,
    names_prefix = "quin_"
  )

df <- bind_rows(df, workforce_wide)

write.csv(df, "final_data.csv", row.names = FALSE)

### PCN Workforce -----------------
pcn <- read.csv("../data/pcn_workforce/pcn_workforce.csv") %>% select(-IMD)

n_w_patients <- payments[, c("Practice.Code", "Year", "Number.of.Weighted.Patients..Last.Known.Figure.", "PCN.Name")] %>% rename(PCN_NAME = PCN.Name) %>% filter(Year == 2022) %>% group_by(Year, PCN_NAME) %>% summarise(Number.of.Weighted.Patients..Last.Known.Figure. = sum(Number.of.Weighted.Patients..Last.Known.Figure., na.rm = TRUE)) %>% filter(!is.na(PCN_NAME)) %>% mutate(Year = 2024)

pcn <- merge(pcn, n_w_patients, by = c("Year", "PCN_NAME"))

avg <- pcn %>% group_by(Year) %>% 
  summarise(
    Number.of.Weighted.Patients..Last.Known.Figure. = sum(Number.of.Weighted.Patients..Last.Known.Figure., na.rm = TRUE),
    Value = sum(FTE, na.rm = TRUE)
  ) %>%
  mutate(avg = Value / Number.of.Weighted.Patients..Last.Known.Figure. * 10000) %>% select(-c(Value,Number.of.Weighted.Patients..Last.Known.Figure.))

pcn_england <- pcn %>% group_by(Year, IMD_quintile) %>%
  summarise(
    Number.of.Weighted.Patients..Last.Known.Figure. = sum(Number.of.Weighted.Patients..Last.Known.Figure., na.rm = TRUE),
    Value = sum(FTE, na.rm = TRUE)
    ) %>% 
  mutate(ICB_NAME = "England",
         Value = Value / Number.of.Weighted.Patients..Last.Known.Figure. * 10000) %>% select(-Number.of.Weighted.Patients..Last.Known.Figure.)

pcn_agg <- pcn %>% group_by(Year, ICB_NAME, IMD_quintile) %>%
  summarise(
    Number.of.Weighted.Patients..Last.Known.Figure. = sum(Number.of.Weighted.Patients..Last.Known.Figure., na.rm = TRUE),
    Value = sum(FTE, na.rm = TRUE)
  ) %>% 
  mutate(
         Value = Value / Number.of.Weighted.Patients..Last.Known.Figure. * 10000) %>% select(-Number.of.Weighted.Patients..Last.Known.Figure.)

# Check if quintiles 1 and 5 are present in the data
quintile_1_present <- any(pcn_agg$IMD_quintile == 1)
quintile_5_present <- any(pcn_agg$IMD_quintile == 5)

pcn_edge_handled <- pcn_agg %>%
  # Then use the flags to conditionally replace quintiles
  mutate(
    IMD_quintile = case_when(
      IMD_quintile == 2 & !quintile_1_present ~ 1, # Replace quintile 2 with 1 if 1 is missing
      IMD_quintile == 4 & !quintile_5_present ~ 5, # Replace quintile 4 with 5 if 5 is missing
      TRUE ~ IMD_quintile # Otherwise, keep the original quintile
    )
  ) %>%
  ungroup()

pcn <- bind_rows(pcn_england, pcn_edge_handled)

pcn %<>%
  filter(IMD_quintile %in% c(1, 5)) %>%
  pivot_wider(
    names_from = IMD_quintile,
    values_from = Value,
    names_prefix = "quin_"
  ) %>% mutate(Indicator = "PCN_staff")

pcn <- merge(pcn, avg, by = "Year") %>% rename(ICB.NAME = ICB_NAME)

df <- bind_rows(df, pcn)

write.csv(df, "final_data.csv", row.names = FALSE)

### Behaviours ---------------------------------------------------------
#Smoking
prev <- read.csv("../data/behaviours/behaviours.csv")

prev %<>% select(-c(IMD, Area.Name, Practice.Code)) %>% rename(ICB.NAME = ICB_NAME)

avg <- prev %>% group_by(Year, Indicator) %>% summarise(avg = median(Value, na.rm = TRUE))

prev_england <- prev %>% group_by(Year, Indicator, IMD_quintile) %>% summarise(Value = median(Value, na.rm = TRUE)) %>% mutate(ICB.NAME = "England")

prev_agg <- prev %>% group_by(Year, Indicator, IMD_quintile, ICB.NAME) %>% summarise(Value = median(Value, na.rm = TRUE))

# Check if quintiles 1 and 5 are present in the data
quintile_1_present <- any(prev_agg$IMD_quintile == 1)
quintile_5_present <- any(prev_agg$IMD_quintile == 5)

prev_edge_handled <- prev_agg %>%
  # Then use the flags to conditionally replace quintiles
  mutate(
    IMD_quintile = case_when(
      IMD_quintile == 2 & !quintile_1_present ~ 1, # Replace quintile 2 with 1 if 1 is missing
      IMD_quintile == 4 & !quintile_5_present ~ 5, # Replace quintile 4 with 5 if 5 is missing
      TRUE ~ IMD_quintile # Otherwise, keep the original quintile
    )
  ) %>%
  ungroup()

prev <- bind_rows(prev_england, prev_edge_handled)

prev %<>%
  filter(IMD_quintile %in% c(1, 5)) %>%
  pivot_wider(
    names_from = IMD_quintile,
    values_from = Value,
    names_prefix = "quin_"
  )

prev <- merge(prev, avg, by = c("Year", "Indicator")) 

df <- bind_rows(df, prev)

write.csv(df, "final_data.csv", row.names = FALSE)

### Prevalence ---------------------------------------------------------


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
