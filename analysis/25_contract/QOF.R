library(ggplot2)
library(tidyverse)
library(magrittr)
library(scales)
library(readxl)

# Load Excel file
excel <- "QOF_CVD.xlsx"

# Get all sheet names
sheets <- excel_sheets(excel)

# Identify the sheets containing CVD conditions (excluding metadata)
cvd_sheets <- sheets[3:length(sheets)] # Assuming first two sheets are metadata

# Define which sheets have prevalence and their respective column formats
sheets_with_prevalence <- c("AF", "CHD", "HF", "HYP", "PAD", "STIA") # Keep
sheets_bp <- c("BP") # BP needs List Size (45+)
sheets_drop <- c("CHOL", "LVSD") # Drop these

# Function to process each CVD sheet
process_cvd_sheet <- function(sheet_name) {
  df <- read_excel(excel, sheet = sheet_name, skip = 10) # Load data
  colnames(df) <- df[1, ] # Set first row as column names
  df <- df[-1, ] # Remove header row

  # Select only Prevalence & Achievement (and List Size for BP)
  if (sheet_name %in% sheets_with_prevalence) {
    df <- df[, c(6, 10, 18)] %>%
      rename(
        Practice.Code = `Practice code`,
        Prevalence = `Prevalence (%)`,
        Achievement = `Achievement (%)`
      )
  } else if (sheet_name %in% sheets_bp) { # BP uses List Size (45+) instead of Prevalence
    df <- df[, c(6, 9, 13)] %>%
      rename(
        Practice.Code = `Practice code`,
        Prevalence = `List size \r\naged 45+`, # Treat as Prevalence for BP
        Achievement = `Achievement (%)`
      )
  } else if (sheet_name == "STIA") { # STIA has different column numbers
    df <- df[, c(6, 15, 20)] %>%
      rename(
        Practice.Code = `Practice code`,
        Prevalence = `Prevalence (%)`,
        Achievement = `Achievement (%)`
      )
  }

  df <- df %>% mutate(CVD_domain = sheet_name) # Add CVD domain column
  return(df)
}

# Process all CVD sheets and combine them
df_all_cvd <- bind_rows(lapply(setdiff(cvd_sheets, sheets_drop), process_cvd_sheet))

# Process df_all_cvd with correct handling of BP, CHOL, and LVSD
df_filtered <- df_all_cvd %>%
  filter(!CVD_domain %in% c("CHOL", "LVSD")) %>% # Drop CHOL and LVSD
  mutate(
    Prevalence = as.numeric(Prevalence), # Use BP's List Size (45+) as Prevalence
    Achievement = as.numeric(Achievement)
  )

# Reshape to wide format
df_wide <- df_filtered %>%
  pivot_wider(
    names_from = CVD_domain,
    values_from = c(Achievement, Prevalence),
    names_sep = "_",
    values_fn = mean # Aggregate if duplicate Practice.Code entries exist
  ) %>%
  select(Practice.Code, sort(tidyselect::peek_vars())) # Order columns by domain

# View the cleaned dataset
print(df_wide)

### IMD
IMD <- read.csv("../../data/IMD/IMD_interpolated.csv") %>%
  filter(Year == 2024) %>%
  select(-Year) # Remove Year column if not needed

age <- read.csv("../../data/age_group/age.csv") %>%
  filter(Year == 2024) %>%
  select(-Year)

# Merge with df_wide
df_final <- df_wide %>%
  left_join(IMD, by = "Practice.Code") %>%
  mutate(IMD_quintile = ntile(IMD, 5)) %>%
  left_join(age, by = "Practice.Code")

# Achievment
df_final %>%
  group_by(IMD_quintile) %>%
  summarise(
    AF = mean(Achievement_AF, na.rm = TRUE),
    BP = mean(Achievement_BP, na.rm = TRUE),
    CHD = mean(Achievement_CHD, na.rm = TRUE),
    HF = mean(Achievement_HF, na.rm = TRUE),
    HYP = mean(Achievement_HYP, na.rm = TRUE),
    PAD = mean(Achievement_PAD, na.rm = TRUE),
    STIA = mean(Achievement_STIA, na.rm = TRUE)
  )

# Prevalence
prev_agg <- df_final %>%
  group_by(IMD_quintile) %>%
  summarise(
    AF = mean(Prevalence_AF, na.rm = TRUE),
    BP = mean(Prevalence_BP, na.rm = TRUE),
    CHD = mean(Prevalence_CHD, na.rm = TRUE),
    HF = mean(Prevalence_HF, na.rm = TRUE),
    HYP = mean(Prevalence_HYP, na.rm = TRUE),
    PAD = mean(Prevalence_PAD, na.rm = TRUE),
    STIA = mean(Prevalence_STIA, na.rm = TRUE)
  ) %>%
  pivot_longer(
    cols = -IMD_quintile,
    names_to = "Condition",
    values_to = "Rate"
  )

prev_agg %>%
  filter(IMD_quintile == 1 | IMD_quintile == 5) %>%
  filter(Condition == "AF" | Condition == "CHD" | Condition == "HF" | Condition == "STIA") %>%
  ggplot(aes(x = Rate, y = Condition, color = factor(IMD_quintile))) +
  geom_point(size = 4) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"), # Center title like nvd3
    panel.grid.minor = element_blank(),
    axis.title.y = element_blank(),
    axis.title.x = element_text(size = 16),
    axis.text.y = element_text(size = 16),
    legend.position = "bottom"
  ) +
  scale_color_manual(values = c("1" = "#EF7A34", "5" = "#A80026")) +
  scale_y_discrete(labels = c(
    "STIA" = "Stroke / TIA",
    "HF" = "Heart Failure",
    "CHD" = "Coronary Heart Disease",
    "AF" = "Atrial Fibrillation"
  )) +
  labs(
    title = "Prevalence of Select CVD Conditions by IMD Quintile",
    x = "Prevalence (%)",
    color = "IMD Quintile"
  )

ggsave("Prevalence.png", width = 15, height = 10, dpi = 100)


df_final %>%
  select(-Prevalence_BP) %>%
  filter(!is.na(IMD_quintile)) %>%
  group_by(IMD_quintile) %>%
  summarise(
    avg_achievement = mean(c_across(starts_with("Achievement")), na.rm = TRUE),
    avg_prevalence = mean(c_across(starts_with("Prevalence")), na.rm = TRUE),
    avg_prevalence = sd(c_across(starts_with("Prevalence")), na.rm = TRUE),
    avg_prop65 = mean(prop65, na.rm = TRUE)
  )

library(ggplot2)

df_final %>%
  filter(!is.na(IMD_quintile)) %>%
  group_by(IMD_quintile) %>%
  summarise(across(starts_with("Achievement"), mean, na.rm = TRUE)) %>%
  pivot_longer(cols = starts_with("Achievement"), names_to = "Domain", values_to = "Avg_Achievement") %>%
  ggplot(aes(x = as.factor(IMD_quintile), y = Avg_Achievement, fill = Domain)) +
  geom_col(position = "dodge") +
  labs(
    title = "Average Achievement (%) by IMD Quintile",
    x = "IMD Quintile (1 = Most Deprived, 5 = Least Deprived)",
    y = "Average Achievement (%)"
  ) +
  theme_minimal()

df_final %>%
  select(-Prevalence_BP) %>%
  filter(!is.na(IMD_quintile)) %>%
  group_by(IMD_quintile) %>%
  summarise(across(starts_with("Prevalence"), mean, na.rm = TRUE)) %>%
  pivot_longer(cols = starts_with("Prevalence"), names_to = "Domain", values_to = "Avg_Prevalence") %>%
  ggplot(aes(x = as.factor(IMD_quintile), y = Avg_Prevalence, fill = Domain)) +
  geom_col(position = "dodge") +
  labs(
    title = "Average Prevalence (%) by IMD Quintile",
    x = "IMD Quintile (1 = Most Deprived, 5 = Least Deprived)",
    y = "Average Prevalence (%)"
  ) +
  theme_minimal()

### Age
