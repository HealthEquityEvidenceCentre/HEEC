payments23 <- read.csv("../../data/payments/raw/22-23.csv")
IMD <- read.csv("../../data/IMD/IMD_interpolated.csv") %>% filter(Year == 2023)

# Merge the provided data with IMD data
df <- merge(payments23, IMD, by = "Practice.Code", all.x = TRUE) %>% mutate(IMD_quintile = ntile(IMD, 5))

merged_df <- df %>% select(
  Practice.Code, IMD_quintile,
  Total.QOF.Payments,
  Total.Locum.Allowances,
  Appraisal...Appraiser.Costs.in.Respect.of.Locums,
  Childhood.Vaccination.and.Immunisation.Scheme, # just include this one
  Non.DES.Item.Pneumococcal.Vaccine..Childhood.Immunisation.Main.Programme,
  Covid.Immunisation,
  Total.NHS.Payments.to.General.Practice,
  Number.of.Registered.Patients..Last.Known.Figure.
)

payments_agg <- merged_df %>%
  group_by(IMD_quintile) %>%
  filter(!is.na(IMD_quintile)) %>%
  summarise(
    Total_patients = sum(Number.of.Registered.Patients..Last.Known.Figure.),
    Total_payments = sum(Total.NHS.Payments.to.General.Practice),
    Total_locum_payments = sum(
      Total.Locum.Allowances,
      Appraisal...Appraiser.Costs.in.Respect.of.Locums
    ),
    Total_childhood = sum(Childhood.Vaccination.and.Immunisation.Scheme),
    Total_vaccination = sum(
      Childhood.Vaccination.and.Immunisation.Scheme,
      Non.DES.Item.Pneumococcal.Vaccine..Childhood.Immunisation.Main.Programme,
      Covid.Immunisation
    ),
    Total_QOF = sum(Total.QOF.Payments)
  ) %>%
  mutate(
    Average_payments = Total_payments / Total_patients,
    Average_locum = Total_locum_payments / Total_patients,
    Average_vaccination = Total_vaccination / Total_patients,
    Average_childhood = Total_childhood / Total_patients,
    Average_QOF = Total_QOF / Total_patients
  ) %>%
  select(IMD_quintile, Average_QOF, Average_locum, Average_vaccination, Average_childhood, Average_payments)

# Plot
colors <- c("#EF7A34", "#00A865", "#007AA8", "#531A5C", "#A80026")

payments_agg %>%
  filter(!is.na(IMD_quintile)) %>%
  ggplot(aes(x = factor(IMD_quintile), y = Average_QOF, fill = factor(IMD_quintile))) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"), # Center title like nvd3
    legend.position = "none",
    axis.text.y = element_text(size = 16),
    axis.title.y = element_text(size = 18),
    axis.text.x = element_text(size = 16),
    axis.title.x = element_text(size = 18),
    axis.line.x = element_line(size = 1),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank()
  ) +
  labs(
    x = "IMD Quintile",
    y = "QOF payment per registered patient(£)",
    title = "Average QOF Payments per Registered Patient by IMD Quintile",
    fill = "IMD quintile"
  ) +
  scale_fill_manual(
    values = colors,
    labels = c("Q1 (least deprived)", "Q2", "Q3", "Q4", "Q5 (most deprived)")
  ) +
  scale_x_discrete(
    labels = c("Q1 (least deprived)", "Q2", "Q3", "Q4", "Q5 (most deprived)")
  )

ggsave("QOF_per_patient.png", width = 15, height = 10, dpi = 100)

payments_agg %>%
  filter(!is.na(IMD_quintile)) %>%
  ggplot(aes(x = factor(IMD_quintile), y = Average_childhood, fill = factor(IMD_quintile))) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"), # Center title like nvd3
    legend.position = "none",
    axis.text.y = element_text(size = 16),
    axis.title.y = element_text(size = 18),
    axis.text.x = element_text(size = 16),
    axis.title.x = element_text(size = 18),
    axis.line.x = element_line(size = 1),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank()
  ) +
  labs(
    x = "IMD Quintile",
    y = "Childhood vaccination payments per registered patient(£)",
    title = "Average Childhood Vaccination Payments per Registered Patient by IMD Quintile",
    fill = "IMD quintile"
  ) +
  scale_fill_manual(
    values = colors,
    labels = c("Q1 (least deprived)", "Q2", "Q3", "Q4", "Q5 (most deprived)")
  ) +
  scale_x_discrete(
    labels = c("Q1 (least deprived)", "Q2", "Q3", "Q4", "Q5 (most deprived)")
  ) +
  scale_y_continuous(
    labels = label_number(accuracy = 0.01)
  )

ggsave("vaccination_per_patient.png", width = 15, height = 10, dpi = 100)

payments_agg %>%
  filter(!is.na(IMD_quintile)) %>%
  ggplot(aes(x = factor(IMD_quintile), y = Average_locum, fill = factor(IMD_quintile))) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"), # Center title like nvd3
    legend.position = "none",
    axis.text.y = element_text(size = 16),
    axis.title.y = element_text(size = 18),
    axis.text.x = element_text(size = 16),
    axis.title.x = element_text(size = 18),
    axis.line.x = element_line(size = 1),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank()
  ) +
  labs(
    x = "IMD Quintile",
    y = "Locum payments per registered patient(£)",
    title = "Average Locum Payments per Registered Patient by IMD Quintile",
    fill = "IMD quintile"
  ) +
  scale_fill_manual(
    values = colors,
    labels = c("Q1 (least deprived)", "Q2", "Q3", "Q4", "Q5 (most deprived)")
  ) +
  scale_x_discrete(
    labels = c("Q1 (least deprived)", "Q2", "Q3", "Q4", "Q5 (most deprived)")
  )

ggsave("locum_per_patient.png", width = 15, height = 10, dpi = 100)


### Workforce
file_list <- list.files("../../data/workforce/raw", full.names = TRUE)
fy_files <- file_list[grepl("22_0[4-9]|22_1[0-2]|23_0[1-3]\\.csv$", file_list)]

df_list <- list()

# Iterate through the files
for (file in fy_files) {
  df <- read.csv(file)
  print(paste("Loaded:", file)) # Print file name for tracking progress

  # Select relevant columns and compute locums
  df <- df %>%
    select(PRAC_CODE, TOTAL_GP_FTE, TOTAL_GP_HC, TOTAL_GP_EXL_FTE, TOTAL_GP_EXL_HC) %>%
    mutate(
      TOTAL_GP_FTE = as.numeric(TOTAL_GP_FTE),
      TOTAL_GP_HC = as.numeric(TOTAL_GP_HC),
      TOTAL_GP_EXL_FTE = as.numeric(TOTAL_GP_EXL_FTE),
      TOTAL_GP_EXL_HC = as.numeric(TOTAL_GP_EXL_HC),
      TOTAL_LOCUMS_FTE = TOTAL_GP_FTE - TOTAL_GP_EXL_FTE,
      TOTAL_LOCUMS_HC = TOTAL_GP_HC - TOTAL_GP_EXL_HC
    )

  # Store in list
  df_list[[file]] <- df
}

# Combine all data into one dataframe
full_data <- bind_rows(df_list)

library(DescTools)

full_data %<>%
  group_by(PRAC_CODE) %>%
  summarise(
    TOTAL_GP_FTE = mean(TOTAL_GP_FTE, na.rm = TRUE),
    TOTAL_GP_HC = mean(TOTAL_GP_HC),
    TOTAL_LOCUMS_FTE = mean(TOTAL_LOCUMS_FTE, na.rm = TRUE),
    TOTAL_LOCUMS_HC = mean(TOTAL_LOCUMS_HC)
  )

merged_wf <- merge(full_data, merged_df, by.x = "PRAC_CODE", by.y = "Practice.Code", all.x = TRUE)

wf_agg <- merged_wf %>%
  filter(!is.na(IMD_quintile)) %>%
  group_by(IMD_quintile) %>%
  summarise(
    Total_patients = sum(Number.of.Registered.Patients..Last.Known.Figure.),
    Total_GP_FTE = sum(TOTAL_GP_FTE, na.rm = TRUE),
    Total_GP_HC = sum(TOTAL_GP_HC, na.rm = TRUE),
    Total_locum_FTE = sum(TOTAL_LOCUMS_FTE, na.rm = TRUE),
    Total_locum_HC = sum(TOTAL_LOCUMS_HC, na.rm = TRUE)
  ) %>%
  mutate(
    GP_FTE_per_10k = Total_GP_FTE / Total_patients * 100000,
    GP_HC_per_10k = Total_GP_HC / Total_patients * 100000,
    locum_FTE_per_10k = Total_locum_FTE / Total_patients * 100000,
    locum_HC_per_10k = Total_locum_HC / Total_patients * 100000
  ) %>%
  select(IMD_quintile, GP_FTE_per_10k, GP_HC_per_10k, locum_FTE_per_10k, locum_HC_per_10k, Total_patients)

t <- merge(payments_agg, wf_agg, by = "IMD_quintile", all.x = TRUE)

pcn <- read.csv("../../data/pcn_workforce/pcn_workforce.csv") %>% filter(Year == 2024)

t <- merge(wf_agg, pcn %>%
  group_by(IMD_quintile) %>%
  summarise(total.pcn = sum(FTE)), by = "IMD_quintile") %>%
  mutate(PCN_FTE_per_10k = total.pcn / Total_patients * 100000)
