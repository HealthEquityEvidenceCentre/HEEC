library(magrittr)
library(dplyr)
library(scales)
library(ggplot2)

df <- read.csv("../dispensing_git/data/payments/payments.csv")
df23 <- df[df$Year == 2023, ]

df23$Practice.Code %>%
  unique() %>%
  length()

dispensing23 <- df[df$Dispensing.Practice == "Yes" & df$Year == 2023, ]
non_dispensing23 <- df[df$Dispensing.Practice == "No" & df$Year == 2023, ]

dispensing23$Practice.Code %>%
  unique() %>%
  length()

non_dispensing23$Practice.Code %>%
  unique() %>%
  length()

# Payments
agg_dispensing <- df23 %>%
  filter(
    (IMD_decile == 1 | IMD_decile == 5) &
      (Dispensing.Practice == "No" | Dispensing.Practice == "Yes") &
      !is.na(IMD_decile)
  ) %>%
  group_by(IMD_decile, Dispensing.Practice) %>%
  summarise(
    total_payments = sum(Total.NHS.Payments.to.General.Practice, na.rm = TRUE),
    weighted_patients = sum(Number.of.Weighted.Patients..Last.Known.Figure., na.rm = TRUE),
    .groups = "drop" # to ungroup after summarise
  ) %>%
  mutate(
    payment_per_patient = total_payments / weighted_patients
  )

# Create additional rows for all practices in IMD_decile 1 and 5
additional_rows <- bind_rows(
  data.frame(
    IMD_decile = "1",
    Dispensing.Practice = "All",
    total_payments = sum(df23[df23$IMD_decile == 1, ]$Total.NHS.Payments.to.General.Practice, na.rm = TRUE),
    weighted_patients = sum(df23[df23$IMD_decile == 1, ]$Number.of.Weighted.Patients..Last.Known.Figure., na.rm = TRUE)
  ) %>%
    mutate(
      payment_per_patient = total_payments / weighted_patients
    ),
  data.frame(
    IMD_decile = "5",
    Dispensing.Practice = "All",
    total_payments = sum(df23[df23$IMD_decile == 5, ]$Total.NHS.Payments.to.General.Practice, na.rm = TRUE),
    weighted_patients = sum(df23[df23$IMD_decile == 5, ]$Number.of.Weighted.Patients..Last.Known.Figure., na.rm = TRUE)
  ) %>%
    mutate(
      payment_per_patient = total_payments / weighted_patients
    )
)

agg_dispensing$IMD_decile %<>% as.factor()

# Combine the aggregated data with the additional rows
agg_dispensing <- bind_rows(agg_dispensing, additional_rows)

# Replace the "All" Dispensing.Practice with "All Practices"
agg_dispensing$Dispensing.Practice[agg_dispensing$Dispensing.Practice == "All"] <- "All practices"
agg_dispensing$Dispensing.Practice[agg_dispensing$Dispensing.Practice == "Yes"] <- "Dispensing Practices"
agg_dispensing$Dispensing.Practice[agg_dispensing$Dispensing.Practice == "No"] <- "Non-dispensing Practices"

# Update the factor levels for Dispensing.Practice
agg_dispensing$Dispensing.Practice <- factor(agg_dispensing$Dispensing.Practice,
  levels = c("Dispensing Practices", "Non-dispensing Practices", "All practices")
)

ggplot(agg_dispensing, aes(x = payment_per_patient, y = Dispensing.Practice, color = as.factor(IMD_decile))) +
  geom_point(size = 5) +
  scale_color_manual(values = c("5" = "#A80026", "1" = "#1B2C57"), labels = c("Least deprived (Q1)", "Most deprived (Q5)")) +
  labs(
    title = "Total Payment per Weighted Patient, 2022/23",
    subtitle = "Practices in Most and Least Deprived IMD Quintiles",
    x = "Payment per Patient (£)",
    y = "Dispensing Practice",
    color = "IMD Quintile"
  ) +
  theme_minimal(base_size = 15) +
  theme(
    plot.title = element_text(size = 18, face = "bold"),
    plot.subtitle = element_text(size = 14),
    axis.title.y = element_blank(),
    axis.text.x = element_text(size = 10),
    legend.position = "bottom",
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 8),
    legend.key.size = unit(0.5, "cm"),
    panel.grid.major = element_line(color = "grey80"),
    panel.grid.minor = element_line(color = "grey90"),
    plot.margin = margin(5, 5, 5, 5) # Adjust margin to make space for the legend
  )

# Save the plot to a file
ggsave("dispensing/charts/median_payments.png", width = 10, height = 5)


# Size
dispensing23$Number.of.Registered.Patients..Last.Known.Figure. %>%
  sum()

dispensing23$Number.of.Registered.Patients..Last.Known.Figure. %>%
  mean()

# Rurality
table(dispensing23$Practice.Rurality)
table(non_dispensing23$Practice.Rurality)

table(df23[df23$Practice.Rurality == "Rural", ]$Dispensing.Practice)
df23[df23$Practice.Rurality == "Rural", ]$Practice.Code %>%
  unique() %>%
  length()

table(df23[df23$Practice.Rurality == "Rural", ]$IMD_decile)

# Earnings
df <- read.csv("GP_earnings/GP_earn_exp.csv")

# Dipsensing/Non-dispensing
df1 <- df[df$Region == "All" & df$Practice.type == "All" & df$Rurality == "All" & df$Size == "All", ]
df2 <- df[df$Region == "All" & df$Practice.type == "Dispensing" & df$Rurality == "All" & df$Size == "All", ]
df3 <- df[df$Region == "All" & df$Practice.type == "Non-dispensing" & df$Rurality == "All" & df$Size == "All", ]

ggplot() +
  geom_line(data = df1, aes(x = Year, y = Income.before.tax..contractor., colour = "All"), size = 1) +
  geom_line(data = df2, aes(x = Year, y = Income.before.tax..contractor., colour = "Dispensing"), size = 1.5) +
  geom_line(data = df3, aes(x = Year, y = Income.before.tax..contractor., colour = "Non-dispensing"), size = 1.5) +
  labs(title = "", x = "Year", y = "Income before tax (£)", colour = "Practice type") +
  scale_colour_manual(values = c("All" = "black", "Dispensing" = "#531A5C", "Non-dispensing" = "#EF7A34")) +
  theme_minimal() +
  scale_y_continuous(labels = comma) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"), # Center title like nvd3
    legend.position = "bottom",
    legend.justification = "center", # Adjust as needed
    axis.text.x = element_text(size = 12),
    axis.title.x = element_text(size = 14),
    axis.line.x = element_line(size = 1), # Adjust x-axis line thickness
    axis.text.y = element_text(size = 12),
    axis.title.y = element_text(size = 14),
    panel.grid.minor.x = element_blank(), # Remove minor vertical grid lines
    panel.grid.minor.y = element_blank(), # Remove minor horizontal grid lines
    legend.text = element_text(size = 10),
  )

ggsave("dispensing_git/analysis/dispensing/earnings.png", width = 15, height = 7.5, dpi = 300)

# Satisfaction
df <- read.csv("satisfaction/satisfaction.csv")
df23 <- df[df$Year == 2023, ]

df23[df23$Practice.Code %in% dispensing23$Practice.Code, ]$overall_pct %>%
  mean()

df23[df23$Practice.Code %in% non_dispensing23$Practice.Code, ]$overall_pct %>%
  mean(na.rm = TRUE)

# IMD
# show the distribution of IMD quintiles, as a percentage of the total number of practices in each group
table(dispensing23$IMD_decile)
table(non_dispensing23$IMD_decile)

# visualise the distribution of IMD deciles
library(patchwork)

# Define the colors vector as provided
colors <- c("#EF7A34", "#00A865", "#007AA8", "#531A5C", "#A80026")

# Custom labels for the legend
custom_labels <- c(
  "1" = "Q1 (least deprived)",
  "2" = "Q2",
  "3" = "Q3",
  "4" = "Q4",
  "5" = "Q5 (most deprived)"
)


dispensing_plot <- ggplot(dispensing23, aes(x = as.factor(IMD_decile), fill = as.factor(IMD_decile))) +
  geom_bar() +
  scale_fill_manual(values = colors, labels = custom_labels) +
  labs(
    title = "Dispensing Practices",
    # x = "IMD Quintile",
    y = "Number of Practices",
    fill = "IMD Quintile"
  ) +
  theme_minimal(base_size = 15) +
  theme(
    plot.title = element_text(size = 18, face = "bold"),
    axis.title.x = element_blank(),
    axis.title.y = element_text(size = 14),
    axis.text.x = element_blank(), # Adjust margin to reduce space
    axis.text.y = element_text(size = 12),
    legend.position = "right",
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10),
    panel.grid.major.x = element_blank(), # Remove major vertical grid lines
    panel.grid.minor.x = element_blank(), # Remove minor vertical grid lines
    # panel.grid.major = element_line(color = "grey80"),
    # panel.grid.minor = element_line(color = "grey90")
  )

non_dispensing_plot <- ggplot(non_dispensing23[!is.na(non_dispensing23$IMD_decile), ], aes(x = as.factor(IMD_decile), fill = as.factor(IMD_decile))) +
  geom_bar() +
  scale_fill_manual(values = colors, labels = custom_labels) +
  labs(
    title = "Non-dispensing Practices",
    y = "Number of practices",
    fill = "IMD Quintile"
  ) +
  theme_minimal(base_size = 15) +
  theme(
    plot.title = element_text(size = 18, face = "bold"),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x = element_blank(), # Adjust margin to reduce space
    axis.text.y = element_text(size = 12),
    legend.position = "right",
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10),
    panel.grid.major.x = element_blank(), # Remove major vertical grid lines
    panel.grid.minor.x = element_blank(), # Remove minor vertical grid lines
    # panel.grid.major = element_line(color = "grey80"),
    # panel.grid.minor = element_line(color = "grey90"),
  )

# Combine the two plots using patchwork
combined_plot <- dispensing_plot + non_dispensing_plot +
  plot_layout(guides = "collect") & theme(legend.position = "bottom")

# Display the combined plot
print(combined_plot)

ggsave("dispensing/charts/imd_distribution.png", width = 8, height = 8, dpi = 300)

# total "Prescribing.Fee.Payments", "Dispensing.Fee.Payments", "Reimbursement.of.Drugs" for each group
dispensing_total <- sum(dispensing23$Prescribing.Fee.Payments) + sum(dispensing23$Dispensing.Fee.Payments) + sum(dispensing23$Reimbursement.of.Drugs)
non_dispensing_total <- sum(non_dispensing23$Prescribing.Fee.Payments) + sum(non_dispensing23$Dispensing.Fee.Payments) + sum(non_dispensing23$Reimbursement.of.Drugs)

dispensing_total / sum(dispensing23$Total.NHS.Payments.to.General.Practice) * 100
non_dispensing_total / sum(non_dispensing23$Total.NHS.Payments.to.General.Practice) * 100

# Total payments gradient by dispensing vs non-dispensing
dispensing <- df[df$Dispensing.Practice == "Yes", ]
non_dispensing <- df[df$Dispensing.Practice == "No", ]

agg_dispensing <- dispensing %>%
  group_by(Year, IMD_decile) %>%
  summarise(
    total_payments = sum(Total.NHS.Payments.to.General.Practice),
    weighted_patients = sum(Number.of.Weighted.Patients..Last.Known.Figure.)
  ) %>%
  mutate(
    payment_per_patient = total_payments / weighted_patients
  )

agg_dispensing$IMD_decile %<>% as.factor()

colors <- c("#A80026", "#531A5C", "#1B2C57", "#007AA8", "#00A865", "#EF7A34")

agg_dispensing[!is.na(agg_dispensing$IMD_decile), ] %>%
  ggplot(aes(x = Year, y = payment_per_patient, group = IMD_decile, color = IMD_decile)) +
  geom_line(size = 1.5) +
  geom_point(size = 3) +
  labs(x = "", y = "Average payment (£)", title = "Total NHS payments per weighted patient by IMD quintile (dispensing practices)", color = "IMD quintile") +
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
  scale_x_continuous(breaks = unique(agg_dispensing$Year))

ggsave("dispensing/charts/dispensing_payments.png", width = 10, height = 6, dpi = 300)

agg_non_dispensing <- non_dispensing %>%
  group_by(Year, IMD_decile) %>%
  summarise(
    total_payments = sum(Total.NHS.Payments.to.General.Practice),
    weighted_patients = sum(Number.of.Weighted.Patients..Last.Known.Figure.)
  ) %>%
  mutate(
    payment_per_patient = total_payments / weighted_patients
  )

agg_non_dispensing$IMD_decile %<>% as.factor()

agg_non_dispensing[!is.na(agg_non_dispensing$IMD_decile), ] %>%
  ggplot(aes(x = Year, y = payment_per_patient, group = IMD_decile, color = IMD_decile)) +
  geom_line(size = 1.5) +
  geom_point(size = 3) +
  labs(x = "", y = "Average payment (£)", title = "Total NHS payments per weighted patient by IMD quintile (non-dispensing practices)", color = "IMD quintile") +
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
  scale_x_continuous(breaks = unique(agg_non_dispensing$Year))

ggsave("dispensing/charts/non_dispensing_payments.png", width = 10, height = 6, dpi = 300)

# Count the number of dispensing practices in each ICB
# Group and count practices in each ICB
n_prac <- df23 %>%
  group_by(ICB.NAME) %>%
  summarise(
    n_prac = n(),
    total_payments = sum(Total.NHS.Payments.to.General.Practice, na.rm = TRUE),
    weighted_patients = sum(Number.of.Weighted.Patients..Last.Known.Figure., na.rm = TRUE),
  ) %>%
  mutate(
    payment_per_patient = total_payments / weighted_patients
  )

# Group and count dispensing practices in each ICB
n_disp_prac <- dispensing23 %>%
  group_by(ICB.NAME) %>%
  summarise(n_disp_prac = n(), .groups = "drop")

# Merge dataframes and calculate percentage
n_prac <- n_prac[, c("ICB.NAME", "n_prac", "payment_per_patient")] %>%
  left_join(n_disp_prac, by = "ICB.NAME") %>%
  mutate(
    n_disp_prac = coalesce(n_disp_prac, 0),
    n_prac_perc = (n_disp_prac / n_prac) * 100
  ) %>%
  arrange(desc(n_prac_perc))

n_prac %>% print(n = 42)

### For each ICB, calculate the total prescribing payments
# Group and sum prescribing payments for each ICB, and calculate percentage of Total.NHS.Payments.to.General.Practice
prescribing_payments <- df23 %>%
  group_by(ICB.NAME) %>%
  summarise(
    prescribing_payments = sum(Total.Prescribing),
    total_payments = sum(Total.NHS.Payments.to.General.Practice),
    prescribing_payments_perc = (prescribing_payments / total_payments) * 100
  ) %>%
  arrange(desc(prescribing_payments_perc))

n_prac <- merge(n_prac, prescribing_payments[, c("ICB.NAME", "prescribing_payments_perc")], by = "ICB.NAME")

write.csv(n_prac, "n_prac.csv", row.names = FALSE)
