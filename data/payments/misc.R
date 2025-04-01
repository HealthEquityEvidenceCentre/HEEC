library(dplyr)
library(magrittr)
library(tidyr)

df <- read.csv("payments.csv")

Prescribing <- c(
  "Prescribing.Fee.Payments", "Dispensing.Fee.Payments",
  "Reimbursement.of.Drugs"
)

# Dataframe 1: Filtered for a specific practice
df_practice <- df %>%
  filter(Practice.Code == "F81009") %>%
  select(all_of(c("Year", Prescribing, "Number.of.Weighted.Patients..Last.Known.Figure."))) %>%
  mutate(
    Prescribing.Total = rowSums(across(all_of(Prescribing)), na.rm = TRUE),
    Prescribing.per.patient = Prescribing.Total / Number.of.Weighted.Patients..Last.Known.Figure.
  )

# Dataframe 2: Summary statistics for dispensing practices
df_dispensing_summary <- df %>%
  filter(Dispensing.Practice == "Yes") %>%
  select(all_of(c("Year", Prescribing, "Number.of.Weighted.Patients..Last.Known.Figure."))) %>%
  mutate(across(all_of(Prescribing), as.numeric)) %>% # Convert columns to numeric
  group_by(Year) %>%
  summarise(across(all_of(Prescribing), ~ mean(.x, na.rm = TRUE), .names = "Mean_{.col}"), Mean_Number.of.Weighted.Patients..Last.Known.Figure. = mean(Number.of.Weighted.Patients..Last.Known.Figure., na.rm = TRUE)) %>%
  mutate(
    Mean_Prescribing.Total = rowSums(across(all_of(c("Mean_Prescribing.Fee.Payments", "Mean_Dispensing.Fee.Payments", "Mean_Reimbursement.of.Drugs")))),
    Mean_Prescribing.per.patient = Mean_Prescribing.Total / Mean_Number.of.Weighted.Patients..Last.Known.Figure.
  )

# Combine both dataframes into one
df_combined <- left_join(df_practice, df_dispensing_summary, by = "Year")

# View the combined dataframe
head(df_combined)

write.csv(df_combined, "F81009_prescribing.csv", row.names = FALSE)

library(ggplot2)
library(scales)

colors <- c("#EF7A34", "#00A865", "#007AA8", "#531A5C", "#A80026")

# Improved line chart
ggplot(df_combined, aes(x = Year)) +
  geom_line(aes(y = Prescribing.per.patient, color = "Practice (F81009)"), size = 1.2) +
  geom_line(aes(y = Mean_Prescribing.per.patient, color = "Mean Dispensing Practices"),
    size = 1.2, linetype = "dashed"
  ) +
  scale_color_manual(values = c("Practice (F81009)" = colors[1], "Mean Dispensing Practices" = colors[2])) +
  labs(
    title = "Comparison of Prescribing per Patient Over Time",
    x = "Year",
    y = "Prescribing per Patient",
    color = "Legend"
  ) +
  theme_minimal(base_size = 14) +
  theme(legend.position = "top")

ggsave("prescribing1.png")

print(p1)

library(tidyr)
# Filter for 2023
df_2023 <- df_combined %>% filter(Year == 2023)

# Reshape data for plotting
df_2023_long <- df_2023 %>%
  select(
    Prescribing.Fee.Payments, Dispensing.Fee.Payments, Reimbursement.of.Drugs,
    Prescribing.Total
  ) %>%
  pivot_longer(cols = everything(), names_to = "Category", values_to = "Practice_F81009") %>%
  mutate(
    Mean_Value = c(
      df_2023$Mean_Prescribing.Fee.Payments,
      df_2023$Mean_Dispensing.Fee.Payments,
      df_2023$Mean_Reimbursement.of.Drugs,
      df_2023$Mean_Prescribing.Total
    )
  ) %>%
  pivot_longer(cols = c(Practice_F81009, Mean_Value), names_to = "Type", values_to = "Value")

# Fix labels
df_2023_long$Type <- recode(df_2023_long$Type,
  "Practice_F81009" = "Practice (F81009)",
  "Mean_Value" = "Mean Dispensing Practices"
)

ggplot(df_2023_long, aes(x = Category, y = Value, fill = Type)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = c(colors[3], colors[4], colors[5])) +
  scale_y_continuous(labels = label_dollar(prefix = "£", accuracy = 1)) +
  labs(
    title = "Comparison of Values by Category and Type",
    x = "Category",
    y = "Value (£)",
    fill = "Type"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "top"
  )

ggsave("prescribing1.png")
