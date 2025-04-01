library(ggplot2)
library(tidyverse)
library(magrittr)
library(scales)

payments23 <- read.csv("../../data/payments/raw/22-23.csv")
payments23$Year <- 2023

payments23$Practice.Code %>%
  unique() %>%
  length()

t <- payments23 %>%
  group_by(Dispensing.Practice) %>%
  summarise(
    drugs = Reimbursement.of.Drugs %>% sum(),
    dispensing = Dispensing.Fee.Payments %>% sum(),
    prescribing = Prescribing.Fee.Payments %>% sum()
  )

payments23 %<>% filter(Dispensing.Practice != "Unknown")

payments23 %>%
  group_by(Dispensing.Practice) %>%
  summarise(n = n())

payments23$Practice.Code %>%
  unique() %>%
  length()

IMD <- read.csv("../../data/IMD/IMD_interpolated.csv") %>%
  filter(Year == 2023)

df <- merge(payments23, IMD, by = c("Practice.Code", "Year"))

df$Practice.Code %>%
  unique() %>%
  length()

df %<>%
  filter(Total.NHS.Payments.to.General.Practice > 0) %>%
  filter(Average.payments.per.registered.patient < 600) %>%
  filter(Number.of.Registered.Patients..Last.Known.Figure. != 0)

df$Practice.Code %>%
  unique() %>%
  length()

df[df$Dispensing.Practice == "Yes", ]$Dispensing.Practice <- "Dispensing"
df[df$Dispensing.Practice == "No", ]$Dispensing.Practice <- "Non-dispensing"

df[df$Dispensing.Practice == "Dispensing", ]$Number.of.Registered.Patients..Last.Known.Figure. %>% mean()
df[df$Dispensing.Practice == "Non-dispensing", ]$Number.of.Registered.Patients..Last.Known.Figure. %>% mean()

df %>%
  select(c("Dispensing.Practice", "IMD", "Average.payments.per.registered.patient")) %>%
  write.csv(., "nvd3_registered.csv", row.names = FALSE)

df %>%
  group_by(Dispensing.Practice) %>%
  mutate(average_payment = Total.NHS.Payments.to.General.Practice / Number.of.Registered.Patients..Last.Known.Figure.) %>%
  summarise(
    n = n(),
    IMDm = mean(IMD),
    IMDsd = sd(IMD),
    average.paymentm = mean(Average.payments.per.registered.patient.including.PCN.Workforce..Leadership.and.Support),
    average.paymentsd = sd(Average.payments.per.registered.patient),
    average_payment = mean(average_payment, na.rm = TRUE),
    total_payments = sum(Total.NHS.Payments.to.General.Practice),
    total_registered = sum(Number.of.Registered.Patients..Last.Known.Figure., na.rm = TRUE),
    total_weighted = sum(Number.of.Weighted.Patients..Last.Known.Figure.)
  ) %>%
  mutate(
    average_registered = total_payments / total_registered,
    average_weighted = total_payments / total_weighted
  ) %>%
  select(Dispensing.Practice, average_payment, average.paymentm, average_registered, average_weighted)


df %>%
  arrange(Dispensing.Practice == "Dispensing") %>%
  ggplot(aes(x = IMD, y = Average.payments.per.registered.patient, color = Dispensing.Practice)) +
  geom_point(alpha = 0.6, size = 2) +
  labs(
    title = "Scatter Plot of IMD vs. Payments per Registered Patient",
    x = "Index of Multiple Deprivation (IMD)",
    y = "Average Payments per Registered Patient",
    color = "Dispensing Practice"
  ) +
  scale_y_continuous(
    limits = c(0, 600),
    breaks = seq(0, 600, by = 50)
  ) +
  scale_x_continuous(
    limits = c(0, 70),
    breaks = seq(0, 70, by = 5)
  ) +
  scale_color_manual(values = c("Dispensing" = "#531A5C", "Non-dispensing" = "#EF7A34")) + # Match nvd3 colors
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"), # Center title like nvd3
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    panel.grid.major = element_line(color = "gray90"), # Light grid to match nvd3
    panel.grid.minor = element_blank(), # Remove minor grid
    legend.position = c(1, 1), # Top-right corner (just like nvd3)
    legend.justification = c(1, 1), # Right-align legend
    legend.title = element_blank(), # Remove legend title
    legend.text = element_text(size = 12)
  )

ggsave("payment.png", width = 15, height = 7.5, dpi = 300)

### Weighted
df %>%
  group_by(Dispensing.Practice) %>%
  summarise(
    n = n(),
    IMDm = mean(IMD),
    IMDsd = sd(IMD),
    average.paymentm = mean(Average.payments.per.weighted.patient),
    average.paymentsd = sd(Average.payments.per.weighted.patient)
  )

df %>%
  arrange(Dispensing.Practice == "Dispensing") %>%
  ggplot(aes(x = IMD, y = Average.payments.per.weighted.patient, color = Dispensing.Practice)) +
  geom_point(alpha = 0.6, size = 2) +
  labs(
    # title = "Scatter Plot of IMD vs. Payments per Weighted Patient",
    x = "IMD (Higher = More deprived)",
    y = "Payments per Weighted Patient (£)",
    color = "Dispensing Practice"
  ) +
  scale_y_continuous(
    limits = c(0, 600),
    breaks = seq(0, 600, by = 50)
  ) +
  scale_x_continuous(
    limits = c(0, 70),
    breaks = seq(0, 70, by = 5)
  ) +
  scale_color_manual(values = c("Dispensing" = "#531A5C", "Non-dispensing" = "#EF7A34")) + # Match nvd3 colors
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"), # Center title like nvd3
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    panel.grid.major = element_line(color = "gray90"), # Light grid to match nvd3
    panel.grid.minor = element_blank(), # Remove minor grid
    legend.position = c(1, 1), # Top-right corner (just like nvd3)
    legend.justification = c(1, 1), # Right-align legend
    legend.title = element_blank(), # Remove legend title
    legend.text = element_text(size = 12)
  )

ggsave("payment_weighted.png", width = 15, height = 7.5, dpi = 300)

### Prescribing
Prescribing <- c(
  "Prescribing.Fee.Payments", "Dispensing.Fee.Payments",
  "Reimbursement.of.Drugs"
)

df$Total.Prescribing <- rowSums(df[, Prescribing], na.rm = TRUE)

df$Reimbursement.of.Drugs %>% sum()
df[df$Dispensing.Practice == "Non-dispensing", ]$Reimbursement.of.Drugs %>% sum()
df[df$Dispensing.Practice == "Dispensing", ]$Reimbursement.of.Drugs %>% sum()

df$Dispensing.Fee.Payments %>% sum()
df[df$Dispensing.Practice == "Non-dispensing", ]$Dispensing.Fee.Payments %>% sum()
df[df$Dispensing.Practice == "Dispensing", ]$Dispensing.Fee.Payments %>% sum()

df$Prescribing.Fee.Payments %>% sum()
df[df$Dispensing.Practice == "Non-dispensing", ]$Prescribing.Fee.Payments %>% sum()
df[df$Dispensing.Practice == "Dispensing", ]$Prescribing.Fee.Payments %>% sum()

df$Total.NHS.Payments.to.General.Practice.minus.Prescribing <- df$Total.NHS.Payments.to.General.Practice - df$Total.Prescribing
df$Average.payments.minus.Prescribing.per.weighted.patient <- df$Total.NHS.Payments.to.General.Practice.minus.Prescribing / df$Number.of.Weighted.Patients..Last.Known.Figure.

df %>%
  group_by(Dispensing.Practice) %>%
  summarise(
    n = n(),
    IMDm = mean(IMD),
    IMDsd = sd(IMD),
    average.paymentm = mean(Average.payments.minus.Prescribing.per.weighted.patient, na.rm = TRUE),
    average.paymentsd = sd(Average.payments.minus.Prescribing.per.weighted.patient, na.rm = TRUE)
  )

df %>%
  arrange(Dispensing.Practice == "Dispensing") %>%
  ggplot(aes(x = IMD, y = Average.payments.minus.Prescribing.per.weighted.patient, color = Dispensing.Practice)) +
  geom_point(alpha = 0.6, size = 2) +
  labs(
    title = "",
    x = "IMD (Higher = More deprived)",
    y = "Payments (excluding prescribing) per Weighted Patient (£)",
    color = "Dispensing Practice"
  ) +
  scale_y_continuous(
    limits = c(0, 600),
    breaks = seq(0, 600, by = 50)
  ) +
  scale_x_continuous(
    limits = c(0, 70),
    breaks = seq(0, 70, by = 5)
  ) +
  scale_color_manual(values = c("Dispensing" = "#531A5C", "Non-dispensing" = "#EF7A34")) + # Match nvd3 colors
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"), # Center title like nvd3
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    panel.grid.major = element_line(color = "gray90"), # Light grid to match nvd3
    panel.grid.minor = element_blank(), # Remove minor grid
    legend.position = c(1, 1), # Top-right corner (just like nvd3)
    legend.justification = c(1, 1), # Right-align legend
    legend.title = element_blank(), # Remove legend title
    legend.text = element_text(size = 12)
  )

ggsave("payment_less_pres.png", width = 15, height = 7.5, dpi = 300)

### Workforce
workforce <- read.csv("../../data/workforce/workforce_year.csv") %>%
  select(-IMD) %>%
  filter(Year == 2023)

workforce %<>%
  left_join(payments23[, c("Practice.Code", "Dispensing.Practice", "Number.of.Weighted.Patients..Last.Known.Figure.")], by = "Practice.Code")

workforce %<>% select(c("Practice.Code", "Dispensing.Practice", "Number.of.Weighted.Patients..Last.Known.Figure.", "TOTAL_GP_EXTGL_FTE", "TOTAL_LOCUUM_TRN_FTE", "TOTAL_NURSES_FTE", "TOTAL_DPC_FTE", "TOTAL_ADMIN_FTE"))

agg <- workforce %>%
  group_by(Dispensing.Practice) %>%
  summarise(
    GP_per_100k = sum(TOTAL_GP_EXTGL_FTE, na.rm = TRUE) / sum(Number.of.Weighted.Patients..Last.Known.Figure., na.rm = TRUE) * 10000,
    Locum_per_100k = sum(TOTAL_LOCUUM_TRN_FTE, na.rm = TRUE) / sum(Number.of.Weighted.Patients..Last.Known.Figure., na.rm = TRUE) * 10000,
    Nurse_per_100k = sum(TOTAL_NURSES_FTE, na.rm = TRUE) / sum(Number.of.Weighted.Patients..Last.Known.Figure., na.rm = TRUE) * 10000,
    DPC_per_100k = sum(TOTAL_DPC_FTE, na.rm = TRUE) / sum(Number.of.Weighted.Patients..Last.Known.Figure., na.rm = TRUE) * 10000,
    Admin_per_100k = sum(TOTAL_ADMIN_FTE, na.rm = TRUE) / sum(Number.of.Weighted.Patients..Last.Known.Figure., na.rm = TRUE) * 10000,
  ) %>%
  pivot_longer(
    cols = -Dispensing.Practice,
    names_to = "Workforce_Metric",
    values_to = "Rate_per_100k"
  ) %>%
  mutate(
    Workforce_Metric = factor(Workforce_Metric, levels = c(
      "GP_per_100k", "Locum_per_100k",
      "Nurse_per_100k", "DPC_per_100k",
      "Admin_per_100k"
    )),
    Dispensing.Practice = factor(Dispensing.Practice,
      levels = c("No", "Yes"),
      labels = c("Non-dispensing", "Dispensing")
    )
  ) %>%
  filter(., is.na(Dispensing.Practice) == FALSE)

agg$Workforce_Metric <- factor(agg$Workforce_Metric,
  levels = c("Admin_per_100k", "DPC_per_100k", "Nurse_per_100k", "Locum_per_100k", "GP_per_100k"),
  labels = c("Admin", "DPC", "Nurses", "Locums", "GPs")
)

agg %>%
  ggplot(., aes(x = Rate_per_100k, y = Workforce_Metric, color = Dispensing.Practice)) +
  geom_point(size = 4) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"), # Center title like nvd3
    panel.grid.minor = element_blank(),
    axis.title.y = element_blank(),
    axis.title.x = element_text(size = 12),
    axis.text.y = element_text(size = 12),
    legend.position = "bottom"
  ) +
  scale_color_manual(values = c("Non-dispensing" = "#EF7A34", "Dispensing" = "#531A5C")) +
  labs(
    title = "",
    x = "Rate (FTE) per 10,000 Weighted Patients",
    color = "Practice Type"
  )

ggsave("workforce.png", width = 15, height = 7.5, dpi = 300)

### Bar chart
df %<>%
  mutate(IMD_quintile = ntile(IMD, 5))

df %>%
  group_by(IMD_quintile) %>%
  summarise(
    Average.payments.per.registered.patient = mean(Average.payments.per.registered.patient, na.rm = TRUE),
    Average.payments.per.weighted.patient = mean(Average.payments.per.weighted.patient, na.rm = TRUE)
  )

data <- df %>%
  group_by(IMD_quintile) %>%
  summarise(
    weighted = sum(Number.of.Weighted.Patients..Last.Known.Figure.),
    total = sum(Total.NHS.Payments.to.General.Practice),
    prescribing = sum(Total.Prescribing),
    global = sum(Global.Sum)
  ) %>%
  mutate(
    Total.per.weighted = total / weighted,
    less.prescribing.per.weighted = (total - prescribing) / weighted,
    less.global.per.weighted = (total - global) / weighted,
    less.per.weighted = (total - prescribing - global) / weighted,
    prescribing.per.weighted = Total.per.weighted - less.prescribing.per.weighted,
    global.per.weighted = Total.per.weighted - less.global.per.weighted
  ) %>%
  select(c(1, 9, 10, 11))

data_long <- data %>%
  tidyr::pivot_longer(
    cols = c(less.per.weighted, global.per.weighted, prescribing.per.weighted),
    names_to = "Category",
    values_to = "Value"
  ) %>%
  mutate(
    Category = factor(
      recode(Category,
        "prescribing.per.weighted" = "Prescribing",
        "global.per.weighted" = "Global sum",
        "less.per.weighted" = "Other"
      ),
      levels = c("Prescribing", "Global sum", "Other")
    )
  )

# Create the stacked bar chart
ggplot(data_long, aes(x = IMD_quintile, y = Value, fill = Category)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c("Other" = "#007AA8", "Prescribing" = "#EF7A34", "Global sum" = "#00A865")) +
  labs(
    x = "IMD Quintile (Higher = More deprived)",
    y = "Payment per weighted patient (£)",
    fill = "Category",
    title = ""
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    legend.justification = "center",
    axis.text.x = element_text(size = 10),
    axis.title.x = element_text(size = 12),
    axis.line.x = element_line(size = 1), # Adjust x-axis line thickness
    panel.grid.minor.x = element_blank(), # Remove minor vertical grid lines
    panel.grid.minor.y = element_blank(), # Remove minor horizontal grid lines
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"), # Center title like nvd3
  )

ggsave("payments_bar.png", width = 15, height = 9, dpi = 300)
