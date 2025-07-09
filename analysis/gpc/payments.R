library(magrittr)
library(dplyr)
library(ggplot2)
library(tidyr)

payments23 <- read.csv("data/payments/raw/22-23.csv")
IMD <- read.csv("data/IMD/imd_raw.csv") %>%
  group_by(AreaCode) %>%
  filter(Year == max(Year)) %>%
  ungroup() %>%
  select(AreaCode, IMD = Value)

merged_data <- payments23 %>%
  left_join(IMD, by = c("Practice.Code" = "AreaCode"))

merged_data <- merged_data %>%
  mutate(IMD_quintile = ntile(IMD, 5)) %>%
  filter(Contract.Type %in% c("GMS"))

### QOF
QOF <- merged_data %>% select(Practice.Code, Number.of.Weighted.Patients..Last.Known.Figure., Total.QOF.Payments, Total.NHS.Payments.to.General.Practice.including.Covid.and.PCN.payments, IMD_quintile)

QOF_agg <- QOF %>%
  group_by(IMD_quintile) %>%
  summarise(Patients = sum(Number.of.Weighted.Patients..Last.Known.Figure., na.rm = TRUE), QOF = sum(Total.QOF.Payments), Total = sum(Total.NHS.Payments.to.General.Practice.including.Covid.and.PCN.payments)) %>%
  mutate(Average.QOF = QOF / Patients, Average.Total = Total / Patients, IMD_quintile = as.factor(IMD_quintile))

colors <- c("#EF7A34", "#00A865", "#007AA8", "#531A5C", "#A80026")

QOF_agg %>%
  filter(!is.na(IMD_quintile)) %>%
  ggplot(aes(x = IMD_quintile, y = Average.QOF, fill = IMD_quintile)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = colors) +
  labs(
    x = "IMD Quintile",
    y = "QOF Payment per Weighted Patient (£)",
    title = "QOF Payments per Weighted Patient by IMD Quintile, 2023 (England)",
    fill = "IMD Quintile"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    # legend.justification = "center",
    axis.text.x = element_text(size = 10),
    axis.title.x = element_text(size = 12),
    axis.line.x = element_line(size = 1),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    plot.title = element_text(hjust = 0.5)
  )

ggsave("analysis/gpc/qof_payments.png", width = 10, height = 7)

### Immunisations
merged_data %>% colnames()

Imm <- merged_data %>% select(Practice.Code, Number.of.Weighted.Patients..Last.Known.Figure., Influenza.and.Pneumococcal.Immunisations, IMD_quintile)

Imm_agg <- Imm %>%
  group_by(IMD_quintile) %>%
  summarise(Patients = sum(Number.of.Weighted.Patients..Last.Known.Figure., na.rm = TRUE), Imm = sum(Influenza.and.Pneumococcal.Immunisations)) %>%
  mutate(Average.Imm = Imm / Patients, IMD_quintile = as.factor(IMD_quintile))

colors <- c("#EF7A34", "#00A865", "#007AA8", "#531A5C", "#A80026")

Imm_agg %>%
  filter(!is.na(IMD_quintile)) %>%
  ggplot(aes(x = IMD_quintile, y = Average.Imm, fill = IMD_quintile)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = colors) +
  labs(
    x = "IMD Quintile",
    y = "Immunisation Payment per Weighted Patient (£)",
    title = "Immunisation Payments per Weighted Patient by IMD Quintile, 2023 (England)",
    fill = "IMD Quintile"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    # legend.justification = "center",
    axis.text.x = element_text(size = 10),
    axis.title.x = element_text(size = 12),
    axis.line.x = element_line(size = 1),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    plot.title = element_text(hjust = 0.5)
  )

ggsave("analysis/gpc/imm_payments.png", width = 10, height = 7)

### LIS
LIS <- merged_data %>% select(Practice.Code, Number.of.Weighted.Patients..Last.Known.Figure., Local.Incentive.Schemes, IMD_quintile)

LIS_agg <- LIS %>%
  group_by(IMD_quintile) %>%
  summarise(Patients = sum(Number.of.Weighted.Patients..Last.Known.Figure., na.rm = TRUE), LIS = sum(Local.Incentive.Schemes)) %>%
  mutate(Average.LIS = LIS / Patients, IMD_quintile = as.factor(IMD_quintile))

colors <- c("#EF7A34", "#00A865", "#007AA8", "#531A5C", "#A80026")

LIS_agg %>%
  filter(!is.na(IMD_quintile)) %>%
  ggplot(aes(x = IMD_quintile, y = Average.LIS, fill = IMD_quintile)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = colors) +
  labs(
    x = "IMD Quintile",
    y = "Local Incentive Scheme Income per Weighted Patient (£)",
    title = "Local Incentive Scheme Income per Weighted Patient by IMD Quintile, 2023 (England)",
    fill = "IMD Quintile"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    # legend.justification = "center",
    axis.text.x = element_text(size = 10),
    axis.title.x = element_text(size = 12),
    axis.line.x = element_line(size = 1),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    plot.title = element_text(hjust = 0.5)
  )

ggsave("analysis/gpc/LIS_payments.png", width = 10, height = 7)

### Premises
premises <- merged_data %>% select(Practice.Code, Number.of.Weighted.Patients..Last.Known.Figure., Premises.Payments, IMD_quintile)

premises_agg <- premises %>%
  group_by(IMD_quintile) %>%
  summarise(Patients = sum(Number.of.Weighted.Patients..Last.Known.Figure., na.rm = TRUE), Premises = sum(Premises.Payments)) %>%
  mutate(Average.Premises = Premises / Patients, IMD_quintile = as.factor(IMD_quintile))

colors <- c("#EF7A34", "#00A865", "#007AA8", "#531A5C", "#A80026")

premises_agg %>%
  filter(!is.na(IMD_quintile)) %>%
  ggplot(aes(x = IMD_quintile, y = Average.Premises, fill = IMD_quintile)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = colors) +
  labs(
    x = "IMD Quintile",
    y = "Premises Payments per Weighted Patient (£)",
    title = "Premises Payments per Weighted Patient by IMD Quintile, 2023 (England)",
    fill = "IMD Quintile"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    # legend.justification = "center",
    axis.text.x = element_text(size = 10),
    axis.title.x = element_text(size = 12),
    axis.line.x = element_line(size = 1),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    plot.title = element_text(hjust = 0.5)
  )

ggsave("analysis/gpc/premises_payments.png", width = 10, height = 7)

### Global Sum
merged_data$Contract.Type %>% unique()

global_sum <- merged_data %>% select(Practice.Code, Contract.Type, Number.of.Registered.Patients..Last.Known.Figure., Number.of.Weighted.Patients..Last.Known.Figure., Global.Sum, Total.NHS.Payments.to.General.Practice.including.Covid.and.PCN.payments, IMD_quintile)

# only include relevant practices
global_sum_agg <- global_sum %>%
  filter(Contract.Type %in% c("GMS")) %>%
  group_by(IMD_quintile) %>%
  summarise(Patients = sum(Number.of.Weighted.Patients..Last.Known.Figure., na.rm = TRUE), registered_patients = sum(Number.of.Registered.Patients..Last.Known.Figure., na.rm = TRUE), global_sum = sum(Global.Sum), Total = sum(Total.NHS.Payments.to.General.Practice.including.Covid.and.PCN.payments)) %>%
  mutate(Average.Global_sum = global_sum / Patients, Average.Registered = global_sum / registered_patients, Average.Total = Total / Patients, IMD_quintile = as.factor(IMD_quintile))


colors <- c("#EF7A34", "#00A865", "#007AA8", "#531A5C", "#A80026")

global_sum_agg %>%
  filter(!is.na(IMD_quintile)) %>%
  ggplot(aes(x = IMD_quintile, y = Average.Global_sum, fill = IMD_quintile)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = colors) +
  labs(
    x = "IMD Quintile",
    y = "Global Sum per Weighted Patient (£)",
    title = "Global Sum per Weighted Patient by IMD Quintile, 2023 (England), GMS only",
    fill = "IMD Quintile"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    # legend.justification = "center",
    axis.text.x = element_text(size = 10),
    axis.title.x = element_text(size = 12),
    axis.line.x = element_line(size = 1),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    plot.title = element_text(hjust = 0.5)
  )

ggsave("analysis/gpc/global_sum_payments.png", width = 10, height = 7)

library(tidyr)

global_sum_long <- global_sum_agg %>%
  select(IMD_quintile, Average.Global_sum, Average.Registered) %>%
  pivot_longer(
    cols = c(Average.Global_sum, Average.Registered),
    names_to = "Type",
    values_to = "Value"
  ) %>%
  mutate(
    Type = recode(Type,
      "Average.Global_sum" = "Per Weighted Patient",
      "Average.Registered" = "Per Registered Patient"
    )
  )

global_sum_long %>%
  filter(!is.na(IMD_quintile)) %>%
  ggplot(aes(x = IMD_quintile, y = Value, fill = Type)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.7), width = 0.6) +
  scale_fill_manual(values = c("Per Weighted Patient" = "#531A5C", "Per Registered Patient" = "#EF7A34")) +
  labs(
    x = "IMD Quintile",
    y = "Global Sum per Patient (£)",
    title = "Global Sum per Patient by IMD Quintile, 2023 (GMS only)",
    fill = NULL
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    axis.text.x = element_text(size = 10),
    axis.title.x = element_text(size = 12),
    axis.line.x = element_line(size = 1),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    plot.title = element_text(hjust = 0.5)
  )

ggsave("analysis/gpc/global_sum_payments2.png", width = 10, height = 7)

### Dispensing
dispensing <- merged_data %>%
  mutate(
    Dispensing_Total = Prescribing.Fee.Payments + Dispensing.Fee.Payments + Reimbursement.of.Drugs
  ) %>%
  group_by(IMD_quintile) %>%
  summarise(
    Patients = sum(Number.of.Weighted.Patients..Last.Known.Figure., na.rm = TRUE),
    Dispensing = sum(Dispensing_Total, na.rm = TRUE)
  ) %>%
  mutate(
    Average.Dispensing = Dispensing / Patients,
    IMD_quintile = as.factor(IMD_quintile)
  )

### Total
total_payments <- merged_data %>%
  group_by(IMD_quintile) %>%
  summarise(
    Patients = sum(Number.of.Weighted.Patients..Last.Known.Figure., na.rm = TRUE),
    Total = sum(Total.NHS.Payments.to.General.Practice.including.Covid.and.PCN.payments, na.rm = TRUE)
  ) %>%
  mutate(Average.Total = Total / Patients, IMD_quintile = as.factor(IMD_quintile))


### Stacked
# Join to get total payments
payments_with_total <- QOF_agg %>%
  select(IMD_quintile, QOF = Average.QOF) %>%
  left_join(Imm_agg %>% select(IMD_quintile, Immunisations = Average.Imm), by = "IMD_quintile") %>%
  left_join(LIS_agg %>% select(IMD_quintile, LIS = Average.LIS), by = "IMD_quintile") %>%
  left_join(premises_agg %>% select(IMD_quintile, Premises = Average.Premises), by = "IMD_quintile") %>%
  left_join(global_sum_agg %>% select(IMD_quintile, `Global Sum` = Average.Global_sum), by = "IMD_quintile") %>%
  left_join(dispensing %>% select(IMD_quintile, Dispensing = Average.Dispensing), by = "IMD_quintile") %>%
  left_join(total_payments %>% select(IMD_quintile, Average.Total), by = "IMD_quintile") %>%
  mutate(`All Other` = Average.Total - (QOF + Immunisations + LIS + Premises + `Global Sum` + Dispensing))

payments_long <- payments_with_total %>%
  select(-Average.Total) %>%
  pivot_longer(cols = -IMD_quintile, names_to = "Payment_Type", values_to = "Value")

payments_long$Payment_Type <- recode(payments_long$Payment_Type,
  "AllOther" = "All Other",
  "Imm" = "Immunisations",
  "GlobalSum" = "Global Sum"
)

payments_long$Payment_Type <- factor(
  payments_long$Payment_Type,
  levels = c("Dispensing (-84%)", "All Other, (5%)", "QOF (-15%)", "Immunisations (-39%)", "LIS (-2%)", "Premises (9%)", "Global Sum (-2%)")
)

colors_named <- c(
  "All Other, (5%)" = "#A80026",
  "Dispensing (-84%)" = "#F9C74F",
  "QOF (-15%)" = "#00A865",
  "Immunisations (-39%)" = "#999999",
  "LIS (-2%)" = "#531A5C",
  "Premises (9%)" = "#EF7A34",
  "Global Sum (-2%)" = "#007AA8"
)

payments_long %>%
  filter(!is.na(IMD_quintile)) %>%
  ggplot(aes(x = IMD_quintile, y = Value, fill = Payment_Type)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = colors_named) +
  labs(
    x = "IMD Quintile (Higher = More deprived)",
    y = "Payment per Weighted Patient (£)",
    fill = "Payment Type (% Difference (Q5 - Q1))",
    title = "Average GP Payments per Weighted Patient by IMD Quintile, 2023 (England), GMS only"
  ) +
  scale_y_continuous(limits = c(0, 200), breaks = seq(0, 250, 25)) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 10),
    axis.title.x = element_text(size = 12),
    axis.line.x = element_line(size = 1),
    panel.grid.minor = element_blank(),
    plot.title = element_text(hjust = 0.5)
  )

ggsave("analysis/gpc/stacked_payments_by_imd.png", width = 10, height = 7)

# Convert IMD_quintile to numeric if it's not
payments_long$IMD_quintile <- as.numeric(as.character(payments_long$IMD_quintile))

# Calculate Q5 - Q1 for each Payment Type
q_diff <- payments_long %>%
  filter(IMD_quintile %in% c(1, 5)) %>%
  pivot_wider(names_from = IMD_quintile, values_from = Value, names_prefix = "Q") %>%
  mutate(Difference = Q5 - Q1) %>%
  select(Payment_Type, Q1, Q5, Difference)

q_diff

q_diff <- q_diff %>%
  mutate(Percent_Diff = (Difference) / Q1 * 100)

q_diff %>%
  arrange(desc(abs(Percent_Diff))) %>%
  mutate(Percent_Diff = round(Percent_Diff, 0))


### Workforce
wf <- read.csv("data/workforce/workforce_year.csv")

wf_agg <- wf %>%
  group_by(Year, IMD_quintile) %>%
  summarise(
    TOTAL_PATIENTS = sum(TOTAL_PATIENTS, na.rm = TRUE),
    TOTAL_GP_EXTGL_FTE = sum(TOTAL_GP_EXTGL_FTE, na.rm = TRUE),
    TOTAL_GP_PER_10k = TOTAL_GP_EXTGL_FTE / TOTAL_PATIENTS * 10000
  )

wf_agg$IMD_quintile <- as.factor(wf_agg$IMD_quintile)
wf_agg$Year <- as.factor(wf_agg$Year)

colors <- c("#EF7A34", "#00A865", "#007AA8", "#531A5C", "#A80026")

wf_agg %>%
  filter(!is.na(IMD_quintile)) %>%
  ggplot(aes(x = Year, y = TOTAL_GP_PER_10k, group = IMD_quintile, color = IMD_quintile)) +
  geom_line(size = 1.5) +
  geom_point(size = 3) +
  labs(
    title = "Fully Qualified GPs FTE per 10,000 Patients by IMD Quintile",
    x = "Year",
    y = "Total GPs FTE"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    legend.justification = "center",
    axis.text.x = element_text(size = 10),
    axis.title.x = element_text(size = 12),
    axis.line.x = element_line(size = 1),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    plot.title = element_text(hjust = 0.5)
  ) +
  scale_color_manual(values = colors, labels = c("Q1 (least deprived)", "Q2", "Q3", "Q4", "Q5 (most deprived)")) +
  labs(color = "IMD quintile")

ggsave("analysis/gpc/workforce.png", width = 10, height = 7)


### QOF
qof <- read.csv("data/QOF/total_qof_points.csv")

qof_agg <- qof %>%
  group_by(Year, IMD_quintile) %>%
  summarise(
    QOF = mean(Value, na.rm = TRUE),
  )

qof_agg$IMD_quintile <- as.factor(qof_agg$IMD_quintile)

colors <- c("#EF7A34", "#00A865", "#007AA8", "#531A5C", "#A80026")

qof_agg %>%
  filter(!is.na(IMD_quintile)) %>%
  ggplot(aes(x = Year, y = QOF, group = IMD_quintile, color = IMD_quintile)) +
  geom_line(size = 1.5) +
  geom_point(size = 3) +
  labs(
    title = "Average QOF Points (% of max) by IMD quintile (England)",
    x = "Year",
    y = "QOF points (% of max)"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    legend.justification = "center",
    axis.text.x = element_text(size = 10),
    axis.title.x = element_text(size = 12),
    axis.line.x = element_line(size = 1),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    plot.title = element_text(hjust = 0.5)
  ) +
  scale_color_manual(values = colors, labels = c("Q1 (least deprived)", "Q2", "Q3", "Q4", "Q5 (most deprived)")) +
  labs(color = "IMD quintile")

ggsave("analysis/gpc/qof.png", width = 10, height = 7)


### GP Salaries
library(tibble)

income_df <- tribble(
  ~Year, ~`Bottom 10% (Salaried)`, ~`Median (Salaried)`, ~`Top 10% (Salaried)`,
  ~`Bottom 10% (Partner)`, ~`Median (Partner)`, ~`Top 10% (Partner)`,
  2016, 27700, 55900, 86800, 54700, 104900, 151900,
  2017, 27800, 56600, 89400, 56800, 109600, 158300,
  2018, 28200, 58400, 92900, 58200, 113400, 165500,
  2019, 29700, 60600, 95600, 62300, 117300, 177400,
  2020, 31800, 63600, 99000, 66400, 121800, 183900,
  2021, 32000, 64900, 101600, 77200, 142000, 215600,
  2022, 33900, 68000, 105900, 82200, 153400, 236300,
  2023, 33300, 69200, 109200, 72800, 140200, 216100
)

income_long <- income_df %>%
  pivot_longer(
    cols = -Year,
    names_to = c("Percentile", "PracticeType"),
    names_pattern = "(.*) \\((.*)\\)",
    values_to = "Income"
  )

# Filter for 2023 top and bottom percentiles
annotations <- income_long %>%
  filter(Year == 2023, Percentile %in% c("Top 10%", "Bottom 10%")) %>%
  mutate(
    label = paste(Percentile),
    # hjust = ifelse(PracticeType == "Partner", -0.1, 1.1),
    vjust = 2, 1.5
  )

ggplot(income_long, aes(x = Year, y = Income, color = PracticeType, linetype = Percentile)) +
  geom_line(size = 1) +
  scale_color_manual(values = c("Partner" = "#D55E00", "Salaried" = "#0072B2")) +
  scale_y_continuous(labels = scales::comma) +
  scale_linetype_manual(values = c(
    "Bottom 10%" = "dotted",
    "Median" = "solid",
    "Top 10%" = "dotted"
  )) +
  guides(linetype = "none") + # <- hides percentile legend
  geom_text(
    data = annotations,
    aes(label = label),
    angle = 90,
    # hjust = -0.2,
    # vjust = 0.5,
    size = 3,
    show.legend = FALSE
  ) +
  labs(
    title = "Partner vs Salaried GP Incomes Before Tax by Year",
    y = "Income before tax (£)",
    x = "Year",
    color = "Practice type",
    linetype = NULL # removes 'Percentile' title from legend
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 10),
    axis.title.x = element_text(size = 12),
    axis.line.x = element_line(size = 1),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    plot.title = element_text(hjust = 0.5),
    legend.position = "right"
  )

ggsave("analysis/gpc/salary.png", width = 10, height = 7)



### Inflation
library(tibble)

income_df <- tibble(
  Year = 2016:2023,
  Contractor = c(104900, 109600, 113400, 117300, 121800, 142000, 153400, 140200),
  Contractor_Adjusted = c(127100, 122900, 132300, 134000, 135900, 150300, 163700, 140200),
  Salaried = c(55900, 56600, 58400, 60600, 63600, 64900, 68000, 69200),
  Salaried_Adjusted = c(67639, 63392, 68328, 69084, 71232, 68794, 72760, 69200)
)

library(tidyr)

income_long <- income_df %>%
  pivot_longer(cols = -Year, names_to = "Series", values_to = "Income") %>%
  mutate(
    Series = recode(Series,
      "Contractor" = "Contractor (Nominal)",
      "Contractor_Adjusted" = "Contractor (Real)",
      "Salaried" = "Salaried (Nominal)",
      "Salaried_Adjusted" = "Salaried (Real)"
    )
  )

income_long <- income_long %>%
  mutate(
    Linetype = ifelse(grepl("Real", Series), "Real", "Nominal")
  )

library(ggplot2)
library(scales)

ggplot(income_long, aes(x = Year, y = Income, color = Series, linetype = Linetype)) +
  geom_line(size = 1) +
  scale_y_continuous(labels = comma) +
  scale_color_manual(values = c(
    "Contractor (Nominal)" = "#531A5C",
    "Contractor (Real)" = "#531A5C",
    "Salaried (Nominal)" = "#00A865",
    "Salaried (Real)" = "#00A865"
  )) +
  scale_linetype_manual(values = c("Real" = "solid", "Nominal" = "dotted")) +
  labs(
    title = "GP Income Before Tax: Contractor vs Salaried (Nominal vs Real)",
    y = "Income before tax (£)",
    x = "Year",
    color = NULL,
    linetype = NULL
  ) +
  theme_minimal() +
  guides(linetype = "none") + # <- hides percentile legend
  theme(
    axis.text.x = element_text(size = 10),
    axis.title.x = element_text(size = 12),
    axis.line.x = element_line(size = 1),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    plot.title = element_text(hjust = 0.5),
    legend.position = "right"
  )


ggsave("analysis/gpc/salary2.png", width = 10, height = 7)
