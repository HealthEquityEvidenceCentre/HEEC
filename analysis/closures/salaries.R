library(tibble)
library(ggplot2)
library(dplyr)
library(tidyr)
library(scales)

# Create the income data by practice size and year
practice_income <- tribble(
  ~Year, ~Size, ~Income,
  2018, "0-4,999", 106200,
  2019, "0-4,999", 107400,
  2020, "0-4,999", 110500,
  2021, "0-4,999", 130900,
  2022, "0-4,999", 137300,
  2023, "0-4,999", 128000,
  2018, "5,000-9,999", 112000,
  2019, "5,000-9,999", 115400,
  2020, "5,000-9,999", 119800,
  2021, "5,000-9,999", 138900,
  2022, "5,000-9,999", 148800,
  2023, "5,000-9,999", 136400,
  2018, "10,000-14,999", 115700,
  2019, "10,000-14,999", 119100,
  2020, "10,000-14,999", 124000,
  2021, "10,000-14,999", 143800,
  2022, "10,000-14,999", 155700,
  2023, "10,000-14,999", 141300,
  2018, "15,000-19,999", 118900,
  2019, "15,000-19,999", 123800,
  2020, "15,000-19,999", 129800,
  2021, "15,000-19,999", 147400,
  2022, "15,000-19,999", 156400,
  2023, "15,000-19,999", 142300,
  2018, "20,000+", 116200,
  2019, "20,000+", 124900,
  2020, "20,000+", 124500,
  2021, "20,000+", 152000,
  2022, "20,000+", 171800,
  2023, "20,000+", 154100
)

# Order factor levels
practice_income <- practice_income %>%
  mutate(Size = factor(Size, levels = c("20,000+", "15,000-19,999", "10,000-14,999", "5,000-9,999", "0-4,999")))

# Plot
ggplot(practice_income, aes(x = Year, y = Income, color = Size)) +
  geom_line(linewidth = 1) +
  geom_point() +
  scale_y_continuous(labels = comma) +
  labs(
    title = "GP Contractor Income Before Tax by Practice Size",
    y = "Income before tax (£)",
    x = "Year",
    color = "Practice Size"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.position = "right",
    panel.grid.minor.x = element_blank()
  )

ggsave("analysis/closures/salary_by_practice_size.png", width = 10, height = 7)


# REAL
# Create the income data by practice size and year
practice_income <- tribble(
  ~Year, ~Size, ~Income,
  2018, "0-4,999", 124254,
  2019, "0-4,999", 122436,
  2020, "0-4,999", 123760,
  2021, "0-4,999", 138754,
  2022, "0-4,999", 146911,
  2023, "0-4,999", 128000,
  2018, "5,000-9,999", 131040,
  2019, "5,000-9,999", 131556,
  2020, "5,000-9,999", 134176,
  2021, "5,000-9,999", 147234,
  2022, "5,000-9,999", 159216,
  2023, "5,000-9,999", 136400,
  2018, "10,000-14,999", 135369,
  2019, "10,000-14,999", 135774,
  2020, "10,000-14,999", 138880,
  2021, "10,000-14,999", 152428,
  2022, "10,000-14,999", 166599,
  2023, "10,000-14,999", 141300,
  2018, "15,000-19,999", 139113,
  2019, "15,000-19,999", 141132,
  2020, "15,000-19,999", 145376,
  2021, "15,000-19,999", 156244,
  2022, "15,000-19,999", 167348,
  2023, "15,000-19,999", 142300,
  2018, "20,000+", 135954,
  2019, "20,000+", 142386,
  2020, "20,000+", 139440,
  2021, "20,000+", 161120,
  2022, "20,000+", 183826,
  2023, "20,000+", 154100
)

# Order factor levels
practice_income <- practice_income %>%
  mutate(Size = factor(Size, levels = c("0-4,999", "5,000-9,999", "10,000-14,999", "15,000-19,999", "20,000+")))

ggplot(practice_income, aes(x = Year, y = Income, color = Size)) +
  geom_line(linewidth = 1) +
  geom_point() +
  scale_y_continuous(labels = comma) +
  labs(
    title = "GP Contractor Income Before Tax by Practice Size (Real)",
    y = "Income before tax (£, inflation-adjusted)",
    x = "Year",
    color = "Practice Size"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.position = "right",
    panel.grid.minor.x = element_blank()
  )

ggsave("analysis/closures/real_salary_by_practice_size.png", width = 10, height = 7)
