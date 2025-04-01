files <- dir("../../data/appointments/raw", pattern = "*.csv")

appt <- files %>%
  map(~ read.csv(file.path("../../data/appointments/raw", .))) %>%
  bind_rows() %>%
  select(
    APPOINTMENT_MONTH_START_DATE, GP_CODE,
    APPT_MODE, APPT_STATUS,
    HCP_TYPE, COUNT_OF_APPOINTMENTS
  ) %>%
  rename(Practice.Code = GP_CODE) %>%
  mutate(APPOINTMENT_MONTH_START_DATE = dmy(APPOINTMENT_MONTH_START_DATE)) %>%
  mutate(Year = year(APPOINTMENT_MONTH_START_DATE)) %>%
  mutate(Month = month(APPOINTMENT_MONTH_START_DATE)) %>%
  select(-APPOINTMENT_MONTH_START_DATE)

appt %<>% filter(HCP_TYPE == "GP") %>% filter(Year == 2024)

appt %>% head()

IMD <- read.csv("../../data/IMD/IMD_interpolated.csv") %>%
  filter(Year == 2024) %>%
  select(-Year) # Remove Year column if not needed

appt %<>%
  left_join(IMD, by = "Practice.Code") %>% mutate(IMD_quintile = ntile(IMD, 5))

appt %<>% filter(APPT_MODE == "Video Conference/Online")

a <- appt %>%
  group_by(IMD_quintile) %>%
  summarise(total = sum(COUNT_OF_APPOINTMENTS))

b <- appt %>%
  filter(APPT_MODE == "Video Conference/Online") %>%
  group_by(IMD_quintile) %>%
  summarise(total = sum(COUNT_OF_APPOINTMENTS))

c <- a %>%
  left_join(b, by = "IMD_quintile") %>%
  mutate(access = total.x / total.y)

c %>%
  filter(!is.na(IMD_quintile)) %>%
  ggplot(aes(x = factor(IMD_quintile), y = access, fill = factor(IMD_quintile))) +
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
    y = "Proportion of Appointments that are Online (%)",
    title = "Proportion of Appointments that are Online by IMD Quintile",
    fill = "IMD quintile"
  ) +
  scale_fill_manual(
    values = colors,
    labels = c("Q1 (least deprived)", "Q2", "Q3", "Q4", "Q5 (most deprived)")
  ) +
  scale_x_discrete(
    labels = c("Q1 (least deprived)", "Q2", "Q3", "Q4", "Q5 (most deprived)")
  )

ggsave("access.png", width = 15, height = 10, dpi = 100)
