library(dplyr)

wf <- read.csv("../../data/workforce/workforce_year.csv") %>%
  mutate(GP_partners = TOTAL_GP_SEN_PTNR_FTE + TOTAL_GP_PTNR_PROV_FTE)

wf %>%
  group_by(Year) %>%
  summarise(
    partners = sum(GP_partners, na.rm = TRUE),
    doctors = sum(TOTAL_GP_EXTGL_FTE, na.rm = TRUE)
  )
