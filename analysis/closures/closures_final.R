library(magrittr)
library(dplyr)
library(googlesheets4)

# All practices open 2019/20
payments20 <- read.csv("../../data/payments/raw/19-20.csv")
payments20$Practice.Postcode <- toupper(gsub("[[:space:]]+", "", payments20$Practice.Postcode))

wf20 <- read.csv("../../data/workforce/raw/19_09.csv")

open20 <- unique(c(payments20$Practice.Code, wf20$PRAC_CODE))

# All practice open 2024/25
wf24 <- read.csv("../../data/workforce/raw/24_09.csv")

open24 <- unique(wf24$PRAC_CODE)

# How many that were in the 2019/20 payments/wf were no longer reported in wf24?
closed24 <- setdiff(open20, open24)

### Now let's try and get practice name and address
payments <- read.csv("../../data/payments/payments.csv")
payments$Practice.Postcode <- toupper(gsub("[[:space:]]+", "", payments$Practice.Postcode))

payments[payments$Practice.Code %in% closed24, ] %>% head()

closed_merged <- payments %>%
  filter(Practice.Code %in% closed24) %>%
  group_by(Practice.Code) %>%
  filter(Year == max(Year)) %>%
  select(Practice.Name, Practice.Address, Dispensing.Practice, Practice.Rurality) %>%
  distinct()

# Check against original version
# First, get all the practices I identified in my first analysis
sheet_id <- "1Z4s88OQTTxKvePag9ugUZantq3F9CRpJL0ASMX1Kkcc"

sheet_names <- sheet_names(sheet_id)

sheets_list <- lapply(sheet_names, function(sheet) {
  read_sheet(sheet_id, sheet = sheet) %>%
    mutate(Region = sheet)
})

combined_df <- bind_rows(sheets_list) # Add column for sheet names

closed_merged <- closed_merged %>%
  left_join(combined_df %>% select(`Practice Code`, `Merged with`, `Notes`, Outcome),
    by = c("Practice.Code" = "Practice Code")
  )

# Postcode check
# This is the postcodes of all the practices that are in the closed/merged list
# Extract postcodes of closed practices
postcodes_closed <- payments %>%
  filter(Practice.Code %in% closed24) %>%
  select(Practice.Code, Practice.Name, Practice.Postcode)

# Extract postcodes of open practices (those NOT in closed24)
postcodes_open <- payments %>%
  filter(!Practice.Code %in% closed24) %>%
  select(Practice.Code, Practice.Name, Practice.Postcode)

merged <- postcodes_closed[postcodes_closed$Practice.Postcode %in% postcodes_open$Practice.Postcode, ]$Practice.Code %>% unique()

merged_list <- new %>%
  filter(Practice.Code %in% merged) %>%
  pull(Practice.Code) %>%
  unique()

closed_merged <- closed_merged %>%
  mutate(Outcome = if_else(Practice.Code %in% merged_list, "Merged", Outcome))

write.csv(closed_merged, "closed_merged.csv", row.names = FALSE)
