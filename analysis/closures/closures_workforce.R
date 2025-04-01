library(magrittr)
library(dplyr)

# All practices open 2019/20
payments20 <- read.csv("../../data/payments/raw/19-20.csv")
payments20$Practice.Postcode <- toupper(gsub("[[:space:]]+", "", payments20$Practice.Postcode))

wf20 <- read.csv("../../data/workforce/raw/19_09.csv")

open20 <- unique(c(payments20$Practice.Code, wf20$PRAC_CODE))

# All practice open 2022/23
payments23 <- read.csv("../../data/payments/raw/22-23.csv")
payments23$Practice.Postcode <- toupper(gsub("[[:space:]]+", "", payments23$Practice.Postcode))

wf24 <- read.csv("../../data/workforce/raw/24_09.csv")

open23 <- unique(c(payments23$Practice.Code, wf24$PRAC_CODE))

closed_list <- setdiff(open20, open23)
setdiff(open23, open20) %>% length()
closed_list %>% length()
closed_list %>% sort()

### Postcodes
payments20[payments20$Practice.Code %in% closed_list & payments20$Practice.Postcode %in% payments23$Practice.Postcode, ] %>%
  select(Practice.Code, Practice.Postcode) %>%
  nrow()

# 412 closed practices. 101 have the same postcode but different practice name
setdiff(open20, unique(combined_df$`Practice Code`))

setdiff(setdiff(open20, wf24$PRAC_CODE), unique(combined_df$`Practice Code`))

sheet_id <- "1Z4s88OQTTxKvePag9ugUZantq3F9CRpJL0ASMX1Kkcc"

sheet_names <- sheet_names(sheet_id)

sheets_list <- lapply(sheet_names, function(sheet) {
  read_sheet(sheet_id, sheet = sheet) %>%
    mutate(Region = sheet)
})

combined_df <- bind_rows(sheets_list) # Add column for sheet names

combined_df$`Practice Code`
