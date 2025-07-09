library(magrittr)
library(dplyr)

earnings <- data.frame()

for (file in list.files("GP_earn_exp")[1:5]) {
    print(file)
    earnings <- read.csv(paste0("GP_earn_exp/", file))
    print(head(earnings))
}

data %>% colnames()

for (i in 1:15) {
    colnames(data)[i] %>% print()
    data[, i] %>%
        unique() %>%
        print()
    print("   ")
}

# aggregate payments data
df <- read.csv("nhs_payments/nhs_payments.csv")

# need to get payments including COVID and PCN payments from payments
# it's OK up to 2021. I'm then gonna hard code collection for 2022 and 2021

# payments2022 <- read.csv("nhs_payments/raw/21-22.csv")
# # drop "Total.NHS.Payments.to.General.Practice.Minus.Deductions"
# payments2022 <- payments2022[, -57]
# colnames(payments2022)[60] <- "Total.NHS.Payments.to.General.Practice.Minus.Deductions"
# payments2022$Year <- 2022

# df <- df[df$Year != 2022, ]
# df <- bind_rows(df, payments2022)

# payments2021 <- read.csv("nhs_payments/raw/20-21.csv")
# payments2021[, 52] %>% sum()
# payments2021[, 55] %>% sum()
# payments2021[, 56] %>% sum()

# payments2021$total_covid <- payments2021[, 55] - payments2021[, 52]
# payments2021$total_pcn <- payments2021[, 56] - payments2021[, 52]

# payments2021$total_covid %>% sum()
# payments2021$total_pcn %>% sum()
# payments2021$Total.NHS.Payments.to.General.Practice.including.Covid.and.PCN.payments.minus.deductions <- payments2021$Total.NHS.Payments.to.General.Practice + payments2021$total_covid + payments2021$total_pcn - payments2021$Deductions.for.Pensions..Levies.and.Prescription.Charge.Income

# payments2021 <- payments2021[-54]
# colnames(payments2021)[70] <- "Total.NHS.Payments.to.General.Practice.Minus.Deductions"
# payments2021$Year <- 2021

# df <- df[df$Year != 2021, ]
# df <- bind_rows(df, payments2021)

library(dplyr)

df %>%
    group_by(Year) %>%
    summarise(total = mean(Total.NHS.Payments.to.General.Practice.Minus.Deductions))

# Non-dispensing practices have 0 for dispensing fee payments
# select rows where dispensing fee payments are 0
n <- df[df$Dispensing.Fee.Payments == 0, c(1, 48, 47, 41, 42, 43)]
n %>%
    group_by(Year) %>%
    summarise(total = mean(Total.NHS.Payments.to.General.Practice.Minus.Deductions))

m <- df[df$Dispensing.Fee.Payments != 0, c(1, 48, 47, 41, 42, 43)]
m %>%
    group_by(Year) %>%
    summarise(total = mean(Total.NHS.Payments.to.General.Practice.Minus.Deductions))

# workforce
wf <- read.csv("workforce/workforce.csv")

# combine the two dataframes by Practice.Code and Year
merged_df <- merge(df, wf)


merged_df$TOTAL_GP_SEN_PTNR_HC %<>% as.numeric()
merged_df$TOTAL_GP_PTNR_PROV_HC %<>% as.numeric()
merged_df$TOTAL_N_NURSE_PTNR_HC %<>% as.numeric()
merged_df$TOTAL_ADMIN_MANAGE_PTNR_HC %<>% as.numeric()

merged_df$Total.PTNR.HC <- merged_df$TOTAL_GP_SEN_PTNR_HC + merged_df$TOTAL_GP_PTNR_PROV_HC + merged_df$TOTAL_N_NURSE_PTNR_HC + merged_df$TOTAL_ADMIN_MANAGE_PTNR_HC
merged_df$Payment_per_PTNR <- merged_df$Total.NHS.Payments.to.General.Practice.Minus.Deductions / merged_df$Total.PTNR.HC

payment_per_ptnr <- merged_df[merged_df$Total.PTNR.HC != 0, ] %>%
    group_by(Year) %>%
    summarise(total = mean(Payment_per_PTNR, na.rm = TRUE))

payment_per_ptnr <- payment_per_ptnr[-9, ]

# calculate year on year prcentage change in payments per patient
payment_per_ptnr$change <- c(NA, diff(payment_per_ptnr$total) / lag(payment_per_ptnr$total) * 100)

## checks
merged_df[merged_df$Total.PTNR.HC == 0, ]

###

df %>%
    group_by(Year) %>%
    summarise(total = mean(Total.PTNR.HC, na.rm = TRUE))

x <- df[df$Dispensing.Fee.Payments == 0, ]
x %>%
    group_by(Year) %>%
    summarise(total = mean(Total.PTNR.HC, na.rm = TRUE))

y <- df[df$Dispensing.Fee.Payments != 0, ]
y %>%
    group_by(Year) %>%
    summarise(total = mean(Total.PTNR.HC, na.rm = TRUE))


#### workforce
workforce <- data.frame()

for (file in list.files("workforce/raw")[1:7]) {
    print(file)
    df <- read.csv(paste0("workforce/raw/", file))[, c("PRAC_CODE", "PRAC_NAME", "CCG_CODE", "CCG_NAME", "REGION_NAME", "TOTAL_PATIENTS", "TOTAL_GP_SEN_PTNR_HC", "MALE_GP_SEN_PTNR_HC", "MALE_GP_PTNR_PROV_HC", "FEMALE_GP_SEN_PTNR_HC", "FEMALE_GP_PTNR_PROV_HC", "TOTAL_GP_PTNR_PROV_HC", "TOTAL_GP_SEN_PTNR_FTE", "TOTAL_GP_PTNR_PROV_FTE", "TOTAL_GP_EXTG_HC", "TOTAL_GP_EXTG_FTE", "TOTAL_NURSES_FTE", "TOTAL_DPC_FTE")]
    df <- df %>% rename(., Practice.Code = PRAC_CODE)

    year <- substr(file, 1, nchar(file) - 4)
    year <- substr(year, 6, nchar(year))
    print(year)
    df$Year <- year %>% as.numeric()

    workforce <- rbind(workforce, df)
}

df22 <- read.csv("workforce/raw/Sept_2022.csv")[, c("PRAC_CODE", "PRAC_NAME", "CCG_CODE", "CCG_NAME", "REGION_NAME", "TOTAL_PATIENTS", "TOTAL_GP_SEN_PTNR_HC", "MALE_GP_SEN_PTNR_HC", "MALE_GP_PTNR_PROV_HC", "FEMALE_GP_SEN_PTNR_HC", "FEMALE_GP_PTNR_PROV_HC", "TOTAL_GP_PTNR_PROV_HC", "TOTAL_GP_SEN_PTNR_FTE", "TOTAL_GP_PTNR_PROV_FTE", "TOTAL_GP_EXTG_HC", "TOTAL_GP_EXTG_FTE", "TOTAL_NURSES_FTE", "TOTAL_DPC_FTE")]
df22 <- df22 %>%
    rename(., Practice.Code = PRAC_CODE)
df22$Year <- 2022

workforce <- rbind(workforce, df22)

workforce$TOTAL_GP_SEN_PTNR_FTE %<>% as.numeric()
workforce$TOTAL_GP_PTNR_PROV_FTE %<>% as.numeric()
workforce$TOTAL_GP_SEN_PTNR_HC %<>% as.numeric()
workforce$TOTAL_GP_PTNR_PROV_HC %<>% as.numeric()
workforce$TOTAL_GP_EXTG_HC %<>% as.numeric()
workforce$TOTAL_GP_EXTG_FTE %<>% as.numeric()
workforce$MALE_GP_SEN_PTNR_HC %<>% as.numeric()
workforce$MALE_GP_PTNR_PROV_HC %<>% as.numeric()
workforce$FEMALE_GP_SEN_PTNR_HC %<>% as.numeric()
workforce$FEMALE_GP_PTNR_PROV_HC %<>% as.numeric()

workforce$Total.PTNR.FTE <- workforce$TOTAL_GP_SEN_PTNR_FTE + workforce$TOTAL_GP_PTNR_PROV_FTE
workforce$Total.PTNR.HC <- workforce$TOTAL_GP_SEN_PTNR_HC + workforce$TOTAL_GP_PTNR_PROV_HC
workforce$Male.PTNR.HC <- workforce$MALE_GP_SEN_PTNR_HC + workforce$MALE_GP_PTNR_PROV_HC
workforce$Female.PTNR.HC <- workforce$FEMALE_GP_SEN_PTNR_HC + workforce$FEMALE_GP_PTNR_PROV_HC

for (i in unique(workforce$Year)) {
    print(i)
    workforce[workforce$Year == i, ] %>%
        group_by(REGION_NAME) %>%
        summarise(total = mean(Total.PTNR.HC, na.rm = TRUE), total_fte = mean(Total.PTNR.FTE, na.rm = TRUE)) %>%
        print()
}

workforce %>%
    group_by(Year) %>%
    summarise(male = mean(Male.PTNR.HC, na.rm = TRUE), female = mean(Female.PTNR.HC, na.rm = TRUE))

# combine the two dataframes by Practice.Code and Year
df <- merge(df, workforce, by = c("Practice.Code", "Year"))

n <- df[df$Dispensing.Fee.Payments != 0, ]
n %>%
    group_by(Year) %>%
    summarise(total = mean(Male.PTNR.HC, na.rm = TRUE), total_fte = mean(Female.PTNR.HC, na.rm = TRUE))

m <- df[df$Dispensing.Fee.Payments != 0, ]
m %>%
    group_by(Year) %>%
    summarise(total = mean(Total.PTNR.FTE, na.rm = TRUE))
