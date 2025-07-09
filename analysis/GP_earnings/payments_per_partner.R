for (file in list.files("nhs_payments/raw")[1:7]) {
    year <- substr(file, 1, nchar(file) - 4)
    year <- substr(year, 4, nchar(year))
    year <- paste0("20", year)
    df$Year <- year

    print(year)
}

df <- data.frame()
payment <- list()
ppt <- list()
phc <- list()

library(dplyr)

for (i in 2:6) {
    print(list.files("nhs_payments/raw/")[i])
    print(list.files("workforce/raw/")[i])

    # first, get the average total payments per year (before merging and dropping practices without partners)
    df <- read.csv(paste0("nhs_payments/raw/", list.files("nhs_payments/raw/")[i]))

    # payment[i - 1] <- df$Total.NHS.Payments.to.General.Practice.Minus.Deductions %>%
    #     mean(na.rm = TRUE)
    # print(payment[i - 1])

    wf <- read.csv(paste0("workforce/raw/", list.files("workforce/raw/")[i]))
    wf %<>% rename(., Practice.Code = PRAC_CODE)

    merge <- merge(df, wf, by = "Practice.Code")

    merge$TOTAL_GP_PTNR_PROV_HC %<>% as.numeric()
    merge$TOTAL_GP_SEN_PTNR_HC %<>% as.numeric()
    merge$TOTAL_N_NURSE_PTNR_HC %<>% as.numeric()
    merge$TOTAL_ADMIN_MANAGE_PTNR_HC %<>% as.numeric()

    # sum different types of partnets
    merge$Total.PTNR.HC <- merge$TOTAL_GP_SEN_PTNR_HC + merge$TOTAL_GP_PTNR_PROV_HC + merge$TOTAL_N_NURSE_PTNR_HC + merge$TOTAL_ADMIN_MANAGE_PTNR_HC

    # calculate the number of practices with 0 partners
    merge[merge$Total.PTNR.HC == 0, ] %>%
        nrow() %>%
        print()

    # drop practices with 0 partners so that we can calculate the mean payment per partner
    merge <- merge[merge$Total.PTNR.HC != 0, ]

    merge$Payment_per_PTNR <- merge$Total.NHS.Payments.to.General.Practice.Minus.Deductions / merge$Total.PTNR.HC
    merge$Payment_per_PTNR %>%
        mean(na.rm = TRUE) %>%
        print()

    payment[i - 1] <- merge$Total.NHS.Payments.to.General.Practice.Minus.Deductions %>%
        mean(na.rm = TRUE)
    print(payment[i - 1])

    # add to list ppt
    ppt[i - 1] <- merge$Payment_per_PTNR %>%
        mean(na.rm = TRUE)

    phc[i - 1] <- merge$Total.PTNR.HC %>%
        mean(na.rm = TRUE)
}

# INVESTIGATE characteristics of practices with 0 partners: size, number of GPs, payments
missing <- read.csv("nhs_payments/raw/15-16.csv")
wf <- read.csv("workforce/raw/Sept_2016.csv")
wf %<>% rename(., Practice.Code = PRAC_CODE)

missing <- merge(missing, wf, by = "Practice.Code")

missing$TOTAL_GP_PTNR_PROV_HC %<>% as.numeric()
missing$TOTAL_GP_SEN_PTNR_HC %<>% as.numeric()
missing$TOTAL_N_NURSE_PTNR_HC %<>% as.numeric()
missing$TOTAL_ADMIN_MANAGE_PTNR_HC %<>% as.numeric()

# sum different types of partnets
missing$Total.PTNR.HC <- missing$TOTAL_GP_SEN_PTNR_HC + missing$TOTAL_GP_PTNR_PROV_HC + missing$TOTAL_N_NURSE_PTNR_HC + missing$TOTAL_ADMIN_MANAGE_PTNR_HC

missing <- missing[missing$Total.PTNR.HC == 0, ]
missing$Total.NHS.Payments.to.General.Practice.Minus.Deductions %>% mean(na.rm = TRUE)
missing$Number.of.Registered.Patients..Last.Known.Figure. %>% mean(na.rm = TRUE)
missing$TOTAL_GP_HC %>% mean(na.rm = TRUE)
# how many have 0 TOTAL_GP_HC
missing[missing$TOTAL_GP_HC == 0, ] %>%
    nrow()

###
earn <- c(314900, 338300, 357300, 380900, 402600)

# merge ppt with earn
earn_ppt <- data.frame(Year = 2016:2020, Earnings = earn, Payment = unlist(payment), Payment_per_PTNR = unlist(ppt), Partner_HC = unlist(phc))

earn_ppt$Payment_per_PTNR_incld_Covid <- earn_ppt$Payment_per_PTNR

# mean(earn_ppt$Ratio)

### Check difference between nhs_payments and merged in 2016
payments2016 <- read.csv("nhs_payments/raw/15-16.csv")
wf2016 <- read.csv("workforce/raw/Sept_2016.csv")
wf2016 %<>% rename(., Practice.Code = PRAC_CODE)

merge2016 <- merge(payments2016, wf2016, by = "Practice.Code")
merge2016[, 57] %>% mean(na.rm = TRUE)

### Get remaining years
# 2021
payments2021 <- read.csv("nhs_payments/raw/20-21.csv")
# drop "Total.NHS.Payments.to.General.Practice.Minus.Deductions"
payments2021$Year <- 2021

wf <- read.csv("workforce/raw/Sept_2021.csv")
wf %<>% rename(., Practice.Code = PRAC_CODE)

merge <- merge(payments2021, wf, by = "Practice.Code")


merge$TOTAL_GP_PTNR_PROV_HC %<>% as.numeric()
merge$TOTAL_GP_SEN_PTNR_HC %<>% as.numeric()
merge$TOTAL_N_NURSE_PTNR_HC %<>% as.numeric()
merge$TOTAL_ADMIN_MANAGE_PTNR_HC %<>% as.numeric()

merge$Total.PTNR.HC <- merge$TOTAL_GP_SEN_PTNR_HC + merge$TOTAL_GP_PTNR_PROV_HC + merge$TOTAL_N_NURSE_PTNR_HC + merge$TOTAL_ADMIN_MANAGE_PTNR_HC

merge <- merge[merge$Total.PTNR.HC != 0, ]

merge$Total.Covid.payments <- merge$Total.NHS.Payments.including.covid.vaccination.and.covid.support.and.expansion.payments - merge$Total.NHS.Payments.to.General.Practice
merge$Total.PCN.payments <- merge$Total.NHS.Payments.including.PCN.Workforce..Leadership.and.Support - merge$Total.NHS.Payments.to.General.Practice

merge$Total.NHS.Payments.to.General.Practice.including.Covid.and.PCN.payments.minus.deductions <- merge$Total.NHS.Payments.to.General.Practice.Minus.Deductions + merge$Total.Covid.payments + merge$Total.PCN.payments

merge$Payment_per_PTNR_incld_Covid <- merge$Total.NHS.Payments.to.General.Practice.including.Covid.and.PCN.payments.minus.deductions / merge$Total.PTNR.HC
merge$Payment_per_PTNR_incld_Covid %>% mean(na.rm = TRUE)

merge$Payment_per_PTNR <- merge$Total.NHS.Payments.to.General.Practice.Minus.Deductions / merge$Total.PTNR.HC
merge$Payment_per_PTNR %>% mean(na.rm = TRUE)

earn_ppt <- rbind(earn_ppt, data.frame(Year = 2021, Earnings = 438700, Payment = merge$Total.NHS.Payments.to.General.Practice.including.Covid.and.PCN.payments.minus.deductions %>% mean(na.rm = TRUE), Payment_per_PTNR_incld_Covid = merge$Payment_per_PTNR_incld_Covid %>% mean(na.rm = TRUE), Partner_HC = merge$Total.PTNR.HC %>% mean(na.rm = TRUE), Payment_per_PTNR = merge$Payment_per_PTNR %>% mean(na.rm = TRUE)))

# 2022
payments2022 <- read.csv("nhs_payments/raw/21-22.csv")
# drop "Total.NHS.Payments.to.General.Practice.Minus.Deductions"
payments2022$Year <- 2022

wf <- read.csv("workforce/raw/Sept_2022.csv")
wf %<>% rename(., Practice.Code = PRAC_CODE)

merge <- merge(payments2022, wf, by = "Practice.Code")

merge$TOTAL_GP_PTNR_PROV_HC %<>% as.numeric()
merge$TOTAL_GP_SEN_PTNR_HC %<>% as.numeric()
merge$TOTAL_N_NURSE_PTNR_HC %<>% as.numeric()
merge$TOTAL_ADMIN_MANAGE_PTNR_HC %<>% as.numeric()

merge$Total.PTNR.HC <- merge$TOTAL_GP_SEN_PTNR_HC + merge$TOTAL_GP_PTNR_PROV_HC + merge$TOTAL_N_NURSE_PTNR_HC + merge$TOTAL_ADMIN_MANAGE_PTNR_HC

merge <- merge[merge$Total.PTNR.HC != 0, ]

merge$Payment_per_PTNR_incld_Covid <- merge$Total.NHS.Payments.to.General.Practice.including.Covid.and.PCN.payments.minus.deductions / merge$Total.PTNR.HC
merge$Payment_per_PTNR %>% mean(na.rm = TRUE)

merge$Payment_per_PTNR <- merge$Total.NHS.Payments.to.General.Practice.Minus.Deductions / merge$Total.PTNR.HC
merge$Payment_per_PTNR %>% mean(na.rm = TRUE)

# add to dataframe ppt
earn_ppt <- rbind(earn_ppt, data.frame(Year = 2022, Earnings = 482400, Payment = merge$Total.NHS.Payments.to.General.Practice.including.Covid.and.PCN.payments.minus.deductions %>% mean(na.rm = TRUE), Payment_per_PTNR = merge$Payment_per_PTNR %>% mean(na.rm = TRUE), Partner_HC = merge$Total.PTNR.HC %>% mean(na.rm = TRUE), Payment_per_PTNR_incld_Covid = merge$Payment_per_PTNR_incld_Covid %>% mean(na.rm = TRUE)))

earn_ppt$Ratio <- earn_ppt$Payment_per_PTNR / earn_ppt$Earnings

### plot
ggplot() +
    geom_line(data = earn_ppt, aes(x = Year, y = Earnings, colour = "Earnings")) +
    # geom_line(data = df2, aes(x = Year, y = Total.Earnings, colour = "Dispensing")) +
    # geom_line(data = df3, aes(x = Year, y = Total.Earnings, colour = "Non-dispensing")) +
    geom_line(data = earn_ppt, aes(x = Year, y = Payment_per_PTNR, colour = "Excluding COVID"), linetype = "dashed") +
    geom_line(data = earn_ppt, aes(x = Year, y = Payment_per_PTNR_incld_Covid, colour = "Payment"), linetype = "dashed") +
    # geom_line(data = df2, aes(x = Year, y = Payments.per.Partner, colour = "Dispensing"), linetype = "dashed") +
    # geom_line(data = df3, aes(x = Year, y = Payments.per.Partner, colour = "Non-dispensing"), linetype = "dashed") +
    labs(title = "Mean total earnings (solid) and payments per partner (dashed) by year by Practice Type", x = "Year", y = "Earnings (solid) and Payments per Partner (dashed)", colour = "Practice type") +
    scale_colour_manual(values = c("Earnings" = "black", "Payment" = "Black", "Excluding COVID" = "Red")) +
    scale_y_continuous(labels = function(x) format(x, scientific = FALSE)) +
    scale_linetype_manual(
        # name = "Payment type", values = c("solid", "dashed"),
        # labels = c("Total Earnings", "Payments per Partner"),
        # guide = guide_legend(override.aes = list(color = c("black", "red"), linetype = c("solid", "dashed")))
    )


### Earnings vs income
earn <- c(314900, 338300, 357300, 380900, 402600, 438700, 482400)
income <- c(104900, 109600, 113400, 117300, 121800, 142000, 153400)

ggplot() +
    geom_line(aes(x = 2016:2022, y = earn, colour = "Earnings")) +
    geom_line(aes(x = 2016:2022, y = income, colour = "Income")) +
    labs(title = "Earnings vs income", x = "Year", y = "Earnings and income", colour = "Type") +
    scale_colour_manual(values = c("Earnings" = "black", "Income" = "red")) +
    scale_y_continuous(labels = function(x) format(x, scientific = FALSE))

ggplot() +
    geom_line(data = earn_ppt, aes(x = Year, y = Earnings, colour = "Earnings")) +
    geom_line(data = earn_ppt, aes(x = Year, y = Payment_per_PTNR, colour = "Payments per Partner (Excluding COVID)"), linetype = "dashed") +
    geom_line(data = earn_ppt, aes(x = Year, y = Payment_per_PTNR_incld_Covid, colour = "Payments per Partner (Including COVID)")) +
    geom_line(aes(x = 2016:2022, y = income, colour = "Income before tax")) +
    labs(title = "Earnings vs Income", x = "Year", y = "Amount", colour = "Type") +
    scale_colour_manual(values = c("Earnings" = "black", "Income" = "red", "Income before tax" = "red", "Payments per Partner (Including COVID)" = "Green", "Payments per Partner (Excluding COVID)" = "Green")) +
    scale_y_continuous(labels = function(x) format(x, scientific = FALSE)) +
    scale_linetype_manual(values = c("solid", "solid", "solid", "solid", "dashed", "dashed")) +
    theme(text = element_text(size = 14)) # Adjust font size as needed



### Payment per patient
payments <- read.csv("nhs_payments/nhs_payments.csv")

payments2016 <- payments[payments$Year == 2016 & (payments$Number.of.Registered.Patients..Last.Known.Figure. != 0), ]
(payments2016$Total.NHS.Payments.to.General.Practice.Minus.Deductions / payments2016$Number.of.Registered.Patients..Last.Known.Figure.) %>% mean(na.rm = TRUE)

payments2022 <- payments[payments$Year == 2022 & (payments$Number.of.Registered.Patients..Last.Known.Figure. != 0) & (payments$Number.of.Registered.Patients..Last.Known.Figure. > 50), ]
(payments2022$Total.NHS.Payments.to.General.Practice.Minus.Deductions / payments2022$Number.of.Registered.Patients..Last.Known.Figure.) %>% mean(na.rm = TRUE)


### Nominal vs real
partner_real <- c(125800, 128800, 128400, 128800, 130400, 150200, 153400)
partner_nominal <- c(104900, 109600, 113400, 117300, 121800, 142000, 153400)
sal_real <- c(67100, 66500, 66100, 66600, 68100, 68600, 68000)
sal_nominal <- c(55900, 56600, 58400, 60600, 63600, 64900, 68000)

ggplot() +
    geom_line(aes(x = 2016:2022, y = partner_real, colour = "Partner (Real)")) +
    geom_line(aes(x = 2016:2022, y = partner_nominal, colour = "Partner (Nominal)"), linetype = "dashed") +
    geom_line(aes(x = 2016:2022, y = sal_real, colour = "Salaried (Real)")) +
    geom_line(aes(x = 2016:2022, y = sal_nominal, colour = "Salaried (Nominal)"), linetype = "dashed") +
    labs(title = "Real vs nominal", x = "Year", y = "Real and nominal", colour = "Type") +
    scale_colour_manual(values = c("Partner (Real)" = "black", "Partner (Nominal)" = "black", "Salaried (Real)" = "red", "Salaried (Nominal)" = "red")) +
    scale_y_continuous(labels = function(x) format(x, scientific = FALSE)) +
    guides(colour = guide_legend(override.aes = list(linetype = c("solid", "dashed", "solid", "dashed")))) +
    theme(text = element_text(size = 20)) # Adjust font size as needed


# PLOT headcount of partners over time
ggplot() +
    geom_line(data = earn_ppt, aes(x = Year, y = Partner_HC, colour = "Partners")) +
    labs(title = "Headcount of partners over time", x = "Year", y = "Headcount", colour = "Type") +
    scale_colour_manual(values = c("Partners" = "black")) +
    scale_y_continuous(labels = function(x) format(x, scientific = FALSE)) +
    theme(text = element_text(size = 20)) # Adjust font size as needed
