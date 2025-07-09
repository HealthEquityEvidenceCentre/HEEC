library(magrittr)
library(ggplot2)

df <- read.csv("GP_earnings/GP_earn_exp.csv")

# Dipsensing/Non-dispensing
df1 <- df[df$Region == "All" & df$Practice.type == "All" & df$Rurality == "All" & df$Size == "All", ]
df2 <- df[df$Region == "All" & df$Practice.type == "Dispensing" & df$Rurality == "All" & df$Size == "All", ]
df3 <- df[df$Region == "All" & df$Practice.type == "Non-dispensing" & df$Rurality == "All" & df$Size == "All", ]

# plot a line chart of Income.before.tax ~ Year
library(ggplot2)
ggplot() +
    geom_line(data = df1, aes(x = Year, y = Mean.Total.Partners.HC, colour = "All")) +
    geom_line(data = df2, aes(x = Year, y = Mean.Total.Partners.HC, colour = "Dispensing")) +
    geom_line(data = df3, aes(x = Year, y = Mean.Total.Partners.HC, colour = "Non-dispensing")) +
    labs(title = "Mean total partners by year by Practice Type", x = "Year", y = "Total Partners HC", colour = "Practice type") +
    scale_colour_manual(values = c("All" = "black", "Dispensing" = "red", "Non-dispensing" = "blue"))

ggsave("charts/Mean_total_partners_by_year_by_practice_type.png")

ggplot() +
    geom_line(data = df1, aes(x = Year, y = Income.before.tax..contractor., colour = "All")) +
    geom_line(data = df2, aes(x = Year, y = Income.before.tax..contractor., colour = "Dispensing")) +
    geom_line(data = df3, aes(x = Year, y = Income.before.tax..contractor., colour = "Non-dispensing")) +
    geom_line(data = df1, aes(x = Year, y = Income.before.tax..salaried., colour = "All")) +
    # Add shaded areas for salaried and contractor income
    labs(title = "Partner income before tax by year by Practice Type", x = "Year", y = "Income before tax", colour = "Practice type") +
    scale_colour_manual(values = c("All" = "black", "Dispensing" = "red", "Non-dispensing" = "blue"))

ggsave("charts/type/Partner_income_before_tax_by_year_by_practice_type_ribbon.png")

### same plot but without dispensing vs non-dispensing
ggplot() +
    geom_line(data = df1, aes(x = Year, y = Income.before.tax..contractor., colour = "Partner")) +
    geom_line(data = df1, aes(x = Year, y = Income.before.tax.D1..contractor., colour = "Partner"), linetype = "dashed") +
    geom_line(data = df1, aes(x = Year, y = Income.before.tax.D9..contractor., colour = "Partner"), linetype = "dashed") +
    geom_line(data = df1, aes(x = Year, y = Income.before.tax..salaried., colour = "Salaried")) +
    geom_line(data = df1, aes(x = Year, y = Income.before.tax.D1..salaried., colour = "Salaried"), linetype = "dashed") +
    geom_line(data = df1, aes(x = Year, y = Income.before.tax.D9..salaried., colour = "Salaried"), linetype = "dashed") +
    labs(title = "Partner vs salaried incomes before tax by year", x = "Year", y = "Income before tax", colour = "Practice type") +
    scale_colour_manual(values = c("Partner" = "#EF7A34", "Salaried" = "#007AA8")) +
    scale_linetype_manual(name = "Deciles 1-9", values = "dashed") +
    theme(text = element_text(size = 20)) + # Adjust font size as needed
    # theme_minimal() +
    # add labels for the dashed lines
    annotate("text", x = 2022, y = 100000, label = "Top 10%", colour = "#007AA8", size = 5, angle = 90, hjust = 0, vjust = 0) +
    annotate("text", x = 2022, y = 75000, label = "Bottom 10%", colour = "#EF7A34", size = 5, angle = 90, hjust = 0, vjust = 0) +
    annotate("text", x = 2022, y = 230000, label = "Top 10%", colour = "#EF7A34", size = 5, angle = 90, hjust = 0, vjust = 0) +
    annotate("text", x = 2022, y = 27500, label = "Bottom 10%", colour = "#007AA8", size = 5, angle = 90, hjust = 0, vjust = 0)

ggsave("GP_earnings/charts/Partner_vs_salaried_incomes_before_tax_by_year.png", width = 8, height = 11.5)

ggplot() +
    geom_line(data = df1, aes(x = Year, y = Total.Earnings, colour = "All")) +
    geom_line(data = df2, aes(x = Year, y = Total.Earnings, colour = "Dispensing")) +
    geom_line(data = df3, aes(x = Year, y = Total.Earnings, colour = "Non-dispensing")) +
    geom_line(data = df1, aes(x = Year, y = Payments.per.Partner, colour = "All"), linetype = "dashed") +
    geom_line(data = df2, aes(x = Year, y = Payments.per.Partner, colour = "Dispensing"), linetype = "dashed") +
    geom_line(data = df3, aes(x = Year, y = Payments.per.Partner, colour = "Non-dispensing"), linetype = "dashed") +
    labs(title = "Mean total earnings (solid) and payments per partner (dashed) by year by Practice Type", x = "Year", y = "Earnings (solid) and Payments per Partner (dashed)", colour = "Practice type") +
    scale_colour_manual(values = c("All" = "black", "Dispensing" = "red", "Non-dispensing" = "blue")) +
    scale_y_continuous(labels = function(x) format(x, scientific = FALSE)) +
    scale_linetype_manual(
        name = "Payment type", values = c("solid", "dashed"),
        labels = c("Total Earnings", "Payments per Partner"),
        guide = guide_legend(override.aes = list(color = c("black", "red"), linetype = c("solid", "dashed")))
    )

ggsave("charts/type/Mean_total_earnings_and_payments_by_year_by_practice_type.png")

### Bar charts for dipsensing practices salaried vs contractor
ggplot() +
    geom_bar(data = df1[df1$Year == 2022, ], aes(x = df1, y = Income.before.tax..salaried., fill = "Salaried"), stat = "identity") +
    labs(title = "Mean total partners by year by Practice Type", x = "Practice type", y = "Income before tax", fill = "Practice type") +
    scale_fill_manual(values = c("Salaried" = "red", "Contractor" = "blue"))



# Rurality
df4 <- df[df$Region == "All" & df$Practice.type == "All" & df$Rurality == "Rural" & df$Size == "All", ]
df5 <- df[df$Region == "All" & df$Practice.type == "All" & df$Rurality == "Urban" & df$Size == "All", ]

ggplot() +
    geom_line(data = df1[df1$Year >= 2018, ], aes(x = Year, y = Mean.Total.Partners.HC, colour = "All")) +
    geom_line(data = df4, aes(x = Year, y = Mean.Total.Partners.HC, colour = "Rural")) +
    geom_line(data = df5, aes(x = Year, y = Mean.Total.Partners.HC, colour = "Urban")) +
    labs(title = "Mean total partners by year (Rurality)", x = "Year", y = "Total Partners HC", colour = "Rurality") +
    scale_colour_manual(values = c("All" = "black", "Rural" = "red", "Urban" = "blue"))

ggsave("charts/Mean_total_partners_by_year_by_rurality.png")

ggplot() +
    geom_line(data = df1[df1$Year >= 2018, ], aes(x = Year, y = Income.before.tax..contractor., colour = "All")) +
    geom_line(data = df4, aes(x = Year, y = Income.before.tax..contractor., colour = "Rural")) +
    geom_line(data = df5, aes(x = Year, y = Income.before.tax..contractor., colour = "Urban")) +
    geom_line(data = df1[df1$Year >= 2018, ], aes(x = Year, y = Income.before.tax..salaried., colour = "All")) +
    geom_line(data = df4, aes(x = Year, y = Income.before.tax..salaried., colour = "Rural")) +
    geom_line(data = df5, aes(x = Year, y = Income.before.tax..salaried., colour = "Urban")) +
    labs(title = "Partner income before tax by year (Rurality)", x = "Year", y = "Income before tax", colour = "Rurality") +
    scale_colour_manual(values = c("All" = "black", "Rural" = "red", "Urban" = "blue"))

ggsave("charts/rurality/Partner_income_before_tax_by_year_by_rurality.png")

ggplot() +
    geom_line(data = df1[df1$Year >= 2018, ], aes(x = Year, y = Total.Earnings, colour = "All")) +
    geom_line(data = df4, aes(x = Year, y = Total.Earnings, colour = "Rural")) +
    geom_line(data = df5, aes(x = Year, y = Total.Earnings, colour = "Urban")) +
    geom_line(data = df1[df1$Year >= 2018, ], aes(x = Year, y = Payments.per.Partner, colour = "All"), linetype = "dashed") +
    geom_line(data = df4, aes(x = Year, y = Payments.per.Partner, colour = "Rural"), linetype = "dashed") +
    geom_line(data = df5, aes(x = Year, y = Payments.per.Partner, colour = "Urban"), linetype = "dashed") +
    labs(title = "Mean total earnings (solid) and payments per partner (dashed) by year by Rurality", x = "Year", y = "Mean total earnings (solid) and payments per partner (dashed)", colour = "Practice type") +
    scale_colour_manual(values = c("All" = "black", "Rural" = "red", "Urban" = "blue")) +
    scale_y_continuous(labels = function(x) format(x, scientific = FALSE)) +
    scale_linetype_manual(
        name = "Payment type", values = c("solid", "dashed"),
        labels = c("Total Earnings", "Payments per Partner"),
        guide = guide_legend(override.aes = list(color = c("black", "red"), linetype = c("solid", "dashed")))
    )

ggsave("charts/rurality/Mean_total_earnings_and_payments_by_year_by_rurality.png")

# Practice size
df6 <- df[df$Region == "All" & df$Practice.type == "All" & df$Rurality == "All" & df$Size == "0-4,999", ]
df7 <- df[df$Region == "All" & df$Practice.type == "All" & df$Rurality == "All" & df$Size == "5,000-9,999", ]
df8 <- df[df$Region == "All" & df$Practice.type == "All" & df$Rurality == "All" & df$Size == "10,000-14,999", ]
df9 <- df[df$Region == "All" & df$Practice.type == "All" & df$Rurality == "All" & df$Size == "15,000-19,999", ]
df10 <- df[df$Region == "All" & df$Practice.type == "All" & df$Rurality == "All" & df$Size == "20,000+", ]

ggplot() +
    geom_line(data = df6, aes(x = Year, y = Mean.Total.Partners.HC, colour = "0-4,999")) +
    geom_line(data = df7, aes(x = Year, y = Mean.Total.Partners.HC, colour = "5,000-9,999")) +
    geom_line(data = df8, aes(x = Year, y = Mean.Total.Partners.HC, colour = "10,000-14,999")) +
    geom_line(data = df9, aes(x = Year, y = Mean.Total.Partners.HC, colour = "15,000-19,999")) +
    geom_line(data = df10, aes(x = Year, y = Mean.Total.Partners.HC, colour = "20,000+")) +
    labs(title = "Mean total partners by year by Practice Size", x = "Year", y = "Total Partners HC", colour = "Practice size") +
    scale_colour_manual(values = c("0-4,999" = "black", "5,000-9,999" = "red", "10,000-14,999" = "blue", "15,000-19,999" = "green", "20,000+" = "orange"))

ggsave("charts/Mean_total_partners_by_year_by_practice_size.png")

# Region
df11 <- df[df$Region == "North West" & df$Practice.type == "All" & df$Rurality == "All" & df$Size == "All", ]
df12 <- df[df$Region == "North East and Yorkshire" & df$Practice.type == "All" & df$Rurality == "All" & df$Size == "All", ]
df13 <- df[df$Region == "Midlands" & df$Practice.type == "All" & df$Rurality == "All" & df$Size == "All", ]
df14 <- df[df$Region == "East of England" & df$Practice.type == "All" & df$Rurality == "All" & df$Size == "All", ]
df15 <- df[df$Region == "London" & df$Practice.type == "All" & df$Rurality == "All" & df$Size == "All", ]
df16 <- df[df$Region == "South East" & df$Practice.type == "All" & df$Rurality == "All" & df$Size == "All", ]
df17 <- df[df$Region == "South West" & df$Practice.type == "All" & df$Rurality == "All" & df$Size == "All", ]

ggplot() +
    geom_line(data = df11, aes(x = Year, y = Mean.Total.Partners.HC, colour = "North West")) +
    geom_line(data = df12, aes(x = Year, y = Mean.Total.Partners.HC, colour = "North East and Yorkshire")) +
    geom_line(data = df13, aes(x = Year, y = Mean.Total.Partners.HC, colour = "Midlands")) +
    geom_line(data = df14, aes(x = Year, y = Mean.Total.Partners.HC, colour = "East of England")) +
    geom_line(data = df15, aes(x = Year, y = Mean.Total.Partners.HC, colour = "London")) +
    geom_line(data = df16, aes(x = Year, y = Mean.Total.Partners.HC, colour = "South East")) +
    geom_line(data = df17, aes(x = Year, y = Mean.Total.Partners.HC, colour = "South West")) +
    labs(title = "Mean total partners by year by Region", x = "Year", y = "Total Partners HC", colour = "Region") +
    scale_colour_manual(values = c("North West" = "black", "North East and Yorkshire" = "red", "Midlands" = "blue", "East of England" = "green", "London" = "orange", "South East" = "purple", "South West" = "brown"))

ggsave("GP_earnings/charts/region/Mean_total_partners_by_year.png")

ggplot() +
    geom_line(data = df11, aes(x = Year, y = Income.before.tax..contractor., colour = "North West")) +
    geom_line(data = df12, aes(x = Year, y = Income.before.tax..contractor., colour = "North East and Yorkshire")) +
    geom_line(data = df13, aes(x = Year, y = Income.before.tax..contractor., colour = "Midlands")) +
    geom_line(data = df14, aes(x = Year, y = Income.before.tax..contractor., colour = "East of England")) +
    geom_line(data = df15, aes(x = Year, y = Income.before.tax..contractor., colour = "London")) +
    geom_line(data = df16, aes(x = Year, y = Income.before.tax..contractor., colour = "South East")) +
    geom_line(data = df17, aes(x = Year, y = Income.before.tax..contractor., colour = "South West")) +
    # geom_line(data = df11, aes(x = Year, y = Income.before.tax..salaried., colour = "North West")) +
    # geom_line(data = df12, aes(x = Year, y = Income.before.tax..salaried., colour = "North East and Yorkshire")) +
    # geom_line(data = df13, aes(x = Year, y = Income.before.tax..salaried., colour = "Midlands")) +
    # geom_line(data = df14, aes(x = Year, y = Income.before.tax..salaried., colour = "East of England")) +
    # geom_line(data = df15, aes(x = Year, y = Income.before.tax..salaried., colour = "London")) +
    # geom_line(data = df16, aes(x = Year, y = Income.before.tax..salaried., colour = "South East")) +
    # geom_line(data = df17, aes(x = Year, y = Income.before.tax..salaried., colour = "South West")) +
    labs(title = "Partner income before tax by year by Region", x = "Year", y = "Income before tax", colour = "Region") +
    scale_colour_manual(values = c("North West" = "black", "North East and Yorkshire" = "red", "Midlands" = "blue", "East of England" = "green", "London" = "orange", "South East" = "purple", "South West" = "brown"))

ggsave("GP_earnings/charts/region/Partner_income_before_tax_by_year.png")

# plot a line chart of Income.before.tax ~ Year
ggplot() +
    geom_line(data = df6, aes(x = Year, y = Income.before.tax..contractor., colour = "0-4,999")) +
    geom_line(data = df7, aes(x = Year, y = Income.before.tax..contractor., colour = "5,000-9,999")) +
    geom_line(data = df8, aes(x = Year, y = Income.before.tax..contractor., colour = "10,000-14,999")) +
    geom_line(data = df9, aes(x = Year, y = Income.before.tax..contractor., colour = "15,000-19,999")) +
    geom_line(data = df10, aes(x = Year, y = Income.before.tax..contractor., colour = "20,000+")) +
    # geom_line(data = df6, aes(x = Year, y = Income.before.tax..salaried., colour = "0-4,999")) +
    # geom_line(data = df7, aes(x = Year, y = Income.before.tax..salaried., colour = "5,000-9,999")) +
    # geom_line(data = df8, aes(x = Year, y = Income.before.tax..salaried., colour = "10,000-14,999")) +
    # geom_line(data = df9, aes(x = Year, y = Income.before.tax..salaried., colour = "15,000-19,999")) +
    # geom_line(data = df10, aes(x = Year, y = Income.before.tax..salaried., colour = "20,000+")) +
    labs(title = "Partner income before tax by year by Size", x = "Year", y = "Income before tax", colour = "Practice size") +
    scale_colour_manual(values = c("0-4,999" = "black", "5,000-9,999" = "red", "10,000-14,999" = "blue", "15,000-19,999" = "green", "20,000+" = "orange"))

ggsave("charts/size/Partner_income_before_tax_by_year_by_practice_size.png")

ggplot() +
    geom_line(data = df6, aes(x = Year, y = Total.Earnings, colour = "0-4,999")) +
    geom_line(data = df7, aes(x = Year, y = Total.Earnings, colour = "5,000-9,999")) +
    geom_line(data = df8, aes(x = Year, y = Total.Earnings, colour = "10,000-14,999")) +
    geom_line(data = df9, aes(x = Year, y = Total.Earnings, colour = "15,000-19,999")) +
    geom_line(data = df10, aes(x = Year, y = Total.Earnings, colour = "20,000+")) +
    geom_line(data = df6, aes(x = Year, y = Payments.per.Partner, colour = "0-4,999"), linetype = "dashed") +
    geom_line(data = df7, aes(x = Year, y = Payments.per.Partner, colour = "5,000-9,999"), linetype = "dashed") +
    geom_line(data = df8, aes(x = Year, y = Payments.per.Partner, colour = "10,000-14,999"), linetype = "dashed") +
    geom_line(data = df9, aes(x = Year, y = Payments.per.Partner, colour = "15,000-19,999"), linetype = "dashed") +
    geom_line(data = df10, aes(x = Year, y = Payments.per.Partner, colour = "20,000+"), linetype = "dashed") +
    labs(title = "Mean total earnings (solid) and payments per partner (dashed) by year by Practice Size", x = "Year", y = "Earnings (solid) and Payments per Partner (dashed)", colour = "Practice size") +
    scale_colour_manual(values = c("0-4,999" = "black", "5,000-9,999" = "red", "10,000-14,999" = "blue", "15,000-19,999" = "green", "20,000+" = "orange")) +
    scale_y_continuous(labels = function(x) format(x, scientific = FALSE)) +
    scale_linetype_manual(
        name = "Payment type", values = c("solid", "dashed"),
        labels = c("Total Earnings", "Payments per Partner"),
        guide = guide_legend(override.aes = list(color = c("black", "red", "blue", "green", "orange"), linetype = c("solid", "dashed")))
    )

ggsave("charts/size/Mean_total_earnings_and_payments_by_year_by_practice_size.png")


### Real vs nominal salary increase
partner_real <- c(125800, 128800, 128400, 128800, 130400, 150200, 153400)
partner_nominal <- c(104900, 109600, 113400, 117300, 121800, 142000, 153400)
sal_real <- c(67100, 66500, 66100, 66600, 68100, 68600, 68000)
sal_nominal <- c(55900, 56600, 58400, 60600, 63600, 64900, 68000)

ggplot() +
    geom_line(aes(x = 2016:2022, y = partner_nominal, colour = "Partner (Cash)")) +
    geom_line(aes(x = 2016:2022, y = partner_real, colour = "Partner (Inflation-adjusted)"), linetype = "dashed") +
    geom_line(aes(x = 2016:2022, y = sal_nominal, colour = "Salaried (Cash)")) +
    geom_line(aes(x = 2016:2022, y = sal_real, colour = "Salaried (Inflation-adjusted)"), linetype = "dashed") +
    labs(title = "Cash terms vs inflation-adjusted", x = "Year", y = "Income before tax", colour = "Type") +
    scale_colour_manual(values = c("Partner (Cash)" = "#EF7A34", "Partner (Inflation-adjusted)" = "#EF7A34", "Salaried (Cash)" = "#007AA8", "Salaried (Inflation-adjusted)" = "#007AA8")) +
    scale_y_continuous(labels = function(x) format(x, scientific = FALSE)) +
    guides(colour = guide_legend(override.aes = list(linetype = c("solid", "dashed", "solid", "dashed")))) +
    theme(text = element_text(size = 20)) + # Adjust font size as needed
    theme_minimal()

ggsave("GP_earnings/charts/Nominal_vs_real.png", width = 8, height = 11.5)



### Bar charts
