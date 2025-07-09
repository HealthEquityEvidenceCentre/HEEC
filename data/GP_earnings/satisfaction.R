###Check to see if patient satisfaction is higher in larger practices
satisfaction <- read.csv("satisfaction/satisfaction.csv")
satisfaction <- satisfaction[satisfaction$Year == 2022,]
#rename Practice_Name to Practice.Name
colnames(satisfaction)[1] <- "Practice.Code"

payments <- read.csv("nhs_payments/raw/21-22.csv")
payments <- payments[,c("Practice.Code", "Number.of.Registered.Patients..Last.Known.Figure.")]

#merge payments and satisfaction
satisfaction <- merge(satisfaction, payments, by="Practice.Code")[,-4]

# create a new column Practice.Size, splitting Number.of.Registered.Patients..Last.Known.Figure. into 5 categories: 0-4,999, 5,000-9,999, 10,000-14,999, 15,000-19,999, 20,000+
satisfaction$Practice.Size <- cut(satisfaction$Number.of.Registered.Patients..Last.Known.Figure., 
    breaks=c(0, 4999, 9999, 14999, 19999, Inf), 
    labels=c("0-4,999", "5,000-9,999", "10,000-14,999", "15,000-19,999", "20,000+"))

#calculate the number of practices in each size category
satisfaction %>% group_by(Practice.Size) %>% summarise(n = n())

#calculate the mean satisfaction score for each practice size
satisfaction %>% group_by(Practice.Size) %>% summarise(mean_score = mean(overall_pct))
