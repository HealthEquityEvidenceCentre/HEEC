# Exploring the impact of dispensing practicing on equity in NHS payments to general practices

General practices serving the most deprived populations receive less funding per weighted patient than those serving the least deprived. Here we show that this inequality is driven by a higher concentration of dispensing practices in more affluent areas.

# What are dispensing practices?
Dispensing practices are practices where “at the patient’s request dispensing doctors are allowed to dispense the medicines they prescribe for these patients” (DDA); they are not the same as practices with an on-site pharmacy.

[NHS Digital](https://digital.nhs.uk/data-and-information/publications/statistical/nhs-payments-to-general-practice) provides public CSVs on NHS payments to individual providers of general practice services in England from 2014-15 to 2022-23.

```r
# Download the NHS payments data for 2022/23
library(magrittr)

df <- read.csv("https://files.digital.nhs.uk/05/E6ADA0/nhspaymentsgp-22-23-prac-csv.csv")

dispensing <- df[df$Dispensing.Practice == "Yes", ]
non_dispensing <- df[df$Dispensing.Practice == "No", ]

dispensing$Practice.Code %>%
  unique() %>%
  length()

df

dispensing$Average.Number.of.Registered.Patient %>% sum()

non_dispensing$Practice.Code %>%
  unique() %>%
  length()
```

In 2023, there were 944 dispensing covering 9,505,878 patients and 5,537 non-dispensing practices covering 52,628,766 patients in England (dispensing status unknown for 188 practices).



[OHID](https://fingertips.phe.org.uk/search/deprivation%20index#page/4/gid/1/pat/159/par/K02000001/ati/15/are/E92000001/iid/93553/age/1/sex/4/cat/-1/ctp/-1/yrr/1/cid/4/tbm/1) provides the [Fingertips API](https://fingertips.phe.org.uk/api), which includes practice-level IMD values.



Here, we demonstrate how to download and collate this data into a time series.