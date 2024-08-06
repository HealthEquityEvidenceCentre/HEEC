# Structural inequalities in primary care – the facts and figures

The factors determining the quality and quantity of primary care services vary across England. Here we analyse practice level data relating to the supply, demand, and need for primary care, according to the socioeconomic status of the patients served.

## How does NHS funding to GP surgeries vary across socioeconomic groups?

In order to ensure equitable primary care, practices serving populations with higher health care need should receive commensurately higher funding.

[NHS Digital](https://digital.nhs.uk/data-and-information/publications/statistical/nhs-payments-to-general-practice) provide public CSVs on NHS payments to individual providers of general practice services in England from 2014-15 to 2022-23, which we collate into a time series:

```r
payments2021 <- read.csv("https://files.digital.nhs.uk/A3/625945/nhspaymentsgp-20-21-prac-csv.csv")
payments2021$Year <- 2021
```

Data on IMD is also available.

Here we plot how funding has changed over time by socioeconomic group across England (4).

## Which ICBs have the greatest need for primary care?

Different populations have different needs for primary care. The NHS measures health care need by accounting for the populations’ demographic characteristics and overall health status to produce a ‘weighted’ population count.