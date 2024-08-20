# Which ICBs have the greatest need for primary care?

Different populations have different needs for primary care. The NHS
measures health care need by accounting for the populations’ demographic
characteristics and overall health status to produce a ‘weighted’
population count.

``` r
library(magrittr)
library(dplyr)
```

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
library(ggplot2)

payments <- read.csv("../../data/payments/payments.csv")

agg <- payments[payments$Year == 2023, ] %>%
  group_by(ICB.NAME) %>%
  summarise(
    Registered = sum(Number.of.Registered.Patients..Last.Known.Figure., na.rm = TRUE),
    Weighted = sum(Number.of.Weighted.Patients..Last.Known.Figure., na.rm = TRUE)
  )

agg$Difference <- agg$Weighted - agg$Registered
# calculate percentsge change in weighted patients
agg$Percent.change <- agg$Difference / agg$Registered

# sort by ascending difference
agg <- agg[order(agg$Percent.change), ]

# create barchart of registered patients per ICB
# Create the barchart
my_colors <- colorRampPalette(c("#1B2C57", "#00A865"))(42)

agg %>%
  mutate(ICB.NAME = factor(ICB.NAME, levels = unique(ICB.NAME))) %>%
  ggplot(aes(x = reorder(ICB.NAME, Percent.change), y = Percent.change, fill = ICB.NAME)) +
  geom_bar(stat = "identity") +
  labs(x = "", y = "Percent difference", title = "Percentage Difference between Registered and Weighted Patients") +
  theme_minimal() +
  theme(legend.position = "none", axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_y_continuous(labels = scales::percent) + # Change y-axis labels to percentages
  scale_fill_manual(values = rep(my_colors, length.out = 42))
```

![](README_files/figure-markdown_github/weighted%20patients-1.png)

This chart demonstrates that the ICBs with the greatest healthcare need
relative to population size are North East and North Cumbria,
Lincolnshire, and Cheshire and Merseyside.

This pattern reflects a combination of demographics, such as the number
of older people, socioeconomic disadvantage (such as the level of
poverty) and the health of the population.
