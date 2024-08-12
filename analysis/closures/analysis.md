# Practice Closures Trigger Analysis

``` r
library(magrittr)
### Satisfaction analysis

### Workforce analysis
df15 <- read.csv("../../workforce/raw/15_9.csv")
df23 <- read.csv("../../workforce/raw/23_9.csv")

fte_15 <- df15$TOTAL_GP_EXTGL_FTE %>%
  as.numeric() %>%
  sum(na.rm = TRUE)
fte_23 <- df23$TOTAL_GP_EXTGL_FTE %>%
  as.numeric() %>%
  sum(na.rm = TRUE)
```

General practice is in crisis: patient satisfaction is at a record low;
the number of fully qualified GPs has dropped from 2.7368998^{4} in 2015
to 2.6452591^{4} in 2023 (full-time equivalent, excluding GPs in
training and locums).

``` r
n_prac_15 <- df15$PRAC_CODE %>%
  unique() %>%
  length()
n_prac_23 <- df23$PRAC_CODE %>%
  unique() %>%
  length()
```

Over the past decade there has been a move towards providing general
practice at scale to benefit from economies of scale and to ensure
practices are more sustainable. This has led to a dramatic reduction in
the number of practices from 7623 in 2015 to 6334 in 2023, according to
the workforce data.
