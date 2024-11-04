---
author:
- Health Equity Evidence Centre
params:
  ICB_NAME: England
subtitle: Data from latest period for Suffolk and North East Essex
title: Primary Care Equity Datapack
toc-title: Table of contents
---

-   [Introduction](#introduction){#toc-introduction}
-   [ICB Overview](#icb-overview){#toc-icb-overview}
-   [Inequality in Life
    Expectancy](#inequality-in-life-expectancy){#toc-inequality-in-life-expectancy}

::: cell
`<details>
<summary>`{=html}Code`</summary>`{=html}

``` {.r .cell-code}
# Load necessary libraries
library(ggplot2)
library(showtext)
```

```{=html}
</details>
```
::: {.cell-output .cell-output-stderr}
    Loading required package: sysfonts
:::

::: {.cell-output .cell-output-stderr}
    Loading required package: showtextdb
:::

`<details>
<summary>`{=html}Code`</summary>`{=html}

``` {.r .cell-code}
library(patchwork)
library(ggtext)
library(fingertipsR)
library(tidyverse)
```

```{=html}
</details>
```
::: {.cell-output .cell-output-stderr}
    ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
    ✔ dplyr     1.1.2     ✔ readr     2.1.4
    ✔ forcats   1.0.0     ✔ stringr   1.5.1
    ✔ lubridate 1.9.2     ✔ tibble    3.2.1
    ✔ purrr     1.0.1     ✔ tidyr     1.3.0
:::

::: {.cell-output .cell-output-stderr}
    ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ✖ dplyr::filter() masks stats::filter()
    ✖ dplyr::lag()    masks stats::lag()
    ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors
:::

`<details>
<summary>`{=html}Code`</summary>`{=html}

``` {.r .cell-code}
library(purrr)
library(tibble)
library(spatstat)
```

```{=html}
</details>
```
::: {.cell-output .cell-output-stderr}
    Warning: package 'spatstat' was built under R version 4.3.3
:::

::: {.cell-output .cell-output-stderr}
    Loading required package: spatstat.data
:::

::: {.cell-output .cell-output-stderr}
    Warning: package 'spatstat.data' was built under R version 4.3.3
:::

::: {.cell-output .cell-output-stderr}
    Loading required package: spatstat.univar
:::

::: {.cell-output .cell-output-stderr}
    Warning: package 'spatstat.univar' was built under R version 4.3.3
:::

::: {.cell-output .cell-output-stderr}
    spatstat.univar 3.0-1
    Loading required package: spatstat.geom
:::

::: {.cell-output .cell-output-stderr}
    Warning: package 'spatstat.geom' was built under R version 4.3.3
:::

::: {.cell-output .cell-output-stderr}
    spatstat.geom 3.3-2

    Attaching package: 'spatstat.geom'

    The following object is masked from 'package:patchwork':

        area

    Loading required package: spatstat.random
:::

::: {.cell-output .cell-output-stderr}
    Warning: package 'spatstat.random' was built under R version 4.3.3
:::

::: {.cell-output .cell-output-stderr}
    spatstat.random 3.3-1
    Loading required package: spatstat.explore
:::

::: {.cell-output .cell-output-stderr}
    Warning: package 'spatstat.explore' was built under R version 4.3.3
:::

::: {.cell-output .cell-output-stderr}
    Loading required package: nlme

    Attaching package: 'nlme'

    The following object is masked from 'package:dplyr':

        collapse

    spatstat.explore 3.3-2
    Loading required package: spatstat.model
:::

::: {.cell-output .cell-output-stderr}
    Warning: package 'spatstat.model' was built under R version 4.3.3
:::

::: {.cell-output .cell-output-stderr}
    Loading required package: rpart
    spatstat.model 3.3-1
    Loading required package: spatstat.linnet
:::

::: {.cell-output .cell-output-stderr}
    Warning: package 'spatstat.linnet' was built under R version 4.3.3
:::

::: {.cell-output .cell-output-stderr}
    spatstat.linnet 3.2-1

    spatstat 3.1-1 
    For an introduction to spatstat, type 'beginner' 
:::

`<details>
<summary>`{=html}Code`</summary>`{=html}

``` {.r .cell-code}
library(lubridate)
library(readODS)
```

```{=html}
</details>
```
::: {.cell-output .cell-output-stderr}
    Warning: package 'readODS' was built under R version 4.3.3
:::

`<details>
<summary>`{=html}Code`</summary>`{=html}

``` {.r .cell-code}
library(janitor)
```

```{=html}
</details>
```
::: {.cell-output .cell-output-stderr}

    Attaching package: 'janitor'

    The following objects are masked from 'package:stats':

        chisq.test, fisher.test
:::

`<details>
<summary>`{=html}Code`</summary>`{=html}

``` {.r .cell-code}
library(gdata)
```

```{=html}
</details>
```
::: {.cell-output .cell-output-stderr}

    Attaching package: 'gdata'

    The following objects are masked from 'package:dplyr':

        combine, first, last, starts_with

    The following object is masked from 'package:purrr':

        keep

    The following object is masked from 'package:tidyr':

        starts_with

    The following object is masked from 'package:stats':

        nobs

    The following object is masked from 'package:utils':

        object.size

    The following object is masked from 'package:base':

        startsWith
:::

`<details>
<summary>`{=html}Code`</summary>`{=html}

``` {.r .cell-code}
# Set up Google font and automatic display for text rendering
font_add_google("Poppins", family = "Poppins")
showtext_auto()

# Set up the ggplot theme
theme_set(
  theme_minimal() +
    theme(
      axis.title = element_text(size = 80, family = "Poppins"),
      axis.text = element_text(size = 80, family = "Poppins"),
      plot.caption = element_text(size = 60, family = "Poppins"),
      plot.title = element_text(size = 100, family = "Poppins"),
      plot.subtitle = element_text(size = 90, family = "Poppins"),
      panel.grid = element_line(size = 5),
      legend.title = element_text(size = 60, family = "Poppins"),
      legend.text = element_text(size = 60, family = "Poppins"),
      legend.key.width = unit(3.5, "cm"),
      plot.title.position = "plot"
    )
)
```

```{=html}
</details>
```
::: {.cell-output .cell-output-stderr}
    Warning: The `size` argument of `element_line()` is deprecated as of ggplot2 3.4.0.
    ℹ Please use the `linewidth` argument instead.
:::

`<details>
<summary>`{=html}Code`</summary>`{=html}

``` {.r .cell-code}
# Update default settings for geom_point size
update_geom_defaults("point", list(size = 50))

# Import data
df <- read.csv("final_data.csv")

# Define helper functions or custom operators if needed
"%ni%" <- Negate("%in%")

# Optional: suppress warnings and set figure dimensions globally for knitr
knitr::opts_chunk$set(
  echo = FALSE,
  warning = FALSE,
  fig.width = 90,
  fig.height = 25,
  fig.fullwidth = TRUE
)
```

```{=html}
</details>
```
:::

## Introduction

Primary care, in contrast to specialty care, is associated with a more
equitable distribution of health across populations. One of the key
roles of commissioners is to ensure that resources are distributed
equitably across the healthcare system.

This report presents the latest NHS primary care data, using the Index
of Multiple Deprivation (IMD) to examine inequalities in patient health
determinants and outcomes. We analyze the data according to the
following categories:

-   **Resources (supply)**: Payments, Workforce
-   **Population (demand)**: Disease prevalence, Health-related
    behaviors
-   **Service quality**: Quality and Outcomes Framework (QOF)
    achievement
-   **Access**: Patient experience, Appointments
-   **Impact on secondary care**: Emergency admissions, A&E attendances

For further information or to discuss the results, please contact
[Dr. John Ford](mailto:j.a.ford@qmul.ac.uk).

## ICB Overview

::: cell
::: cell-output-display
![](slides_files/figure-markdown/overview-1.png)
:::
:::

Each practice in England is assigned an Index of Multiple Deprivation
(IMD) based on the population it serves, divided into deprivation
quintiles.

For Suffolk and North East Essex, approximately **7%** of practices
serve the most deprived quintile of patients in England.

## Inequality in Life Expectancy

::: cell
::: cell-output-display
![](slides_files/figure-markdown/Life_Expectancy-1.png)
:::
:::

Average life expectancy for men is in the least deprived 20%.
