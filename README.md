# Health Equity Evidence Centre
This repository serves as a hub for data and analysis related to the Health Equity Evidence Centre project.

Raw and processed data is available in the data subdirectory. We have complete collated practice-level time-series for the data made available by [NHS Digital](https://digital.nhs.uk/):
- Index of Multiple Deprivation
- NHS Payments to General Practice
- GP Patient Survey

RShiny iframes are available in the shiny subdirectory.

Analyses and the relevant code is available in the analysis subdirectory.

## Analysis
### 1.	[Structural inequalities in primary care – the facts and figures](https://www.heec.co.uk/resource/structural-inequalities-primary-care/)

The factors determining the quality and quantity of primary care services vary across England. Here we analyse practice level data relating to the supply, demand, and need for primary care, according to the socioeconomic status of the patients served.

- **Code**:
  [healthcare_need](https://github.com/HealthEquityEvidenceCentre/HEEC/tree/main/analysis/healthcare_need)

- **Charts**:
  [QOF Shiny App](https://heec.shinyapps.io/QOF_shiny/)

### 2. [NHS payments to practices in the East of England](https://www.heec.co.uk/resource/nhs-payments-general-practice-east-england/)

In this resource, we explore structural inequalities in primary care at the ICB level in the East of England. We provide data on NHS payments to GP surgeries, payments per weighted patient and patient satisfaction, showing differences across socioeconomic groups.

- **Code**

- **Charts**:
[Payments per weighted patient](https://heec.shinyapps.io/Payments_shiny/)
[Total payments by type](https://heec.shinyapps.io/Type_shiny/)
[Overall experience by ICB](https://heec.shinyapps.io/Satisfaction_shiny/)

### 3. [What does the latest GP Patient Survey tell us about socio-economic inequalities in general practice?](https://www.heec.co.uk/resource/what-does-the-latest-gp-patient-survey-tell-us-about-socio-economic-inequalities-in-general-practice/)

Overall patient satisfaction with general practice has improved slightly according to the GP Patient Survey 2024, but remains substantially lower than pre-pandemic levels. Patient satisfaction is not the same across the country. Read more to understand inequalities in patient satisfaction from the latest data.

- **Code**:

- **Charts**:
[GPPS by ICB](https://heec.shinyapps.io/GPPS/)
[Overall satisfaction](https://heec.shinyapps.io/overall_shiny/)
[Experience contacting surgery](https://heec.shinyapps.io/access_shiny/)
[Continuity](https://heec.shinyapps.io/continuity_shiny/)
[Confidence](https://heec.shinyapps.io/trust_shiny/)

### 4. [Exploring the impact of dispensing practices on equity in NHS payments to general practices](https://www.heec.co.uk/resource/exploring-the-impact-of-dispensing-practicing-on-equity-in-nhs-payments-to-general-practices/)

General practices serving the most deprived populations receive less funding per weighted patient than those serving the least deprived. Here we show that this inequality is driven by a higher concentration of dispensing practices in more affluent areas.

- **Code**:
[dispensing](https://github.com/HealthEquityEvidenceCentre/HEEC/tree/main/analysis/dispensing)

- **Charts**:
  ![Dispensing Chart](https://github.com/HealthEquityEvidenceCentre/HEEC/raw/main/analysis/dispensing/README_files/figure-markdown_github/unnamed-chunk-5-1.png)
  ![Comparison chart](https://github.com/HealthEquityEvidenceCentre/HEEC/raw/main//analysis/dispensing/README_files/figure-markdown_github/unnamed-chunk-7-1.png)
  nvd3
  Partner income

### 5. [General Practice Inequalities Datapacks](https://www.heec.co.uk/resource/general-practice-inequalities-datapacks/)

There are stark inequalities in the supply, demand and need of general practice. ICBs can take action to address these inequalities.

We’ve developed datapacks for each ICB England to help them understand their inequalities and take action.

Below are the datapacks for the East of England ICBs. If you’d like a copy of your own ICBs datapack, please email us contact@heec.co.uk

Within the datapacks, we calculate the disparity between practices serving the most and least deprived patients for each ICB, across the following categories:

Resources (supply): Payments, Workforce
Population (demand): Disease prevalence, Health-related behaviours
Service quality: QOF achievement
Access: Patient experience, Appointments
Impact on secondary care: Emergency admissions, A&E attendances

- **Code**:
[datapacks](https://github.com/HealthEquityEvidenceCentre/HEEC/tree/main/analysis/datapacks)

- **Charts**:
![Overview](https://github.com/HealthEquityEvidenceCentre/HEEC/blob/main/datapacks/slides_files/figure-commonmark/overview-1.png)

### 6. [Structural inequalities in General Practice in England]
Strong primary care is associated with more equitable health outcomes.

A key role of commissioners is to ensure the equitable distribution of resources across the system.

We present the latest NHS primary care data, using Index of Multiple Deprivation (IMD) to examine inequalities existing in primary care access, experience and outcomes, across the following categories:

Resources (supply): Payments, Workforce
Population (demand): Disease prevalence, Health-related behaviours
Service quality: QOF achievement
Access: Patient experience, Appointments
Impact on secondary care: Emergency admissions, A&E attendances
This analysis was produced by the Health Equity Evidence Centre. Additional data and analysis is available on GitHub.

For further information or to discuss the results, please contact Dr John Ford or Mr Cameron Appel.

- **Code**:
[Health Inequalities Notebook](https://github.com/nhs-r-community/health-inequalities-notebook)

- **Charts**:
[Health Inequalities Notebook](https://health-inequalities.nhsrcommunity.com/england.html)

### 6. [How does the age structure of patients affect NHS payments to General Practice?](https://www.heec.co.uk/resource/how-does-the-age-structure-of-patients-affect-nhs-payments-to-general-practice/)

In 2023/24, 6,669 practices received £10.2 billion from the NHS. Capitation payments to individual practices are adjusted using the Carr-Hill formula. On average, practices received £164.64 per patient, with higher payments for practices serving older populations due to higher healthcare needs, prescribing costs and the specific needs of rural areas. This analysis explores how NHS payments to general practices are informed by the age structure, deprivation and rurality of registered patients.

- **Code**:
[age_structure](https://github.com/HealthEquityEvidenceCentre/HEEC/tree/main/analysis/age_structure)

- **Charts**:
nvd3

### 7. [Dispensing Practices, NHS Funding, and the Geography of Inequality](https://www.heec.co.uk/resource/dispensing-practices-nhs-funding-and-the-geography-of-inequality/)
In 2023, 6,669 general practices received £10.2 billion in NHS funding across England, increasing to £11 billion with COVID-related and Primary Care Network (PCN) payments. Almost 10% of this—£870 million—was allocated to prescribing- and dispensing-related payments, supporting 944 dispensing practices serving 9.5 million patients (£625 million) and 5,537 non-dispensing practices covering 53 million patients (£245 million).

- **Code**:
[dispensing_2]()

- **Charts**:

### 8. [Understanding the Index of Multiple Deprivation (IMD) in public health research](https://www.heec.co.uk/resource/understanding-the-index-of-multiple-deprivation-imd-in-public-health-research/)

The Index of Multiple Deprivation (IMD) is a widely used measure in public health research and policymaking relating to health inequalities. By identifying areas with the greatest levels of deprivation, resources can be allocated more effectively to tackle systemic issues that contribute to unequal health outcomes. This blog provides an overview of the IMD and it’s use in public health research.

### 9. Sorry we’re closed: Exploring general practice closures

In 2019, there were 7,029 General Practice surgeries in England, providing essential healthcare to more than 60 million patients. However, recent data reveals a concerning trend: this number has fallen by over 10%, leaving just 6,256 practices in operation. While many of these closures represented mergers in with other practices, 193 (24%) shut down completely, leaving no direct replacement. This has displaced an estimated 718,000 – equivalent to nearly 1 in every 80 patients in England – forcing them to seek new healthcare providers.

As the number of surgeries dwindled, the strain on existing practices increased. Over the same period, the average GP list size grew from 8,737 to 9,613 patients. The practices that closed were, on average, significantly smaller, with around 4,004 per surgery. At the same time, the structure of NHS workforce has shifted dramatically, with the number of salaried GPs surpassing the number of GP partners for the first time in NHS history. This shift signals a move away from traditional, independently managed practices toward a system increasingly dominated by large, consolidate healthcare providers.

- **Code**

- **Charts**

## Datasets

| Dataset Name | Source | Current Release | Notes |
|--------------|--------|-----------------|-------|
| IMD | [DoHSC](https://www.gov.uk/government/collections/english-indices-of-deprivation) | 2019-09-26 | [Check for latest](#how-to-check-for-updates) |
| GP Earnings | v2.3 | YYYY-MM-DD | [Check for latest](#how-to-check-for-updates) |