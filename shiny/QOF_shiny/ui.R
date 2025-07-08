library(shiny)
library(ggplot2)
library(magrittr)
library(dplyr)

prevalence <- read.csv("prevalence.csv")

# Create a dictionary to map original condition names to desired names
group <- prevalence$GROUP_CODE %>%
    unique() %>%
    sort()

group <- c(
    "Atrial fibrillation",
    "Asthma",
    "Cancer",
    "Secondary prevention of coronary heart disease",
    "Chronic kidney disease",
    "Chronic obstructive pulmonary disease",
    "Cardiovascular disease primary prevention",
    "Dementia",
    "Depression",
    "Diabetes mellitus",
    "Epilepsy",
    "Heart failure",
    "Hypertension",
    "Learning Disability",
    "Mental health",
    "Non-diabetic hyperglycaemia",
    "Obesity",
    "Osteoporosis",
    "Peripheral arterial disease",
    "Palliative care",
    "Rheumatoid arthritis",
    "Stroke and transient ischaemic attack"
)

fluidPage(
    titlePanel(""),
    sidebarLayout(
        sidebarPanel(
            selectInput(
                inputId = "group",
                label = "Select condition:",
                choices = group
            )
        ),
        mainPanel(
            plotOutput(outputId = "plot", width = "100%", height = "600px")
        )
    )
)
