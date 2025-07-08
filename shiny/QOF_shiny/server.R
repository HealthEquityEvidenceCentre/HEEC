library(reshape2)
library(magrittr)
library(dplyr)
library(ggplot2)
library(stringr)

colors <- c("#EF7A34", "#00A865", "#007AA8", "#531A5C", "#A80026")

prevalence <- read.csv("prevalence.csv")

condition_names <- c(
    "Atrial fibrillation" = "AF",
    "Asthma" = "AST",
    "Cancer" = "CAN",
    "Secondary prevention of coronary heart disease" = "CHD",
    "Chronic kidney disease" = "CKD",
    "Chronic obstructive pulmonary disease" = "COPD",
    "Cardiovascular disease primary prevention" = "CVDPP",
    "Dementia" = "DEM",
    "Depression" = "DEP",
    "Diabetes mellitus" = "DM",
    "Epilepsy" = "EP",
    "Heart failure" = "HF",
    "Hypertension" = "HYP",
    "Learning Disability" = "LD",
    "Mental health" = "MH",
    "Non-diabetic hyperglycaemia" = "NDH",
    "Obesity" = "OB",
    "Osteoporosis" = "OST",
    "Peripheral arterial disease" = "PAD",
    "Palliative care" = "PC",
    "Rheumatoid arthritis" = "RA",
    "Stroke and transient ischaemic attack" = "STIA"
)

function(input, output, session) {
    plot_reactive <- reactive({
        selected_condition <- condition_names[input$group]
        # Select the data for the selected ICB
        agg <- prevalence[prevalence$GROUP_CODE == selected_condition, ] %>%
            group_by(Year, IMD_decile) %>%
            summarise(
                PATIENT_LIST_SIZE = sum(PATIENT_LIST_SIZE, na.rm = TRUE),
                REGISTER = sum(REGISTER, na.rm = TRUE),
            ) %>%
            mutate(
                PREVALENCE = (REGISTER / PATIENT_LIST_SIZE) * 100
            )


        # make IMD_decile a factor
        agg$IMD_decile <- factor(agg$IMD_decile, levels = c("1", "2", "3", "4", "5"))

        agg[!is.na(agg$IMD_decile), ] %>%
            ggplot(aes(x = Year, y = PREVALENCE, group = IMD_decile, colour = IMD_decile)) +
            geom_line(size = 1.5) + # Adjust the size as needed
            geom_point(size = 3) +
            labs(x = "", y = "Prevalence (%)", title = paste0("Prevalence of ", input$group, " by IMD quintile (England)")) +
            theme_minimal() +
            theme(
                legend.position = "bottom",
                legend.justification = "center", # Adjust as needed
                axis.text.x = element_text(size = 10),
                axis.title.x = element_text(size = 12),
                axis.line.x = element_line(size = 1), # Adjust x-axis line thickness
                panel.grid.minor.x = element_blank(), # Remove minor vertical grid lines
                panel.grid.minor.y = element_blank(), # Remove minor horizontal grid lines
                # legend.text = element_text(size = 14),  # Increase legend text size
                # legend.title = element_text(size = 16),
                # legend.key.size = unit(1.5, "cm")  # Increase legend key size
            ) +
            scale_color_manual(values = colors, labels = c("Q1 (least deprived)", "Q2", "Q3", "Q4", "Q5 (most deprived)")) +
            labs(color = "IMD quintile") +
            scale_x_continuous(breaks = unique(agg$Year)) # Show every year on the x-axis
    })

    # Render the plot when the ICB selection changes
    output$plot <- renderPlot({
        plot_reactive()
    })
}
