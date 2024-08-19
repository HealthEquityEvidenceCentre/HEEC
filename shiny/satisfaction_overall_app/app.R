library(shiny)
library(ggplot2)
library(dplyr)

# Define UI for application
ui <- fluidPage(
  fluidRow(
    column(
      12,
      selectInput("icb", "Select ICB", choices = NULL)
    ),
    column(
      12,
      plotOutput("satisfactionPlot", height = "600px")
    )
  )
)

# Define server logic required to draw the plot
server <- function(input, output, session) {
  # Read and preprocess data
  data <- reactive({
    satisfaction <- read.csv("satisfaction_link")

    # Calculate quintiles at ICB level
    satisfaction_quintile <- satisfaction[!is.na(satisfaction$ICB.NAME), ] %>%
      select(-IMD_quintile) %>%
      group_by(ICB.NAME) %>%
      mutate(IMD_quintile = ntile(IMD, 5))

    # Aggregate data at ICB level
    agg_icb <- satisfaction_quintile %>%
      group_by(Year, IMD_quintile, ICB.NAME) %>%
      summarise(
        mean_overall = mean(overall_pct, na.rm = TRUE),
        mean_continuity = mean(continuity_pct, na.rm = TRUE),
        mean_trust = mean(trust_pct, na.rm = TRUE),
        mean_access = mean(access_pct, na.rm = TRUE),
        .groups = "drop"
      )

    agg_icb$IMD_quintile <- as.factor(agg_icb$IMD_quintile)

    # Calculate overall statistics for England
    agg_eng <- satisfaction %>%
      group_by(Year, IMD_quintile) %>%
      summarise(
        mean_overall = mean(overall_pct, na.rm = TRUE),
        mean_continuity = mean(continuity_pct, na.rm = TRUE),
        mean_trust = mean(trust_pct, na.rm = TRUE),
        mean_access = mean(access_pct, na.rm = TRUE),
        .groups = "drop"
      )

    agg_eng <- agg_eng %>%
      mutate(ICB.NAME = "England")

    agg_eng$IMD_quintile <- as.factor(agg_eng$IMD_quintile)

    # Combine ICB and England data
    agg_combined <- bind_rows(agg_icb, agg_eng)
    agg_combined$IMD_quintile <- as.factor(agg_combined$IMD_quintile)

    agg_combined
  })

  # Update the ICB selection choices
  observe({
    icb_choices <- unique(data()$ICB.NAME)
    default_icb <- ifelse("England" %in% icb_choices, "England", icb_choices[1])
    updateSelectInput(session, "icb", choices = icb_choices, selected = default_icb)
  })

  # Render the plot based on selected ICB
  output$satisfactionPlot <- renderPlot({
    agg <- data()
    colors <- c("#EF7A34", "#00A865", "#007AA8", "#531A5C", "#A80026")

    ggplot(agg[!is.na(agg$IMD_quintile) & agg$ICB.NAME == input$icb, ], aes(x = Year, y = mean_overall, group = IMD_quintile, colour = IMD_quintile)) +
      geom_line(size = 1.5) +
      geom_point(size = 3) +
      labs(x = "", y = "Percentage responding 'Good'", title = paste("Overall satisfaction with general practice -", input$icb), color = "IMD quintile") +
      theme_minimal() +
      theme(
        plot.title = element_text(size = 24),
        legend.position = "bottom",
        legend.justification = "center",
        legend.title = element_text(size = 20),
        legend.text = element_text(size = 16),
        axis.text.y = element_text(size = 16),
        axis.title.y = element_text(size = 20),
        axis.text.x = element_text(size = 16),
        axis.line.x = element_line(size = 1),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank()
      ) +
      scale_color_manual(values = colors, labels = c("Q1 (least deprived)", "Q2", "Q3", "Q4", "Q5 (most deprived)")) +
      scale_y_continuous(labels = scales::percent_format()) +
      scale_x_continuous(breaks = unique(agg$Year))
  })
}

# Run the application
shinyApp(ui = ui, server = server)
