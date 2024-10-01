library(shiny)
library(ggplot2)
library(dplyr)

icb <- c(
  "Bath and North East Somerset, Swindon and Wiltshire",
  "Bedfordshire, Luton and Milton Keynes",
  "Birmingham and Solihull",
  "Black Country",
  "Bristol, North Somerset and South Gloucestershire",
  "Buckinghamshire, Oxfordshire and Berkshire West",
  "Cambridgeshire and Peterborough",
  "Cheshire and Merseyside",
  "Cornwall and the Isles of Scilly",
  "Coventry and Warwickshire",
  "Derby and Derbyshire",
  "Devon",
  "Dorset",
  "Frimley",
  "Gloucestershire",
  "Greater Manchester",
  "Hampshire and Isle of Wight",
  "Herefordshire and Worcestershire",
  "Hertfordshire and West Essex",
  "Humber and North Yorkshire",
  "Kent and Medway",
  "Lancashire and South Cumbria",
  "Leicester, Leicestershire and Rutland",
  "Lincolnshire",
  "Mid and South Essex",
  "Norfolk and Waveney",
  "North Central London",
  "North East and North Cumbria",
  "North East London",
  "North West London",
  "Northamptonshire",
  "Nottingham and Nottinghamshire",
  "Shropshire, Telford and Wrekin",
  "Somerset",
  "South East London",
  "South West London",
  "South Yorkshire",
  "Staffordshire and Stoke-on-Trent",
  "Suffolk and North East Essex",
  "Surrey Heartlands",
  "Sussex",
  "West Yorkshire"
)

type <- c(
  "Total",
  "Prescribing",
  "Quality and Outcomes Framework",
  "Primary Care Organisation payments, training, and other",
  "COVID",
  "Contracted services",
  "IT and premises",
  "Global Sum"
)

type_labels <- c(
  "Total" = "Total.NHS.Payments.to.General.Practice.per.weighted.patient",
  "Global Sum" = "Total.Global.Sum.per.weighted.patient",
  "IT and premises" = "Total.IT.Premises.per.weighted.patient",
  "Contracted services" = "Total.Contracted.Services.per.weighted.patient",
  "Primary Care Organisation payments, training, and other" = "Total.PCO.per.weighted.patient",
  "Quality and Outcomes Framework" = "Total.QOF.Payments.per.weighted.patient",
  "Prescribing" = "Total.Prescribing.per.weighted.patient"
)

ui <- fluidPage(
  titlePanel(""),
  div(
    sidebarLayout(
      sidebarPanel(
        selectInput(
          inputId = "icb",
          label = "Select ICB:",
          choices = icb
        ),
        selectInput(
          inputId = "type",
          label = "Select Payment Type:",
          choices = type
        )
      ),
      mainPanel(
        plotOutput(outputId = "plot", width = "100%", height = "600px")
      )
    ),
    div(
      textOutput("hello_text"),
      style = "position: relative; top: 10px; left: 10px; width: 100%; font-family: 'Poppins', sans-serif; font-size: 12px;"
    )
  ),
  tags$head(
    tags$style(HTML("
      body {
        font-family: 'Poppins', sans-serif;
        font-size: 12px;
      }
    "))
  )
)

colors <- c("#EF7A34", "#00A865", "#007AA8", "#531A5C", "#A80026")

server <- function(input, output, session) {
  # Read and preprocess data
  plot_reactive <- reactive({
    payments <- read.csv("payments_link")  
    
    selected_type_label <- type_labels[input$type]
    
    # Filter and process the data
    agg <- payments %>%
      filter(ICB.NAME == input$icb) %>%
      group_by(Year, IMD_quintile) %>%
      summarise(
        Total.NHS.Payments.to.General.Practice = sum(Total.NHS.Payments.to.General.Practice, na.rm = TRUE),
        Total.Global.Sum = sum(Total.Global.Sum, na.rm = TRUE),
        Total.IT.Premises = sum(Total.IT.Premises, na.rm = TRUE),
        Total.Contracted.Services = sum(Total.Contracted.Services, na.rm = TRUE),
        Total.PCO = sum(Total.PCO, na.rm = TRUE),
        Total.QOF.Payments = sum(Total.QOF.Payments, na.rm = TRUE),
        Total.Prescribing = sum(Total.Prescribing, na.rm = TRUE),
        Number.of.Weighted.Patients..Last.Known.Figure. = sum(Number.of.Weighted.Patients..Last.Known.Figure., na.rm = TRUE),
      ) %>%
      mutate(
        Total.NHS.Payments.to.General.Practice.per.weighted.patient = Total.NHS.Payments.to.General.Practice / Number.of.Weighted.Patients..Last.Known.Figure.,
        Total.Global.Sum.per.weighted.patient = Total.Global.Sum / Number.of.Weighted.Patients..Last.Known.Figure.,
        Total.IT.Premises.per.weighted.patient = Total.IT.Premises / Number.of.Weighted.Patients..Last.Known.Figure.,
        Total.Contracted.Services.per.weighted.patient = Total.Contracted.Services / Number.of.Weighted.Patients..Last.Known.Figure.,
        Total.PCO.per.weighted.patient = Total.PCO / Number.of.Weighted.Patients..Last.Known.Figure.,
        Total.QOF.Payments.per.weighted.patient = Total.QOF.Payments / Number.of.Weighted.Patients..Last.Known.Figure.,
        Total.Prescribing.per.weighted.patient = Total.Prescribing / Number.of.Weighted.Patients..Last.Known.Figure.
      ) %>%
      ungroup() %>%  
      mutate(IMD_quintile = factor(IMD_quintile))
    
    n_practices <- summarise(group_by(payments[payments$ICB.NAME == input$icb & payments$Year == 2023, ], IMD_quintile), n = n())[5, 2]
    
    if (is.na(n_practices)) {
      text <- paste0("None of the practices in ", input$icb, " serve patients who are predominantly in the most deprived 20% of the national population, and only ", summarise(group_by(payments[payments$ICB.NAME == input$icb & payments$Year == 2023, ], IMD_quintile), n = n())[4, 2], "  practices predominantly serve those in the second highest national quintile of deprivation. In 2023, these practices received £", format(round(agg[agg$Year == 2023, ][1, 17] - agg[agg$Year == 2023, ][4, 17], digits = 2), nsmall = 2), " fewer total payments per weighted patient than those serving the least deprived 20%. The difference in prescribing payments was £", round(agg[agg$Year == 2023, ][1, 16] - agg[agg$Year == 2023, ][4, 16], digits = 2), ".")
    } else {
      text <- paste0(summarise(group_by(payments[payments$ICB.NAME == input$icb & payments$Year == 2023, ], IMD_quintile), n = n())[5, 2], " practices in ", input$icb, " predominantly serve the most deprived 20% of the national population. In 2023, these practices received £", round(agg[agg$Year == 2023, ][1, 17] - agg[agg$Year == 2023, ][5, 17], digits = 2), " fewer total payments per weighted patient than those serving the least deprived 20%. The difference in prescribing payments was £", format(round(agg[agg$Year == 2023, ][1, 16] - agg[agg$Year == 2023, ][5, 16], digits = 2), nsmall = 2), ".")
    }
    
    # Return the processed data
    return(list(
      agg = agg[!is.na(agg$IMD_quintile), ],
      text = text,
      selected_type_label = selected_type_label
    ))
    
  })
  
  output$plot <- renderPlot({
    plot_data <- plot_reactive()$agg
    
    ggplot(plot_data, aes(x = Year, y = !!sym(type_labels[input$type]), group = IMD_quintile, colour = IMD_quintile)) +
      geom_line(size = 1.5) +
      geom_point(size = 3) +
      labs(x = "", y = "Average payment (£)", title = paste0(input$type, " payments per weighted patient by IMD quintile")) +
      theme_minimal() +
      theme(
        legend.position = "bottom",
        legend.justification = "center",
        axis.text.x = element_text(size = 10),
        axis.title.x = element_text(size = 12),
        axis.line.x = element_line(size = 1),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank()
      ) +
      scale_color_manual(values = colors, labels = c("Q1 (least deprived)", "Q2", "Q3", "Q4", "Q5 (most deprived)")) +
      labs(color = "IMD quintile") +
      scale_x_continuous(breaks = unique(plot_data$Year))  # Use plot_data$Year instead of agg$Year
  })
  
  # Render the "hello" text
  output$hello_text <- renderText({
    plot_reactive()$text
  })
}

# Run the application
shinyApp(ui = ui, server = server)