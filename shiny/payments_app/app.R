library(shiny)

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

server <- function(input, output, session) {
  # Read and preprocess data
  data <- reactive({
    payments <- read.csv("payments_link")
  })
}

# Run the application
shinyApp(ui = ui, server = server)