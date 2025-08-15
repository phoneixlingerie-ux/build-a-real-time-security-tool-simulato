# Load necessary libraries
library(shiny)
library(DT)
library(ggplot2)
library(plotly)

# Define UI
ui <- fluidPage(
  titlePanel("Real-Time Security Tool Simulator"),
  sidebarLayout(
    sidebarPanel(
      checkboxInput("checkbox", "Simulate Alerts"),
      sliderInput("alert_rate", "Alert Rate (per minute)", min = 1, max = 100, value = 10),
      sliderInput("threat_level", "Threat Level (1-10)", min = 1, max = 10, value = 5)
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Alerts", DT::dataTableOutput("alerts")),
        tabPanel("Threat Map", plotlyOutput("threat_map")),
        tabPanel("System Health", fluidRow(
          column(width = 6, gaugeOutput("cpu")),
          column(width = 6, gaugeOutput("memory"))
        ))
      )
    )
  )
)

# Define server logic
server <- function(input, output) {
  # Generate random alerts
  alerts <- reactive({
    if (input$checkbox) {
      n_alerts <- sample(1:10, 1)
      data.frame(
        Time = Sys.time() + runif(n_alerts, 0, 60),
        Type = sample(c("INFO", "WARNING", "CRITICAL"), n_alerts, replace = TRUE),
        Message = sample(c("Login attempt failed", "System update available", "Malware detected"), n_alerts, replace = TRUE)
      )
    } else {
      data.frame()
    }
  })
  
  # Update alerts table
  output$alerts <- DT::renderDataTable({
    alerts()
  })
  
  # Generate threat map
  output$threat_map <- renderPlotly({
    ggplot(data.frame(Threat = runif(100, 0, 10)), aes(x = Threat)) + 
      geom_histogram(bins = 10, fill = "red") + 
      labs(x = "Threat Level", y = "Frequency") + 
      theme Plotly()
  })
  
  # Update system health gauges
  output$cpu <- renderGauge({
    gauge(50, min = 0, max = 100, symbol = '%')
  })
  
  output$memory <- renderGauge({
    gauge(30, min = 0, max = 100, symbol = '%')
  })
}

# Run the application
shinyApp(ui = ui, server = server)