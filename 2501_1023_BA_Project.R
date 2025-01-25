library(shiny)
library(shinyjs) # For enabling/disabling inputs and showing messages
library(ggplot2)
library(forecast)
library(plotly)

# Load and preprocess the dataset
data <- read.csv("C:\\Users\\Khalid Laptop\\Desktop\\Project_BA_Khalid&Zuhair_Final\\file_02.csv")

# Data cleaning
cols_to_clean <- c("Thermal.Generation.Actual..in.MU.", 
                   "Thermal.Generation.Estimated..in.MU.")

data[cols_to_clean] <- lapply(data[cols_to_clean], function(x) as.numeric(gsub(",", "", x)))
data$Date <- as.Date(data$Date, format = "%Y-%m-%d")

# UI definition
ui <- fluidPage(
  useShinyjs(), # Include shinyjs
  
  # Title and description
  div(style = "background-color: #003f5c; color:white; padding: 20px; border-radius: 10px; margin-top: 30px; margin-bottom: 20px;",
      titlePanel("Time Series Analysis Dashboard for Regional Power Generation in India (2017-2020)"),
      p("This dashboard provides an interactive visualization of energy generation across different regions and types (Hydro, Thermal, Nuclear, or Total). Users can select the type of generation and region, specify the forecast horizon, and generate forecasts using the ARIMA model."),
      
      p("The plot shows both historical and forecasted data, along with confidence intervals for the forecast. The table below the plot displays the forecast details, including the forecasted values and the lower and upper bounds of the confidence intervals. Click the 'Generate Forecast' button to see the forecasted generation for the selected type and region.")
  ),
  
  # Input panel
  div(id = "input-panel",
      fluidRow(
        column(4, 
               selectInput("type", "Select Type:", 
                           choices = c("Hydro", "Thermal", "Nuclear", "Total"), 
                           selected = "Total")),
        column(4, 
               selectInput("region", "Select Region:", 
                           choices = unique(data$Region), 
                           selected = "North")),
        column(4, 
               sliderInput("forecastHorizonSlider", "Forecast Days:", 
                           min = 1, max = 365, value = 365))
      ),
      fluidRow(
        column(12, 
               div(style = "text-align: center; margin-top: 20px; margin-bottom: 30px;", 
                   actionButton("generateBtn", "Generate Forecast", class = "btn-primary"))),
      ),
      
      # Loading message
      div(id = "loading-message", 
          style = "text-align: center; font-size: 18px; color: #ff6347; font-weight: bold; margin-top: 20px; margin-bottom: 30px; display: none;", 
          "Generating forecast, please wait..."),
      
      # No data message
      div(id = "no-data-message", 
          style = "text-align: center; font-size: 18px; color: #ff0000; font-weight: bold; margin-top: 20px; display: none;", 
          "No data available for the selected area and type.")
  ),
  
  # Display total and daily power generation
  div(style = "margin-bottom: 30px;",
      fluidRow(
        column(6, 
               div(style = "background-color: #EA6A47; color: white; padding: 15px; border-radius: 10px; text-align: center;", 
                   h4(textOutput("totalPowerGeneration")))),
        column(6, 
               div(style = "background-color: #0091D5; color: white; padding: 15px; border-radius: 10px; text-align: center;", 
                   h4(textOutput("dailyPowerGeneration"))))
      )
  ),
  
  # Plot and forecast table
  div(style = "background-color: #f7f7f7; padding: 20px; padding-bottom: 40px; border-radius: 10px; border: 2px solid #ccc; margin-top: 20px; margin-bottom: 40px;",
      fluidRow(
        column(12, plotlyOutput("timeSeriesForecastPlot"))
      ),
      fluidRow(
        column(12, 
               div(style = "display: flex; justify-content: center; margin-top: 20px;",
                   tableOutput("forecastTable"))))
  )
)

# Server logic
server <- function(input, output, session) {
  # Reactive values to store user selections
  selected_inputs <- reactiveValues(
    type = "Total",
    region = "North",
    forecast_horizon = 365
  )
  
  # Observe button click to update stored inputs and check data
  observeEvent(input$generateBtn, {
    disable("input-panel")
    show("loading-message")
    hide("plot-generated-message")
    hide("no-data-message")
    
    selected_inputs$type <- input$type
    selected_inputs$region <- input$region
    selected_inputs$forecast_horizon <- input$forecastHorizonSlider
    
    # Check if data is available for the selected type and region
    type_col <- switch(selected_inputs$type,
                       "Hydro" = "Hydro.Generation.Actual..in.MU.",
                       "Thermal" = "Thermal.Generation.Actual..in.MU.",
                       "Nuclear" = "Nuclear.Generation.Actual..in.MU.",
                       "Total" = NULL)
    
    if (!is.null(type_col) && all(is.na(data[data$Region == selected_inputs$region, type_col]))) {
      show("no-data-message")
      enable("input-panel")
      hide("loading-message")
    }
  })
  
  # Filtered data
  filteredData <- reactive({
    type_col <- switch(selected_inputs$type,
                       "Hydro" = "Hydro.Generation.Actual..in.MU.",
                       "Thermal" = "Thermal.Generation.Actual..in.MU.",
                       "Nuclear" = "Nuclear.Generation.Actual..in.MU.",
                       "Total" = NULL)
    
    if (!is.null(type_col)) {
      plot_data <- data[!is.na(data[[type_col]]), ]
      plot_data <- plot_data[plot_data$Region == selected_inputs$region, c("Date", type_col)]
      colnames(plot_data) <- c("Date", "Value")
    } else {
      plot_data <- data[data$Region == selected_inputs$region, ]
      plot_data$Value <- rowSums(plot_data[, c("Hydro.Generation.Actual..in.MU.",
                                               "Thermal.Generation.Actual..in.MU.",
                                               "Nuclear.Generation.Actual..in.MU.")], 
                                 na.rm = TRUE)
      plot_data <- plot_data[, c("Date", "Value")]
    }
    plot_data
  })
  
  # ARIMA forecast
  precomputed_forecasts <- reactive({
    plot_data <- filteredData()
    
    if (nrow(plot_data) == 0) return(NULL)
    
    ts_data <- ts(plot_data$Value, 
                  start = c(as.numeric(format(min(plot_data$Date), "%Y")), 
                            as.numeric(format(min(plot_data$Date), "%j"))), 
                  frequency = 365)
    
    fit <- auto.arima(ts_data)
    forecasted <- forecast(fit, h = selected_inputs$forecast_horizon, level = c(80, 95))
    
    forecast_dates <- seq(max(plot_data$Date) + 1, by = "days", length.out = selected_inputs$forecast_horizon)
    forecast_df <- data.frame(
      Date = forecast_dates, 
      Value = as.numeric(forecasted$mean), 
      Lower_80 = forecasted$lower[, 1], 
      Upper_80 = forecasted$upper[, 1], 
      Lower_95 = forecasted$lower[, 2], 
      Upper_95 = forecasted$upper[, 2]
    )
    list(forecast_df = forecast_df, ts_data = ts_data)
  })
  
  # Total Power Generation
  output$totalPowerGeneration <- renderText({
    plot_data <- filteredData()
    total_power <- sum(plot_data$Value, na.rm = TRUE)
    paste("Total Power Generation: ", round(total_power, 2), " MU")
  })
  
  # Daily Power Generation
  output$dailyPowerGeneration <- renderText({
    plot_data <- filteredData()
    avg_daily_power <- mean(plot_data$Value, na.rm = TRUE)
    paste("Daily Power Generation (Average): ", round(avg_daily_power, 2), " MU")
  })
  
  # Time Series Plot
  output$timeSeriesForecastPlot <- renderPlotly({
    forecast_data <- precomputed_forecasts()
    if (is.null(forecast_data)) return(NULL)
    
    plot_data <- filteredData()
    forecast_df <- forecast_data$forecast_df
    
    plot <- ggplot() +
      geom_line(data = plot_data, aes(x = Date, y = Value), color = "blue", size = 1) +
      geom_line(data = forecast_df, aes(x = Date, y = Value), color = "red", size = 1) +
      geom_ribbon(data = forecast_df, aes(x = Date, ymin = Lower_95, ymax = Upper_95), 
                  fill = "red", alpha = 0.2) +
      geom_ribbon(data = forecast_df, aes(x = Date, ymin = Lower_80, ymax = Upper_80), 
                  fill = "orange", alpha = 0.3) +
      labs(title = paste("Time Series and ARIMA Forecast for", 
                         selected_inputs$type, "Generation in", 
                         selected_inputs$region),
           x = "Date", y = "Generation (in MU)") +
      theme_minimal()
    
    ggplotly(plot)
  })
  
  # Forecast Table
  output$forecastTable <- renderTable({
    forecast_data <- precomputed_forecasts()
    if (is.null(forecast_data)) return(NULL)
    
    forecast_df <- forecast_data$forecast_df
    forecast_df$Date <- format(forecast_df$Date, "%d-%b-%Y")
    head_tail_data <- rbind(head(forecast_df, 5), tail(forecast_df, 5))
    data.frame(
      Date = head_tail_data$Date,
      Forecasted_Value = head_tail_data$Value,
      Lower_80 = head_tail_data$Lower_80,
      Upper_80 = head_tail_data$Upper_80,
      Lower_95 = head_tail_data$Lower_95,
      Upper_95 = head_tail_data$Upper_95
    )
  })
  
  # Hide loading message and re-enable inputs after forecast
  observe({
    forecast_data <- precomputed_forecasts()
    if (!is.null(forecast_data)) {
      hide("loading-message")
      show("plot-generated-message")
      enable("input-panel")
    }
  })
}

# Run the app
shinyApp(ui = ui, server = server)
