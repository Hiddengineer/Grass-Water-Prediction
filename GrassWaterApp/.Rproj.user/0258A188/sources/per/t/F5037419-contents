# --------------------------------------------
# Lawn Watering Predictor App with OpenWeatherMap API
# --------------------------------------------

library(shiny)
library(httr)
library(jsonlite)
library(dplyr)
library(ggplot2)
library(tibble)

# Evapotranspiration estimation function
estimate_et <- function(temp, humidity) {
  base_et <- 5
  adjustment <- (temp - 20) * 0.2 - (humidity - 50) * 0.1
  max(0, base_et + adjustment)
}

# UI
ui <- fluidPage(
  titlePanel("🌿 Lawn Watering Predictor (5-Day Forecast)"),
  sidebarLayout(
    sidebarPanel(
      textInput("api_key", "OpenWeatherMap API Key:", value = "c009c9afcefa41ed37e0ded9a65d02ca"),
      numericInput("lat", "Latitude:", value = 46.2112),
      numericInput("lon", "Longitude:", value = -119.1372),
      numericInput("area", "Lawn Area (m²):", value = 50),
      actionButton("fetch", "Fetch Forecast & Calculate")
    ),
    mainPanel(
      h4("💧 Predicted Watering Needs"),
      tableOutput("forecast_table"),
      plotOutput("water_plot")
    )
  )
)

# Server logic
server <- function(input, output) {
  observeEvent(input$fetch, {
    req(input$api_key)
    
    url <- paste0("https://api.openweathermap.org/data/2.5/forecast?lat=",
                  input$lat, "&lon=", input$lon,
                  "&appid=", input$api_key, "&units=metric")
    
    response <- try(GET(url), silent = TRUE)
    
    if (inherits(response, "try-error") || response$status_code != 200) {
      showNotification("❌ Error fetching data. Check your API key or coordinates.", type = "error")
      return()
    }
    
    data <- fromJSON(content(response, "text", encoding = "UTF-8"))
    
    if (is.null(data$list) || nrow(data$list) == 0) {
      showNotification("⚠️ No forecast data returned.", type = "warning")
      return()
    }
    
    # Extract and clean forecast data
    rain_data <- tibble(
      time_val = data$list$dt_txt,
      temp_val = data$list$main$temp,
      humidity_val = data$list$main$humidity,
      rain_val = data$list$rain[[1]]
    ) %>%
      mutate(
        rain_val = ifelse(is.na(rain_val), 0, rain_val),
        et = mapply(estimate_et, temp_val, humidity_val),
        net_water = pmax(0, et - rain_val),
        water_liters = net_water * input$area
      )
    
    output$forecast_table <- renderTable({
      rain_data %>%
        select(
          Time = time_val,
          Temperature_C = temp_val,
          Humidity = humidity_val,
          Rain_mm = rain_val,
          ET_mm = et,
          NetWater_mm = net_water,
          Liters_Needed = water_liters
        ) %>%
        head(10)
    })
    
    output$water_plot <- renderPlot({
      ggplot(rain_data, aes(x = as.POSIXct(time_val), y = water_liters)) +
        geom_col(fill = "steelblue") +
        labs(
          title = "Predicted Lawn Watering (Liters)",
          x = "Date & Time",
          y = "Liters of Water Needed"
        ) +
        theme_minimal()
    })
  })
}

# Run the app
shinyApp(ui = ui, server = server)
