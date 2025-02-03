library(shiny)
library(dplyr)
library(leaflet)
library(ggplot2)
library(jsonlite)
library(shinythemes)
library(RColorBrewer)
library(shinyWidgets)
library(tidyjson)
library(tidyverse)
library(shinydashboard)

erthqs <- tryCatch({
  jsonlite::fromJSON("https://earthquake.usgs.gov/earthquakes/feed/v1.0/summary/4.5_month.geojson")
}, error = function(e) {
  print("Error in fetching earthquake data")
  return(NULL)
})

if (is.null(erthqs)) {
  stop("Failed to load earthquake data.")
}

# Extract data
erthqs_df <- erthqs$features %>%
  mutate(
    time = as.POSIXct(properties$time / 1000, origin = "1970-01-01", tz = "UTC"),
    long = sapply(geometry$coordinates, function(x) x[1]),  
    lat = sapply(geometry$coordinates, function(x) x[2]),   
    depth = sapply(geometry$coordinates, function(x) x[3]), 
    magnitude = properties$mag,  
    magType = factor(properties$magType)) %>%
  select(long, lat, depth, magnitude, magType, time)  # Select relevant columns

if (is.null(erthqs_df) || nrow(erthqs_df) == 0) {
  stop("Data is empty or failed to load.")
}

# UI for Dashboard
ui <- dashboardPage(
  skin = "blue",
  
  # Header
  dashboardHeader(title = "Magnitude 4.5+ Earthquakes Worldwide", 
                  titleWidth = 400),
  # Sidebar
  dashboardSidebar(
    sidebarMenu(
      menuItem("Earthquake Analysis", tabName = "analysis", icon = icon("dashboard")),
      menuItem("About", tabName = "info", icon = icon("info-circle"))
    ),
    selectInput(
      inputId = "analysis_var",
      label = "Analysis level",
      choices = c("Earthquake Size" = "magnitude", "Magnitude Type" = "magType")
    ),
    dateRangeInput(
      inputId = "date_range_input",
      label = "Select Date Range",
      start = min(erthqs_df$time),
      end = max(erthqs_df$time),
      min = min(erthqs_df$time),
      max = max(erthqs_df$time),
      format = "yyyy-mm-dd"
    ),
    selectInput(
      inputId = "continent_input",
      label = "Select Continent",
      choices = c("All", "Asia", "Africa", "North America", "South America", "Europe", "Oceania")
    ),
    sliderInput(
      inputId = "depth_filter",
      label = "Select Depth Range (km)",
      min = 0, max = 700, value = c(0, 700)
    )
  ),
  
  # Body
  dashboardBody(
    tabItems(
      tabItem(tabName = "analysis",
              fluidRow(
                column(9,
                       tabsetPanel(
                         tabPanel("Map", leafletOutput("ertqk_map", height = "400px")),
                         tabPanel("Table", tableOutput("ertqk_table")),
                         tabPanel("Plot", plotOutput("my_plot")),
                         tabPanel("Earthquake Frequency Over Time", plotOutput("time_series_plot"))
                       )
                )
              )
      ),
      tabItem(tabName = "info",
              h3("Understanding Earthquake Magnitude and Magnitude Type"),
              p("Magnitude is a measure of the size of an earthquake, indicating the amount of energy released during the event. For example, a magnitude of 7 signifies a very strong earthquake that can cause significant damage."),
              p("Magnitude Type refers to the different methods used to measure the size of an earthquake. There are several types of magnitudes, such as Local Magnitude (ML), Moment Magnitude (Mw), Surface Wave Magnitude (Ms), and Body Wave Magnitude (mb). Each of these types is calculated based on different seismic parameters and has specific applications.")
      )
    )
  )
)

# Server Logic
server <- function(input, output) {
  
  # Filter data based on date range, continent selection, and depth range
  filtered_data <- reactive({
    df <- erthqs_df
    
    # Filter by date range
    if (!is.null(input$date_range_input)) {
      df <- df %>%
        filter(time >= input$date_range_input[1] & time <= input$date_range_input[2])
    }
    
    # Filter by continent
    if (input$continent_input != "All") {
      continent_bounds <- list(
        "Asia" = list(lat = c(10, 80), lon = c(60, 150)),
        "Africa" = list(lat = c(-35, 37), lon = c(-20, 55)),
        "North America" = list(lat = c(24, 72), lon = c(-170, -60)),
        "South America" = list(lat = c(-55, 12), lon = c(-80, -35)),
        "Europe" = list(lat = c(36, 72), lon = c(-30, 45)),
        "Oceania" = list(lat = c(-45, -10), lon = c(110, 180))
      )
      
      bounds <- continent_bounds[[input$continent_input]]
      df <- df %>%
        filter(lat >= bounds$lat[1] & lat <= bounds$lat[2] & 
                 long >= bounds$lon[1] & long <= bounds$lon[2])
    }
    
    # Filter by depth range
    df <- df %>%
      filter(depth >= input$depth_filter[1] & depth <= input$depth_filter[2])
    
    return(df)
  })
  
  # Time Range Output
  output$date_range_text <- renderText({
    earliest_earthquake <- format(min(filtered_data()$time), "%Y-%m-%d %H:%M:%S")
    latest_earthquake <- format(max(filtered_data()$time), "%Y-%m-%d %H:%M:%S")
    
    paste("Data from: ", earliest_earthquake, " to: ", latest_earthquake)
  })
  

  # Map rendering
  output$ertqk_map <- renderLeaflet({
 
    legend_title <- ifelse(input$analysis_var == "magnitude", "Earthquake Size (Magnitude)", "Magnitude Type")
    

    if (input$analysis_var == "magnitude") {
      pal_ertqk <- colorNumeric(
        palette = "YlOrRd", 
        domain = filtered_data()$magnitude  
      )
    } else {
      pal_ertqk <- colorFactor(
        palette = "Set1",  
        domain = filtered_data()$magType  
      )
    }
    
    # map
    leaflet(data = filtered_data()) %>%
      addTiles() %>%
      addCircleMarkers(
        lat = ~lat,
        lng = ~long,
        label = ~paste(
          "Depth: ", depth, " km\n",
          "Magnitude: ", magnitude, "\n",
          "Date: ", format(time, "%Y-%m-%d %H:%M:%S")
        ),
        color = ~pal_ertqk(filtered_data()[[input$analysis_var]]),  
        fillOpacity = .7,
        radius = 4,
        stroke = FALSE) %>%
      addLegend(
        position = "bottomright",
        title = legend_title,  
        pal = pal_ertqk,
        values = ~filtered_data()[[input$analysis_var]],  
        opacity = .7
      )
  })
  
  
  # Table rendering
  output$ertqk_table <- renderTable({
    table <- filtered_data() %>%
      group_by(.data[[input$analysis_var]]) %>%
      count() %>%
      arrange(-n)
    colnames(table) <- c(input$analysis_var, "Number of Earthquakes")
    table
  })
  
  # Plot rendering
  output$my_plot <- renderPlot({
    ggplot(data = filtered_data(), aes(x = magnitude, y = magType, color = magType)) +
      geom_boxplot() +
      geom_jitter(width=0.15, alpha=0.3) +
      labs(
        title = "Earthquakes by Magnitude Type",
        caption = "(Source: USGS)",
        x = "Magnitude",
        y = "Magnitude Type") +
      theme(
        plot.title = element_text(color="royalblue4", size=14, face="bold", hjust = 0.5),
        axis.title.x = element_text(color="steelblue2", size=14, face="bold"),
        axis.title.y = element_text(color="steelblue2", size=14, face="bold"),
        plot.caption.position = "plot",
        legend.position = "none"
      )
  })
  
  # Time Series plot
  output$time_series_plot <- renderPlot({
    ggplot(data = filtered_data(), aes(x = time)) +
      geom_histogram(binwidth = 3600 * 24, fill = "blue", alpha = 0.7) +
      labs(
        title = "Earthquake Frequency Over Time",
        x = "Date",
        y = "Number of Earthquakes"
      ) +
      theme_minimal()
  })
  
  
  
}

shinyApp(ui = ui, server = server)
