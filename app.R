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
    
    # Extracting earthquake properties
    magnitude = properties$mag,  
    magType = factor(properties$magType)
  ) %>%
  select(long, lat, depth, magnitude, magType, time)  # Select relevant columns

if (is.null(erthqs_df) || nrow(erthqs_df) == 0) {
  stop("Data is empty or failed to load.")
}

# Application Interface
ui <- fluidPage(
  theme = shinytheme("cerulean"),
  
  # Custom CSS for title styling
  tags$style(HTML("
    h2 {
      text-align: center; 
      font-weight: bold; 
      font-size: 28px;
      color: steelblue2;
    }
    .date-range-text {
      text-align: center; 
      font-size: 16px;
      font-weight: bold;
      color: steelblue2;
    }
    /* Sidebar custom styling */
    .sidebar-panel {
      padding-top: 10px;  
      margin-top: 0px;
    }
      /* Map styling */
       .leaflet-container {
      width: 100% !important;   
      height: 100vh !important;
       }
       /* Reduce sidebar width */
    .well {
      width: 100%;  /* Make sidebar fill the available width */
      margin-top: 0px;
    }
    .container-fluid {
      padding-left: 0px;  /* Remove default padding for fluid layout */
      padding-right: 0px;
    }
  ")),
  
  titlePanel("Magnitude 4.5+ Earthquakes Worldwide"),
  
  # Use renderUI with HTML for dynamic styling
  uiOutput("date_range"), 
  
  # Sidebar layout with additional inputs for date and continent
  fluidRow(
    column(3,  # Left panel for filters and selections
           class = "sidebar-panel", 
           selectInput(
             inputId = "analysis_var",
             label = "Analysis level",
             choices = c("Earthquake Size" = "magnitude", "Magnitude Type" = "magType")
           ),
           # Date range input
           dateRangeInput(
             inputId = "date_range_input",
             label = "Select Date Range",
             start = min(erthqs_df$time),
             end = max(erthqs_df$time),
             min = min(erthqs_df$time),
             max = max(erthqs_df$time),
             format = "yyyy-mm-dd"
           ),
           # Dropdown for selecting continent
           selectInput(
             inputId = "continent_input",
             label = "Select Continent",
             choices = c("All", "Asia", "Africa", "North America", "South America", "Europe", "Oceania")
           )
    ),
    
    column(9,  # Main content area for map, table, etc.
           mainPanel(
             textOutput("tabs_title"),
             strong("For more information go to the section:"),
             tabsetPanel(
               tabPanel("Map", leafletOutput("ertqk_map", height = "100%")),  # Specify map height
               tabPanel("Table", tableOutput("ertqk_table")),
               tabPanel("Plot", plotOutput("my_plot")),
               tabPanel("Information", 
                        h3("Understanding Earthquake Magnitude and Magnitude Type"),
                        p("Magnitude is a measure of the size of an earthquake, indicating the amount of energy released during the event. For example, a magnitude of 7 signifies a very strong earthquake that can cause significant damage."),
                        p("Magnitude Type refers to the different methods used to measure the size of an earthquake. There are several types of magnitudes, such as Local Magnitude (ML), Moment Magnitude (Mw), Surface Wave Magnitude (Ms), and Body Wave Magnitude (mb). Each of these types is calculated based on different seismic parameters and has specific applications.")
               )
             )
           )
    )
  )
)
# Server logic
server <- function(input, output) {
  
  # Filter data based on date range and continent selection
  filtered_data <- reactive({
    df <- erthqs_df
    
    # Filter by date range
    if (!is.null(input$date_range_input)) {
      df <- df %>%
        filter(time >= input$date_range_input[1] & time <= input$date_range_input[2])
    }
    
    # Filter by continent (This is an example based on the available lat/lon data, 
    # you would need to define the continents properly using bounding boxes or similar logic)
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
    
    return(df)
  })
  
  # Time Range Output
  output$date_range <- renderUI({
    earliest_earthquake <- format(min(filtered_data()$time), "%Y-%m-%d %H:%M:%S")
    latest_earthquake <- format(max(filtered_data()$time), "%Y-%m-%d %H:%M:%S")
    
    # Return HTML for centered text
    HTML(paste("<div style='text-align:center; font-size: 16px; font-weight: bold; color: SkyBlue;'>",
               "Data from: ", earliest_earthquake, " to: ", latest_earthquake,
               "</div>"))
  })
  
  # Map rendering
  output$ertqk_map <- renderLeaflet({
    tryCatch({
      legend_title <- ifelse(input$analysis_var == "magnitude", "Earthquake Size (Magnitude)", "Magnitude Type")
      pal_ertqk <- colorFactor(
        palette = "Dark2",
        domain = filtered_data()[[input$analysis_var]]
      )
      leaflet(data = filtered_data()) %>%
        addTiles() %>%
        addCircleMarkers(
          lat = ~lat,
          lng = ~long,
          label = ~paste("Depth of the event in kilometers: ", depth,
                         "Magnitude: ", magnitude,
                         "Date: ", time),
          color = ~pal_ertqk(filtered_data()[[input$analysis_var]]),
          fillOpacity = .7,
          radius = 4,
          stroke = F) %>%
        addLegend(
          position = "bottomright",
          title = legend_title,
          pal = pal_ertqk,
          values = ~filtered_data()[[input$analysis_var]],
          opacity = .7)
    }, error = function(e) {
      print(paste("Error in map rendering:", e))
      return(NULL)
    })
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
        title = "Earthquakes in The World by Magnitude Type",
        caption = "(based on data from: https://earthquake.usgs.gov/earthquakes/feed/v1.0/summary/4.5_month.csv)",
        x = "Magnitude",
        y = "Magnitude type") +
      theme(
        plot.title = element_text(color="royalblue4", size=14, face="bold", hjust = 0.5),
        axis.title.x = element_text(color="steelblue2", size=14, face="bold"),
        axis.title.y = element_text(color="steelblue2", size=14, face="bold"),
        plot.caption.position = "plot",
        legend.position = "none"
      )
  })
  
  output$tabs_title <- renderText({ 
    "Data source: https://earthquake.usgs.gov/earthquakes/feed/v1.0/csv.php#data"
  })
}

shinyApp(ui = ui, server = server)
