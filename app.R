library(shiny)
library(dplyr)
library(leaflet)
library(ggplot2)
library(jsonlite)
library(shinythemes)
library(plotly)
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

#head(erthqs)
#class(erthqs)
#str(erthqs)

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

#head(erthqs_df)
#class(erthqs_df)
#str(erthqs_df)
#colnames(erthqs_df)

#application interface
ui <- fluidPage(
  theme = shinytheme("united"),
  titlePanel("Magnitude 4.5+ Earthquakes Worldwide"),
  textOutput("date_range"),
  sidebarLayout(
    sidebarPanel(
      selectInput(
        inputId = "analysis_var",
        label = "Analysis level",
        choices = c("Earthquake Size" = "magnitude", "Magnitude Type" = "magType")
      )),
    
    mainPanel(
      textOutput("tabs_title"),
      strong("For more information go to the section:"),
      tabsetPanel(
        tabPanel("Map", leafletOutput("ertqk_map")), 
        tabPanel("Table", tableOutput("ertqk_table")),
        tabPanel("Plot", plotOutput("my_plot"))
      )
    )
  )
)


#table of earhqk

ertqk_table <- erthqs_df %>%
  group_by(magType) %>%   
  arrange(-magnitude) %>%  
  select(magType, magnitude, long, lat, depth) 


#server
server <- function(input, output) {
  
  # time
  output$date_range <- renderText({
    earliest_earthquake <- format(min(erthqs_df$time), "%Y-%m-%d %H:%M:%S")
    latest_earthquake <- format(max(erthqs_df$time), "%Y-%m-%d %H:%M:%S")
    paste("Data from: ", earliest_earthquake, " to: ", latest_earthquake)
  })
  
  # map
  output$ertqk_map <- renderLeaflet({
    tryCatch({
      pal_ertqk <- colorFactor(
        palette = "Dark2",
        domain = erthqs_df[[input$analysis_var]]
      )
      leaflet(data = erthqs_df) %>%
        addTiles() %>%
        addCircleMarkers(
          lat = ~lat,
          lng = ~long,
          label = ~paste("Depth of the event in kilometers: ", depth,
                         "Magnitude: ", magnitude,
                         "Date: ", time),
          color = ~pal_ertqk(erthqs_df[[input$analysis_var]]),
          fillOpacity = .7,
          radius = 4,
          stroke = F) %>%
        addLegend(
          position = "bottomright",
          title = input$analysis_var,
          pal = pal_ertqk,
          values = ~erthqs_df[[input$analysis_var]],
          opacity = .5)
    }, error = function(e) {
      print(paste("Error in map rendering:", e))
      return(NULL)
    })
  })
  
  # table
  output$ertqk_table <- renderTable({
    table <- erthqs_df %>%
      group_by(.data[[input$analysis_var]]) %>%
      count() %>%
      arrange(-n)
    colnames(table) <- c(input$analysis_var, "Number of Earthquakes")
    table
  })
  
  # plot
  output$my_plot <- renderPlot({
    ggplot(data = erthqs_df, aes(x = magnitude, y = magType, color = magType)) +
      geom_boxplot() +
      geom_jitter(width=0.15, alpha=0.3) +
      labs(
        title = "Earthquakes in The World by Magnitude Type",
        caption = "(based on data from: https://earthquake.usgs.gov/earthquakes/feed/v1.0/summary/4.5_month.csv",
        x = "Magnitude",
        y = "Magnitude type") +
      theme(
        plot.title = element_text(color="royalblue4", size=14, face="bold", hjust = 0.5),
        axis.title.x = element_text(color="steelblue2", size=14, face="bold"),
        axis.title.y = element_text(color="steelblue2", size=14, face="bold"),
        plot.caption.position = "plot",
        legend.position = "none")
    
  })
 
  output$tabs_title <- renderText({ 
    "Data source: https://earthquake.usgs.gov/earthquakes/feed/v1.0/csv.php#data"
  })
}

shinyApp(ui = ui, server = server)

