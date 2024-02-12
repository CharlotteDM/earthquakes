library(tidyverse)
library(dplyr)
library(shiny)
library(RColorBrewer)
library(rstudioapi)
library(leaflet)
library(htmlwidgets)
library(ggplot2)
library(jsonlite)

#install.packages("rsconnect")
#install.packages("shinyWidgets")
#install.packages("shinythemes")

library(rsconnect)
library(shinyWidgets)
library(shinythemes)

#uncomment to set working directory of RStudio - only for local
path <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(path)

erthqs <- fromJSON("https://earthquake.usgs.gov/earthquakes/feed/v1.0/summary/4.5_month.geojson")

head(erthqs)
class(erthqs)

erthqs_df <- as.data.frame(erthqs)

head(erthqs_df)
class(erthqs_df)

colnames(erthqs_df)

#application interface
my_ui_earthquake <- fluidPage(
  theme = shinytheme("united"),
  titlePanel("Magn 4.5+ Earthquakes in the world"),
  sidebarLayout(
    sidebarPanel(
      selectInput(
        inputId = "analysis_var",
        label = "Analysis level",
        choices = c("magnitude", "type_of_magnitude")
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

#splitting up column: features.geometry$coordinates

# Split name column into 3
erthqs_df[c('long', 'lat', 'depth')] <- str_split_fixed(df$features.geometry$coordinates, ' ', 3)

# Rearrange columns and remove original name column
df <- df[c('First Name', 'Last Name', 'State')]



#table of earhqk

ertqk_table <- erthqs_df %>%
  group_by(features.properties$magType) %>%
  arrange(-features.properties$mag) %>%
  #select(features.properties$time, features.properties$mag, features.properties$magType, features.properties$gap, features.properties$dmin, features.properties$rms)
  select(features.properties)


#refine code
my_server <- function(input, output) {
  
  output$ertqk_map <- renderLeaflet({
    pal_ertqk <- colorFactor(
      palette = "Dark2",
      domain = ertqk[[input$analysis_var]]
    )
    leaflet(data = erthqs_df) %>%
      addTiles() %>%
      #addProviderTiles("Stamen.TonerLite") %>%
      addCircleMarkers(
        lat = ~latitude,
        lng = ~longitude,
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
  })
  output$ertqk_table <- renderTable({
    table <- ertqk_table %>%
      group_by(ertqk_table[[input$analysis_var]]) %>% 
      count %>%
      arrange(-n) 
    colnames(table) <- c(input$analysis_var, "Number of cases")
    table
  })
  output$my_plot <- renderPlot({
    ggplot (data = ertqk, (aes(magnitude,type_of_magnitude, color=type_of_magnitude))) +
      geom_boxplot() +
      geom_jitter(width=0.15, alpha=0.3) +
      labs(
        title = "Earthquakes 18.04.2022-18.05.2022",
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
    "data source: https://earthquake.usgs.gov/earthquakes/feed/v1.0/csv.php#data"
  })
}


shinyApp(ui = my_ui_earthquake, server = my_server, options = list(height = 800)) 

