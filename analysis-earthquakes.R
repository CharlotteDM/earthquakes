library("tidyverse")
library("dplyr")
library("shiny")
library("RColorBrewer")
library("rstudioapi")
library("leaflet")
library("htmlwidgets")

install.packages("rsconnect")
library("rsconnect")
install.packages("shinyWidgets")
library("shinyWidgets")

path <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(path)


ertqk <- read.csv("data/earthquakes.csv", stringsAsFactors = F)
#data source: https://earthquake.usgs.gov/earthquakes/feed/v1.0/summary/4.5_month.csv


#application interface
my_ui_earthquake <- fluidPage(
  titlePanel("M4.5+ Earthquakes in the world (18.04.2022 - 18.05.2022)"),
  sidebarLayout(
    sidebarPanel(
      selectInput(
        inputId = "analysis_var",
        label = "Analysis level",
        choices = c("mag", "magType")
        )
      ),
    mainPanel(
      leafletOutput("ertqk_map"),
      tableOutput("ertqk_table")
      )
    )
  )



#number of earhqk

#ertqk_table <- ertqk %>%
#  group_by(magType) %>%
#  arrange(-mag) %>%
#  select(time, depth, mag, magType, gap, dmin, rms)




#run app
#server <- function(input, output, sessions) {}
#shinyApp(ui = my_ui_earthquake, server = server)  



#refine code
my_server <- function(input, output) {
  
  output$ertqk_map <- renderLeaflet({
    pal_ertqk <- colorFactor(
      palette = "Dark2",
      domain = ertqk[[input$analysis_var]]
    )
    leaflet(data = ertqk) %>%
      addProviderTiles("Stamen.TonerLite") %>%
      addCircleMarkers(
        lat = ~latitude,
        lng = ~longitude,
        label = ~paste("Depth of the event in kilometers: ", depth,
                        "Magnitude: ", mag),
        color = ~pal_ertqk(ertqk[[input$analysis_var]]),
        fillOpacity = .7,
        radius = 4,
        stroke = F) %>%
      addLegend(
        position = "bottomright",
        title = "Magnitude Type",
        pal = pal_ertqk,
        values = ~ertqk[[input$analysis_var]],
        opacity = .5)
 })
  output$grouped_table <- renderTable({
    ertqk_table <- ertqk %>%
      group_by(ertqk[[input$analysis_var]]) %>%
      arrange(-input$analysis_var) 
      colnames(ertqk_table) <- c(input$analysis_var, "Magnitude")
      
  })
}


shinyApp(ui = my_ui_earthquake, server = my_server) 