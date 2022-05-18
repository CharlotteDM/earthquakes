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


#sets color
pal_ertqk <- colorFactor(palette = "Set3", domain = ertqk[["mag"]])

#creates map
ertqk_map <- leaflet(data = ertqk) %>%
  addProviderTiles("CartoDB.Positron") %>%
  addCircleMarkers(
    lat = ~latitude,
    lng = ~longitude,
    label = ~paste0("</br> Depth of the event in kilometers: ", depth,
                    "</br> MAgnitude: ", mag),
    color = ~pal_ertqk(ertqk[["mag"]]),
    fillOpacity = .7,
    radius = 4,
    stroke = F) %>%
  addLegend(
    position = "bottomright",
    title = "Eartquakes",
    pal = pal_ertqk,
    values = ~mag,
    opacity = .5)


#number of physicians in a given specialization

ertqk_table <- ertqk %>%
  group_by(magType) %>%
  arrange(-mag) %>%
  select(time, depth, mag, magType, gap, dmin, rms)

#application interface
my_ui_earthquake <- fluidPage(
  titlePanel("M4.5+ Earthquakes in the world (18.04.2022 - 18.05.2022)"),
  sidebarLayout(
    sidebarPanel(
      selectInput(
        inputId = "analysis_var",
        label = "Magnitude:",
        choices = unique(ertqk$mag))),
        mainPanel(
          leafletOutput(outputId = "ertqk_map"),
          leafletOutput(outputId = "ertqk_table"))))




#run app
server <- function(input, output, sessions) {}
shinyApp(ui = my_ui_earthquake, server = server)  



#refine code
my_server <- function(input, output, session) {
  output$ertqk_map <- renderLeaflet({
    pal_ertqk <- colorFactor(
      palette = "Set3",
      domain = ertqk[[input$analysis_var]]
    )
    leaflet(data = ertqk) %>%
      addProviderTiles("Stamen.TonerLite") %>%
      addCircleMarkers(
        lat = ~latitude,
        lng = ~longitude,
        label = ~paste0("</br> Depth of the event in kilometers: ", depth,
                        "</br> MAgnitude: ", mag),
        color = ~pal_ertqk(ertqk[["mag"]]),
        fillOpacity = .7,
        radius = 4,
        stroke = F) %>%
      addLegend(
        position = "bottomright",
        title = "Eartquakes",
        pal = pal_ertqk,
        values = ~mag,
        opacity = .5)
 })
}


shinyApp(ui = my_ui_earthquake, server = my_server) 