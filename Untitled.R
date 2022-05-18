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


ertqk <- read.csv("", stringsAsFactors = F)
#data source: https://earthquake.usgs.gov/earthquakes/feed/v1.0/summary/4.5_month.csv