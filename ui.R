library(shiny)
library(data.table)
library(magrittr)
library(utils)
library(shiny.semantic)
library(plotly)
library(geosphere)
library(bit64)
library(leaflet)
library(lubridate)

# Define UI for application that draws a histogram
shinyUI(semanticPage(
    uiOutput("mainPage")
))
