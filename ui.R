#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(plotly)
library(sf)
library(tidyr)

gpkg_path <- "appdata/vectors.gpkg"
layers <- read_sf(gpkg_path, "layers_overview")



# aggregation_layers <- 
#   c(
#    "Hexagon 10km" = "grass_hex10km",
#    "Hexagon 5km" = "grass_hex5km",
#    "Hexagon 20km" = "grass_hex20km",
#    "Biogeografische Regionen" = "grass_biogreg",
#    "Kantone" = "grass_kantone",
#    "Kantone x Biogeografische Regionen" = "grass_kantone_biogreg",
#    "Biogeografische Regionen x Hexagon 10km" = "grass_hex10km_biogreg",
#    "Kantone x Hexagon 10km"= "grass_hex10km_kantone"
#     # "Biogeografische Regionen",
#    # "Kantone"
#   )

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  tags$script(src = "myjs.js"),
    # Application title
    titlePanel("Grassland Biodiverstiy in Switzerland"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
          # sliderInput("hoehenstufe", "Höhenstufe:", min = 0, max = 3000, value = c(0,3000)),
          selectInput("datensatz","Datensatz",unique(layers$dataset)),
          selectInput("aggregation","Aggregationsstufe",unique(layers$aggregation1)),
          selectInput("column_y","Unabhängige Variabel 1",c("artenreichtum_gefasspflanzen", "artenreichtum_neophyten", "artenanteil_neophyten", "deckungsanteil_neophyten", "temperaturzahl", "kontinentalitatszahl", "feuchtezahl", "reaktionszahl", "nahrstoffzahl", "strategie_c", "strategie_r", "strategie_s")),
          selectInput("colorize","Farbskala", c("Graduell" = "linear", "Bivariate" = c("Intensität" = "intensity","Durchsichtigkeit" = "alpha"))),
          plotlyOutput("scatterplot"),
        ),
        
        # Show a plot of the generated distribution
        mainPanel(
          tabsetPanel(
            type = "tabs",
            tabPanel("Map",leaflet::leafletOutput("map",height = 600)),
            tabPanel("Legend",plotOutput("legend"))
                      
          )
            
            
        )
    )
))
