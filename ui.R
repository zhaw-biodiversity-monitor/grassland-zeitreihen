#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


source("libraries.R")
source("utils.R")

gpkg_path <- "appdata/vectors.gpkg"
layers <- read_sf(gpkg_path, "layers_overview")
aggregation1 <- unique(layers$aggregation1)
aggregation1 <- aggregation1[aggregation1 != "layers"]

col_y_options <- c(
  "artenzahl",
  "relative_artenzahl",
  "shannon_index",
  "shannon_evenness",
  "temperaturzahl",
  "kontinentalitatszahl",
  "lichtzahl",
  "feuchtezahl",
  "reaktionszahl",
  "nahrstoffzahl",
  "humuszahl",
  "konkurrenzzahl",
  "ruderalzahl",
  "stresszahl",
  "mahdvertraglichkeit"
)

names(col_y_options) <- clean_names(col_y_options)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  tags$script(src = "myjs.js"),
  # Application title
  titlePanel("Grassland Biodiverstiy in Switzerland"),
  
  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      # sliderInput("hoehenstufe", "Höhenstufe:", min = 0, max = 3000, value = c(0,3000)),
      # selectInput("datensatz", "Datensatz", unique(layers$dataset)),
      selectInput(
        "aggregation",
        "Aggregation",
        aggregation1
      ),
      selectInput(
        "column_y",
        "Unabhängige Variabel",
        col_y_options
      ),

      plotlyOutput("scatterplot"),
    ),
    
    # Show a plot of the generated distribution
    mainPanel(tabsetPanel(
      type = "tabs",
      tabPanel("Map", leaflet::leafletOutput("map", height = 600)),
      tabPanel("Legend", plotOutput("legend"))
      
    ))
  )
))
