#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(readr)
library(sf)
library(leaflet)
library(ggplot2)
library(dplyr)
library(mapedit)

grassland <- read_csv(
  "grassland-data/Grassland_ALLEMA-BDM-WBS_v.5.csv",
  col_select = c("Artenreichtum", "Artenreichtum.Neophyten", "Länge", "Breite")) |>  
  st_as_sf(coords = c("Länge", "Breite"),crs = 4326, remove = FALSE)



# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
  output$map <- renderLeaflet({
    leaflet(grassland) |> 
      addTiles() |> 
      setView(lat = 46.89599, lng = 8.31787, zoom = 8) |> 
      addCircleMarkers(layerId = ~grassland,radius = 1) |> 
      # leaflet::addCircles(grassland$Länge, grassland$Breite) |> 
      addPmToolbar(
        toolbarOptions = pmToolbarOptions(drawMarker = FALSE,drawPolyline = FALSE, drawCircle = FALSE, drawPolygon = FALSE,cutPolygon = FALSE),
        editOptions = pmEditOptions(draggable = TRUE),
        cutOptions = pmCutOptions(cursorMarker = FALSE)
        )
  })
  
  grassland_inbounds <- reactive({
    if (is.null(input$map_bounds))
      return(grassland[FALSE,])
    bounds <- input$map_bounds
    latRng <- range(bounds$north, bounds$south)
    lngRng <- range(bounds$east, bounds$west)
    
    subset(grassland,
           Breite >= latRng[1] & Breite <= latRng[2] &
             Länge >= lngRng[1] & Länge <= lngRng[2])
  })
  
  
   

    output$distPlot <- renderPlot({

        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white',
             xlab = 'Waiting time to next eruption (in mins)',
             main = 'Histogram of waiting times')

    })
    
    

    
    output$boxplot2 <- renderPlotly({
      
      rbind(
        mutate(grassland_inbounds(), typ = "in bound"),
        mutate(grassland, typ = "all")
      ) |> 
        st_drop_geometry() |> 
        plot_ly(x = ~typ, y = ~Artenreichtum, type = "box", name = "All Points") |> 
        config(displayModeBar = FALSE) |> 
        plotly::layout(hovermode = FALSE) 
    })

})
