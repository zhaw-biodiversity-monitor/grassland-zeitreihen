library(shiny)
library(readr)
library(sf)
library(leaflet)
library(ggplot2)
library(dplyr)
library(mapedit)
# library(leafpm)
library(leaflet.extras)
library(plotly)
library(htmltools) # for htmlEscape
library(tidyr)
library(purrr)

grassland <- read_csv("appdata/normallandschaft.csv")

read_all_layers <- function(file, exception = NA){
  layer_names <- st_layers(file)$name
  layer_names <- layer_names[!(layer_names %in% exception)]
  sapply(layer_names, \(x)st_read(file, x),simplify = FALSE) 
}

# takes two vectors (e.g. Artenreichtum and n), groups each into discrete groups 
# by quantile (probs), and returns a group index which can be used to colorize 
# using a bivarate scale
get_bivariate_group <- function(vec1, vec2, prob1 = seq(0,1,0.25), prob2 = prob1, break1 = NA, break2 = NA){

  stopifnot(length(vec1) == length(vec2))
  vecs <- list(vec1, vec2)       #
  probs <- list(prob1,prob2)     # -> create lists to use map2
  breaks <- list(break1, break2) #
  
  mybreaks <- map2(vecs,probs, \(x,y) quantile(x,y,na.rm = TRUE))
  mybreaks[!is.na(breaks)] <- breaks[!is.na(breaks)]
  
  cuts <- map2(vecs, mybreaks, \(x,y)cut(x, y, include.lowest = TRUE, dig.lab = 10, labels = FALSE))
  
  # Get all *possible* factor levels (even those not in the data)
  
  fac_levels <- map(lengths(mybreaks)-1, seq_len) |> 
    expand.grid() |> 
    apply(1, paste,collapse = "-") |> 
    sort()
  do.call(cbind, cuts) |> 
    apply(1, \(x){paste(x,collapse = "-")}) |> 
    factor(levels =  fac_levels)
}


# from here:
# https://github.com/rstudio/gt/blob/ff878e10d21a3ba897c5f99801b796da8fb637fa/R/helpers.R#L2496-L2536
adjust_luminance <- function(colors,steps) {
  stopifnot(steps < 2, steps > -2)
  rgb_matrix <- t(grDevices::col2rgb(colors, alpha = TRUE)) / 255
  alpha <- rgb_matrix[, "alpha"]
  luv_matrix <- grDevices::convertColor(rgb_matrix[, 1:3], "sRGB", "Luv")
  h <- atan2(luv_matrix[, "v"], luv_matrix[, "u"]) * 180 / pi
  c <- sqrt(luv_matrix[, "u"]^2 + luv_matrix[, "v"]^2)
  l <- luv_matrix[, "L"]
  y <- l / 100.
  x <- log(-(y / (y - 1)))
  y_2 <- 1 / (1 + exp(-(x + steps)))
  l <- y_2 * 100.
  grDevices::hcl(h, c, l, alpha = alpha)
}

# create a matrix with color palette
bivariate_matrix_luminocity <- function(mypal, n = length(mypal), combine_with = "cbind"){
    accumulate(seq_len(n-1),\(x,y) adjust_luminance(x,1),.init = mypal) |> 
      rev() |> 
      (\(x) do.call(combine_with, x))()
}

bivariate_matrix_alpha <- function(mypal, n = length(mypal), alpha_range = c(0,1)){
  rgb_mat <- col2rgb(mypal)/255  
  a_from <- alpha_range[1]
  a_to <- alpha_range[2]
  alpha_seq <- seq(a_from, a_to,(a_to-a_from)/(n-1))
  
  sapply(alpha_seq, function(alpha){
    apply(rgb_mat, 2,\(x) rgb(x[1],x[2],x[3],alpha))
  }) 
}

gpkg_path <- "appdata/vectors.gpkg"
geodata <- read_all_layers(gpkg_path,"layers_overview")

shinyServer(function(input, output) {
  
  output$map <- renderLeaflet({
    leaflet() |> 
      addTiles("https://wmts20.geo.admin.ch/1.0.0/ch.swisstopo.pixelkarte-grau/default/current/3857/{z}/{x}/{y}.jpeg", group = "Pixelkarte grau") |>
      addTiles("https://wmts.geo.admin.ch/1.0.0/ch.swisstopo.swissimage/default/current/3857/{z}/{x}/{y}.jpeg",group = "Swissimage") |>
      addTiles("https://wmts20.geo.admin.ch/1.0.0/ch.swisstopo.pixelkarte-farbe/default/current/3857/{z}/{x}/{y}.jpeg", group = "Pixelkarte farbig") |>
      addLayersControl(baseGroups = c("Pixelkarte grau", "Pixelkarte farbig", "Swissimage")) |> 
      fitBounds(5.955902,45.81796,10.49206,47.80845 ) |> 
      # set singleFeature = TRUE to only disaable multi-feature drawing
      addDrawToolbar(
        polylineOptions = FALSE, 
        polygonOptions = FALSE,
        circleOptions = FALSE,
        markerOptions = FALSE, 
        circleMarkerOptions= FALSE, 
        singleFeature = TRUE,
        editOptions = editToolbarOptions()
        )
    
  })
  
  observe({
    layer_name <- paste(input$aggregation, input$datensatz, sep = "_")
    geodata_i <- na.omit(geodata[[layer_name]])
    ycol <- geodata_i[[input$column_y]]
    n <- geodata_i[["n"]]

    geodata_i$label <- paste(
      paste(input$column_y,round(ycol,2), sep = ":"),
      paste("Anzahl Erhebungen", n, sep = ":"),
      sep = "<br>"
    )

    
    
    if(input$colorize == "linear"){
      
      colpal <- "RdBu"
      geodata_i$grp <- ycol
      
      pal <- colorNumeric(colpal, geodata_i$grp, reverse = TRUE)
      
      fillOpacity <- 0.5
    } else {
      
      prob2 <- seq(0,1,.25)
      nbreaks2 <- length(prob2)-1
      break1 <- c(0,2,5,Inf)
      nbreaks1 <- length(break1)-1
      geodata_i$grp <- get_bivariate_group(n,ycol,prob2 = prob2, break1 = break1)
      mypal <- rev(RColorBrewer::brewer.pal(nbreaks2,"RdBu"))
      levs <- levels(geodata_i$grp)
      
      if(input$colorize == "intensity"){
        
        pal_col <- bivariate_matrix_luminocity(mypal,nbreaks1) |> as.vector()  
        pal <- colorFactor(pal_col, levels = levs, alpha = FALSE)
        fillOpacity <- 0.5
      } else{
        pal_col <- bivariate_matrix_alpha(mypal, nbreaks1, alpha_range = c(0.1,0.8)) |> as.vector()
        pal <- colorFactor(pal_col, levels = levs, alpha = TRUE)
        fillOpacity <- 1
      }
    }
    leafletProxy("map", data = geodata_i) |> 
      clearShapes() |> 
      clearControls() |> 
      addPolygons(fillColor = ~pal(grp), color = ~pal(grp), fillOpacity = fillOpacity, opacity = 0, label = ~lapply(label, htmltools::HTML))
    
  })
  

  
  
  
  ranges <- reactive({
    all_features <- input$map_draw_all_features
    features <- all_features$features
    coords <- map(features, \(x)x$geometry$coordinates[[1]])
    map(coords, \(x){
      x |> 
        map(\(y)c(y[[1]],y[[2]])) |> 
        do.call(rbind, args = _) |> 
        apply(2,range)
      })
    
  })
  
  grassland_inbounds <- reactive({
    # ranges()
    # ranges <- ranges()[[1]]
    
    
    if(length(ranges())>0){
      # browser()
      ranges <- ranges()[[1]]
      lat <- ranges[,1]
      lat <- ranges[,2]
      grassland |> 
        filter(lange > min(lng), lange < max(lng),breite > min(lat), breite < max(lat))
    
    } else{
      grassland[FALSE, ]
    }
  })
   
  
  
  
  # observeEvent(input$map_draw_all_features,{browser()})
  
  
  
  # grassland_inbounds_drawing <- reactive({
  #   all_features <- input$map_draw_all_features
  #   coords <- all_features[[2]][[1]]$geometry$coordinates
  #   latRng <- sapply(coords, \(x)x[[2]]) |> range()
  #   lngRng <- sapply(coords, \(x)x[[1]]) |> range()
  #   filter(grassland,
  #          breite >= latRng[1] & breite <= latRng[2] &
  #            lange >= lngRng[1] & lange <= lngRng[2])
  # })

    output$scatterplot <- renderPlotly({
      
      grassland <- grassland |> rename(column_y = input$column_y) 
      grassland_inbounds <- grassland_inbounds() |> rename(column_y = input$column_y) 
      
      if(input$aggregation %in% c("BGR","kantone")){
        grassland <- grassland |> rename(agg = input$aggregation) 
        grassland_inbounds <- grassland_inbounds |> rename(agg = input$aggregation) 
        
        fig <- plot_ly(grassland, x = ~meereshohe, y = ~column_y,type = "scatter",color = ~agg, mode = "markers") |> 
          add_trace(data = grassland_inbounds, color = "", marker = list(color = "rgba(255,255,255,0)",line = list(color = "rgba(0, 0, 0, .8)", width = 2)),name = "in bounds")
      } else{
        fig <- plot_ly(grassland, x = ~meereshohe, y = ~column_y,type = "scatter", mode = "markers", marker = list(color = 'rgba(255, 182, 193, 1)'),name = "all") |> 
          add_trace(data = grassland_inbounds, marker = list(color = "rgba(255,255,255,0)",line = list(color = "rgba(152, 0, 0, .8)", width = 2)),name = "in bounds") 
      }
      
      fig |> 
        layout(
          hovermode = FALSE,
          clickmode = "none", 
          modebar  = list(
            remove = c("autoScale2d", "autoscale", "editInChartStudio", "editinchartstudio", "hoverCompareCartesian", "hovercompare", "lasso", "lasso2d", "orbitRotation", "orbitrotation", "pan", "pan2d", "pan3d", "reset", "resetCameraDefault3d", "resetCameraLastSave3d", "resetGeo", "resetSankeyGroup", "resetScale2d", "resetViewMapbox", "resetViews", "resetcameradefault", "resetcameralastsave", "resetsankeygroup", "resetscale", "resetview", "resetviews", "select", "select2d", "sendDataToCloud", "senddatatocloud", "tableRotation", "tablerotation", "toImage", "toggleHover", "toggleSpikelines", "togglehover", "togglespikelines", "toimage", "zoom", "zoom2d", "zoom3d", "zoomIn2d", "zoomInGeo", "zoomInMapbox", "zoomOut2d", "zoomOutGeo", "zoomOutMapbox", "zoomin", "zoomout", "displaylogo")
          )
        )

        
        
    })
})
