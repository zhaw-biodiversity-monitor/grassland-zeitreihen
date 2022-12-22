library(shiny)
library(readr)
library(sf)
library(leaflet)
library(ggplot2)
library(dplyr)
# library(mapedit)
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


# get_bivariate_plot <- function(prob1, prob2){
#   df0 <- tibble(
#     xstart = head(prob2,-1),
#     xend = tail(prob2, -1),
#     x_i = seq_len(length(prob2)-1),
#     ystart = head(prob1,-1),
#     yend = tail(prob1, -1),
#     y_i = seq_len(length(prob1)-1),
#   )
#   
#   df <- complete(df0, nesting(xstart, xend, x_i),nesting(ystart, yend, y_i))
#   
#   tibble(
#     x_i = seq_len(length(prob2)-1),
#     xstart_name = head(names(prob2),-1),
#     xend_name = tail(names(prob2), -1),
#   )
#   
#   tibble(
#     y_i = seq_len(length(prob1)-1),
#     ystart_name = head(names(prob1),-1),
#     yend_name = tail(names(prob1), -1),
#   )
#   
#   # vec_names <- names(vecs)
#   df$grp <- apply(cbind(df$y_i, df$x_i), 1, \(x) mat[x[1],x[2]])
#   
#   names(bivpal) <- sort(df$grp)
#   
#   xbreaks <- rowMeans(cbind(df0$x_i,lead(df0$x_i)))
#   xlabs <- as.integer(lead(df0$xend))
#   
#   ybreaks <- rowMeans(cbind(df0$y_i,lead(df0$y_i)))
#   ylabs <- as.integer(lead(df0$yend))    
#   ggplot(df, aes(x = x_i, y = y_i, fill = as.character(grp))) +
#     geom_raster() +
#     scale_fill_manual(values = bivpal) +
#     scale_x_continuous(breaks = xbreaks, labels = xlabs) +
#     scale_y_continuous(breaks = ybreaks, labels = ylabs) +
#     coord_equal() +
#     # labs(x = vec_names[2], y = vec_names[1]) +
#     theme(legend.position = "none", 
#           panel.grid = element_blank(),
#           panel.background = element_blank(),
#           # axis.text = element_blank(),
#           axis.ticks = element_blank())
# }
  

gpkg_path <- "appdata/vectors.gpkg"
geodata <- read_all_layers(gpkg_path,"layers_overview")

shinyServer(function(input, output) {
  
  output$map <- renderLeaflet({
    leaflet() |> 
      addTiles("https://wmts20.geo.admin.ch/1.0.0/ch.swisstopo.pixelkarte-grau/default/current/3857/{z}/{x}/{y}.jpeg", group = "Pixelkarte grau") |>
      addTiles("https://wmts.geo.admin.ch/1.0.0/ch.swisstopo.swissimage/default/current/3857/{z}/{x}/{y}.jpeg",group = "Swissimage") |>
      addTiles("https://wmts20.geo.admin.ch/1.0.0/ch.swisstopo.pixelkarte-farbe/default/current/3857/{z}/{x}/{y}.jpeg", group = "Pixelkarte farbig") |>
      addLayersControl(baseGroups = c("Pixelkarte grau", "Pixelkarte farbig", "Swissimage")) |> 
      fitBounds(5.955902,45.81796,10.49206,47.80845 )
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
  
  clicked_loc <- reactive({
    event <- input$map_shape_click
    if (is.null(event)){
      return()
    }
    input$map_shape_click
    
    })
 
  grassland_inbounds <- reactive({
    if (is.null(input$map_bounds))
      return(grassland[FALSE,])
    bounds <- input$map_bounds
    latRng <- range(bounds$north, bounds$south)
    lngRng <- range(bounds$east, bounds$west)
    
    hoehenstufe <- input$hoehenstufe
    filter(
      grassland,
      breite >= latRng[1],
      breite <= latRng[2],
      lange >= lngRng[1],
      lange <= lngRng[2],
      meereshohe >= hoehenstufe[1],
      meereshohe <= hoehenstufe[2]
           )
  })
  
    
   grassland_inbounds_drawing <- reactive({

    all_features <- input$map_draw_all_features

    coords <- all_features[[2]][[1]]$geometry$coordinates

    latRng <- sapply(coords, \(x)x[[2]]) |> range()
    lngRng <- sapply(coords, \(x)x[[1]]) |> range()

    filter(grassland,
           breite >= latRng[1] & breite <= latRng[2] &
             lange >= lngRng[1] & lange <= lngRng[2])
    })

    output$boxplot2 <- renderPlotly({
      rbind(
        mutate(grassland_inbounds(), typ = "in bound"),
        mutate(grassland, typ = "all")

      ) |>
        plot_ly(x = ~typ, y = ~artenreichtum_gefasspflanzen, type = "box") |>
        config(displayModeBar = FALSE) |>
        plotly::layout(hovermode = FALSE)
    })
    


})
