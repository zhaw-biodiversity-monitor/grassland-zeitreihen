
source("libraries.R")
source("utils.R")

grassland <- read_csv("appdata/normallandschaft.csv")

mycols <- list(
  drawing = list(
    rgba_string = "rgba(0, 51, 255, 1)", 
    hex = "#0033FF"
    ),
  selected_polygon = list(
    rgba_string = "rgba(255, 48, 0, 1)", 
    hex = "#ff3000"
    )
)

gpkg_path <- "appdata/vectors.gpkg"
geodata <- read_all_layers(gpkg_path, "layers_overview")

shinyServer(function(input, output) {
  output$map <- renderLeaflet({
    leaflet() |>
      addTiles(
        "https://wmts20.geo.admin.ch/1.0.0/ch.swisstopo.pixelkarte-grau/default/current/3857/{z}/{x}/{y}.jpeg",
        group = "Pixelkarte grau"
      ) |>
      addTiles(
        "https://wmts.geo.admin.ch/1.0.0/ch.swisstopo.swissimage/default/current/3857/{z}/{x}/{y}.jpeg",
        group = "Swissimage"
      ) |>
      addTiles(
        "https://wmts20.geo.admin.ch/1.0.0/ch.swisstopo.pixelkarte-farbe/default/current/3857/{z}/{x}/{y}.jpeg",
        group = "Pixelkarte farbig"
      ) |>
      addLayersControl(baseGroups = c("Pixelkarte grau", "Pixelkarte farbig", "Swissimage")) |>
      fitBounds(5.955902, 45.81796, 10.49206, 47.80845) |>
      # set singleFeature = TRUE to only disaable multi-feature drawing
      addDrawToolbar(
        polylineOptions = FALSE,
        polygonOptions = FALSE,
        circleOptions = FALSE,
        markerOptions = FALSE,
        circleMarkerOptions = FALSE,
        singleFeature = TRUE,
        rectangleOptions = drawRectangleOptions(
          shapeOptions = drawShapeOptions(
            color = as.character(mycols$drawing$hex),
            fill = FALSE,
            weight = 2
            )
          ),
        editOptions = editToolbarOptions()
      )

  })
  geodata_i <- reactive({geodata_i <- select_dataset(geodata, input$aggregation, input$datensatz)})
  
  observe({
    geodata_i <- geodata_i()
    ycol <- geodata_i[[input$column_y]]
    n <- geodata_i[["n"]]
    
    geodata_i$label <- paste(
      paste(input$column_y, round(ycol, 2), sep = ":"),
      paste("Anzahl Erhebungen", n, sep = ":"),
      sep = "<br>"
    )

    colpal <- "RdBu"
    geodata_i$grp <- ycol

    pal <- colorNumeric(colpal, geodata_i$grp, reverse = TRUE)

    fillOpacity <- 0.5
    prob2 <- seq(0, 1, .25)
    nbreaks2 <- length(prob2) - 1
    break1 <- c(0, 2, 5, Inf)
    nbreaks1 <- length(break1) - 1
    geodata_i$grp <-
      get_bivariate_group(n, ycol, prob2 = prob2, break1 = break1)
    mypal <- rev(RColorBrewer::brewer.pal(nbreaks2, "RdBu"))
    levs <- levels(geodata_i$grp)
    pal_col <- 
    pal <- colorFactor(pal_col, levels = levs, alpha = TRUE)
    fillOpacity <- 1


    leafletProxy("map", data = geodata_i) |>
      clearShapes() |>
      clearControls() |>
      addPolygons(
        fillColor = ~ pal(grp),
        color = ~ pal(grp),
        fillOpacity = fillOpacity,
        opacity = 0,
        label = ~ lapply(label, htmltools::HTML)
      )
    
  })
  
  observe({
      geodata_i <- geodata_i()
      
      selvec <- as.vector(geodata_i[,input$aggregation,drop = TRUE]) == selected_object()
      
      leafletProxy("map", data = geodata_i[selvec,]) |>
        clearGroup("polygonselection") |>
        addPolygons(fillOpacity = 0,group = "polygonselection",color = mycols$selected_polygon$hex,fill = FALSE)
    
    
  
  })
  
  
  
  
  
  ranges <- reactive({
    all_features <- input$map_draw_all_features
    features <- all_features$features
    coords <- map(features, \(x)x$geometry$coordinates[[1]])
    # print(coords)
    map(coords, \(x) {
      x |>
        map(\(y)c(y[[1]], y[[2]])) |>
        do.call(rbind, args = _) |>
        apply(2, range)
    })
    
  })
  
  grassland_inbounds <- reactive({

    if (length(ranges()) > 0) {
      
      ranges <- ranges()[[1]]
      lat <- ranges[, 2]
      lng <- ranges[, 1]
      grassland |>
        filter(lange > min(lng),
               lange < max(lng),
               breite > min(lat),
               breite < max(lat))
      
    } else{
      grassland[FALSE, ]
    }
  })
  
  
  
  
  # observeEvent(input$map_shape_click,{browser()})
  
  # Makes sure that this object exists even before the first clicking event
  selected_object <- reactiveVal("")
  observeEvent(input$map_shape_click, {
    loc_list <- input$map_shape_click
      loc_list <- input$map_shape_click
      geodata_i <-
        select_dataset(geodata, input$aggregation, input$datensatz)
      loc <- st_point(c(loc_list$lng, loc_list$lat)) |>
        st_sfc(crs = 4326)
      
      selected_object_str <-
        as.vector(geodata_i[loc, input$aggregation, drop = TRUE])
      selected_object(selected_object_str) # sets the value of this reactiveValue
   
    
  })
  
  
  grassland_renamed <- reactive({
    grassland <- grassland |>
      rename(column_y = input$column_y)|> 
      rename(agg = input$aggregation)

    return(grassland)
  })
  
  grassland_inbounds_renamed <- reactive({
    grassland_inbounds <- grassland_inbounds() |>
      rename(column_y = input$column_y)
      grassland_inbounds <-
        grassland_inbounds |> rename(agg = input$aggregation)
 
    return(grassland_inbounds)
  })
  
  
  output$scatterplot <- renderPlotly({
    
    fig <-
      plot_ly(
        grassland_renamed(),
        x = ~ meereshohe,
        y = ~ column_y,
        type = "scatter",
        mode = "markers",
        marker = list(color = 'rgba(255, 182, 193, 1)'),
        name = "all"
      ) |> 
      add_trace(
        data = grassland_inbounds_renamed(),
        color = "",
        marker = list(
          color = "rgba(255,255,255,0)",
          line = list(color = mycols$drawing$rgba_string, width = 2)
        ),
        name = "in bounds"
      )
    if (selected_object() != "") { 
      
      grassland_inpolygon <- grassland_renamed()[grassland_renamed()$agg == selected_object(),]
      
      fig <-
        fig |>
        add_trace(
          data = grassland_inpolygon,
          color = "",
          marker = list(
            color = "rgba(255,255,255,0)",
            line = list(color = mycols$selected_polygon$rgba_string, width = 2)
          ),
          name = "in polygon"
        )
    }
    
    fig |>
      layout(
        hovermode = FALSE,
        clickmode = "none",
        yaxis = list(title = clean_names(input$column_y)),
        modebar  = list(
          remove = c(
            "autoScale2d",
            "autoscale",
            "editInChartStudio",
            "editinchartstudio",
            "hoverCompareCartesian",
            "hovercompare",
            "lasso",
            "lasso2d",
            "orbitRotation",
            "orbitrotation",
            "pan",
            "pan2d",
            "pan3d",
            "reset",
            "resetCameraDefault3d",
            "resetCameraLastSave3d",
            "resetGeo",
            "resetSankeyGroup",
            "resetScale2d",
            "resetViewMapbox",
            "resetViews",
            "resetcameradefault",
            "resetcameralastsave",
            "resetsankeygroup",
            "resetscale",
            "resetview",
            "resetviews",
            "select",
            "select2d",
            "sendDataToCloud",
            "senddatatocloud",
            "tableRotation",
            "tablerotation",
            "toImage",
            "toggleHover",
            "toggleSpikelines",
            "togglehover",
            "togglespikelines",
            "toimage",
            "zoom",
            "zoom2d",
            "zoom3d",
            "zoomIn2d",
            "zoomInGeo",
            "zoomInMapbox",
            "zoomOut2d",
            "zoomOutGeo",
            "zoomOutMapbox",
            "zoomin",
            "zoomout",
            "displaylogo"
          )
        )
      )
    
    
    
  })
})
