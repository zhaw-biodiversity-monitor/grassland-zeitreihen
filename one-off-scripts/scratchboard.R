library(sf)
library(tidyverse)

read_all_layers <- function(file){
  sapply(st_layers(file)$name, \(x)st_read(file, x),simplify = FALSE) 
}

geodata <- read_all_layers("appdata//vectors.gpkg")

geodata_i <- geodata$hex20_tww


geodata_i$grp <- get_bivariate_group(vec1 = geodata_i$artenreichtum_gefasspflanzen, vec2 = geodata_i$n)

leaflet(geodata_i) |> 
  clearShapes() |> 
  clearControls()
  

levels(hoehe)
