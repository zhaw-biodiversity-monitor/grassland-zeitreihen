

library(tidyverse)
library(sf)
library(readxl)
library(glue)

read_all_layers <- function(file){
  sapply(st_layers(file)$name, \(x)st_read(file, x),simplify = FALSE) 
}

delete_all_layers <- function(file){
  sapply(st_layers(file)$name, \(x)st_delete(file, x),simplify = FALSE) 
}

hexagonize <- function(hex, to_be_hexagonized, ..., .na_omit = TRUE, .do_union = TRUE){
  joined <- st_join(hex, select(to_be_hexagonized, ...), largest = TRUE)
  if(.na_omit) joined <- na.omit(joined)
  if(.do_union){
    joined <- joined %>%
      group_by(...) %>%
      summarise()
  }
  return(joined)
}

aggregate_grass <- function(
    x, 
    by, 
    weight_col = "design_weight", 
    columns_to_weight = c("artenreichtum_gefasspflanzen", "artenreichtum_neophyten", "artenanteil_neophyten", "deckungsanteil_neophyten", "temperaturzahl", "kontinentalitatszahl", "feuchtezahl", "reaktionszahl", "nahrstoffzahl", "strategie_c", "strategie_r", "strategie_s"), 
    columns_to_count = "_p_a$", # columns matching this ("matches()") will just be counted
    filter_zero_count = TRUE){
  
  # browser()
  stopifnot(all(columns_to_weight %in%names(x)))
  stopifnot(weight_col %in% names(x))
  
  # create a tidy select that I can use with matches()
  columns_to_weight_tidy_sel <- paste(columns_to_weight, collapse = "|")
  
  # multiply all columns that should be weighed with the weight_col
  weighted <- x[c(weight_col, columns_to_weight)] |>  # select only the weight col and the columns to weigh
    mutate(
      across(
        matches(columns_to_weight_tidy_sel),   # for all columns that should be weighed...
        function(z){z*x[[weight_col]]})        # ... multiply by the weigh col
    ) 
  
  weighted_sum <- cbind(
    weighted,                                   
    st_drop_geometry(select(x, matches("_p_a$")))
  ) |> 
    aggregate(by, sum, na.rm = TRUE)
  
  weighted_mean <- weighted_sum |> 
    mutate(
      across(
        matches(columns_to_weight_tidy_sel), 
        function(z){z/weighted_sum[[weight_col]]})
    ) 
  
  # agg_fun <- aggregate(x_weighted, by, FUN = FUN)
  # agg_fun$n <- 
  weighted_mean$n <- aggregate(transmute(x, n = 1), by, FUN = length) |> st_drop_geometry() |> (\(x) x[,1])()
  
  weighted_mean <- weighted_mean|> 
    mutate(
      across(
        ends_with("_p_a"),
        function(z){z/n}
      )
    )
  
  weighted_mean <- cbind(
    weighted_mean,
    st_drop_geometry(by)
  )
  
  
  if(filter_zero_count){
    weighted_mean <- weighted_mean[weighted_mean$n>0 & !is.na(weighted_mean$n),]
  }
  weighted_mean
}

import_sheet <- function(xlsx, sheet){
  library(readxl)
  library(janitor)
  library(dplyr)
  read_excel(xlsx, sheet) |> 
    janitor::clean_names() |> 
    transmute(
      plot_id,
      design_weight, 
      x_lv95, 
      y_lv95, 
      lange, 
      breite, 
      meereshohe, 
      artenreichtum_gefasspflanzen, 
      artenreichtum_neophyten, 
      artenanteil_neophyten, 
      deckungsanteil_neophyten,
      temperaturzahl,
      kontinentalitatszahl,
      feuchtezahl,
      reaktionszahl,
      nahrstoffzahl,
      strategie_c,
      strategie_r,
      strategie_s,
      lolium_multiflorum_p_a,
      veronica_filiformis_p_a,
      veronica_persica_p_a,
      medicago_sativa_p_a,
      erigeron_annuus_p_a,
      matricaria_discoidea_p_a,
      bromus_inermis_p_a,
      conyza_canadensis_aggr_p_a,
      impatiens_parviflora_p_a,
      juncus_tenuis_p_a,
      solidago_gigantea_p_a,
      vicia_villosa_p_a
    ) |> 
    mutate(
      across(!ends_with("_p_a"),as.numeric),
      across(ends_with("_p_a"),as.logical),
    )
}



BGR <- read_sf("grassland-data-raw/biogreg/BiogeographischeRegionen/N2020_Revision_BiogeoRegion.shp") |>
    st_zm() 


kantone <- read_sf("grassland-data-raw/swissboundaries3d_2022-05_2056_5728.shp/SHAPEFILE_LV95_LN02/swissBOUNDARIES3D_1_3_TLM_KANTONSGEBIET.shp") |> 
  st_zm() |> 
  select(NAME, KANTONSNUM)

schweiz <- read_sf("grassland-data-raw/swissboundaries3d_2022-05_2056_5728.shp/SHAPEFILE_LV95_LN02/swissBOUNDARIES3D_1_3_TLM_LANDESGEBIET.shp") |> 
  st_zm() |> 
  filter(NAME != "Liechtenstein") |> 
  st_union()

sheets <- c(normallandschaft = "Normallandschaft (BDM)", tww = "TWW (WBS)", moore = "Moore (WBS)")

xlsx_path <- "grassland-data-raw/Grassland_ALLEMA-BDM-WBS_v.6_Biodiversitätsmonitor.xlsx"
gpkg_path <- "appdata/vectors.gpkg"

# delete_all_layers(gpkg_path)
# lays <- read_all_layers(gpkg_path)

grass_df <- imap(sheets, \(x,y){import_sheet(xlsx_path, x)})



grass_sf <- imap(grass_df, ~st_as_sf(.x, coords = c("x_lv95","y_lv95"), crs = 2056, remove = FALSE) )


hex10 <- st_make_grid(schweiz, 10000,square = FALSE) |> st_as_sf() |> mutate(hex10 = row_number())
hex20 <- st_make_grid(schweiz, 20000,square = FALSE) |> st_as_sf() |> mutate(hex20 = row_number())

hex10_BGR <- hexagonize(hex10, BGR, DERegionNa)
hex10_BGR_l <- imap(grass_sf, ~aggregate_grass(.x, hex10_BGR))
imap(hex10_BGR_l, function(x,y){x |> st_transform(4326) |> write_sf(gpkg_path, glue("hex10_BGR_{y}"),delete_layer = TRUE)})

hex10_kantone <- hexagonize(hex10,kantone, NAME)
hex10_kantone_l <- imap(grass_sf, ~aggregate_grass(.x, hex10_kantone))
imap(hex10_kantone_l, function(x,y){x |> st_transform(4326) |> write_sf(gpkg_path, glue("hex10_kantone_{y}"),delete_layer = TRUE)})

hex10_l <- imap(grass_sf, ~aggregate_grass(.x, hex10))
imap(hex10_l, function(x,y){x |> st_transform(4326) |> write_sf(gpkg_path, glue("hex10_{y}"),delete_layer = TRUE)})
grass_sf <- map(grass_sf, \(x) st_join(x, hex10))

hex20_l <- imap(grass_sf, ~aggregate_grass(.x, hex20))
imap(hex20_l, function(x,y){x |> st_transform(4326) |> write_sf(gpkg_path, glue("hex20_{y}"),delete_layer = TRUE)})
grass_sf <- map(grass_sf, \(x) st_join(x, hex20))


BGR <- BGR |> 
  group_by(BGR = DERegionNa) |> 
  summarise()


BGR_l <- imap(grass_sf, ~aggregate_grass(.x, BGR))
imap(BGR_l, function(x,y){x |> st_transform(4326) |> write_sf(gpkg_path, glue("BGR_{y}"),delete_layer = TRUE)})
grass_sf <- map(grass_sf, \(x) st_join(x, BGR))


kantone <- kantone |> 
  group_by(kantone = NAME) |> 
  summarise()

kantone_l <- imap(grass_sf, ~aggregate_grass(.x, kantone))
imap(kantone_l, function(x,y){x |> st_transform(4326) |> write_sf(gpkg_path, glue("kantone_{y}"),delete_layer = TRUE)})
grass_sf <- map(grass_sf, \(x) st_join(x, kantone))

layers <- tibble(layer_name = st_layers(gpkg_path)$name)

layers <- layers |> 
  extract(layer_name,c("aggregation","dataset"),"(\\w+)_(\\w+)",remove = FALSE) |> 
  separate(aggregation, c("aggregation1","aggregation2"),sep = "_",fill = "right")

write_sf(layers, gpkg_path, "layers_overview")




calculate_precision <- function(lat,lng,precision,crs = 4326){
  # rounded <- sapply(coords, \(x) round(x,precision))
  coords <- cbind(lat,lng) |> 
    as.data.frame()
  coords_sf <- st_as_sf(coords, coords = c("lat","lng"),crs = crs)
  
  added <- lapply(coords, \(x) x+1/10^precision) |> as.data.frame()
  added_sf <- st_as_sf(added, coords = c("lat","lng"),crs = crs)
  
  st_distance(coords_sf,added_sf,by_element = TRUE)

}

calculate_precision(grass_df$normallandschaft$breite,grass_df$normallandschaft$lange,2) |> summary()

# imap(grass_df, ~write_csv(.x, glue("appdata/{.y}.csv")))
# this following line reduces the precision to 2 decimal places 
# in WGS84 is about 1'500m (+/- 750m?). With this approach, the 
# raw data can basically be made publicly accessible (I need to confirm this
# with deng). It's important to do this rounding on the basis on WGS84 coordinates
# since the coordinates in 2056 are in some cases already a round number (since
# they originate from a "grid" placed over Switzerland in 21781 or 2056)
grass_sf |> 
  imap(\(x,y){
    x |> 
      select(-ends_with("lv95")) |> 
      mutate(across(c(lange,breite),\(z)round(z,2))) |> 
      st_drop_geometry() |> 
      write_csv(glue("appdata/{y}.csv"))
  })
