# i had the intuition that filtering the points based on four lat long coordinates from a bbox via a non spatial method (lat > xmin, lat < xmax etc) 
# was faster than filtering by a spatial object. 
# A big downside of this method is, however, that you can only filter via a box (and when using lat / long coordinates, this method is not precise). 
# I therefore considered switching to a st_filter method, but decided to test which method was faster. 
# This script shows this test. It turns out non-spatial filtering takes about half the time it does for a spatial filter. I've therefore decided to stick with
# a non-spatial filter for now.

library(sf)
normallandschaft <- grass_sf$normallandschaft |> 
  st_transform(4326)

bb <- st_bbox(normallandschaft) |> 
  st_as_sfc()


pois <- st_sample(bb,5000)
pois <- cbind(pois, st_coordinates(pois))

polys <- function(bb, n, crs = 4326){
  st_sample(bb,n) |> 
    st_buffer(10000) |> 
    split(1:n) |> 
    map(\(x)st_as_sfc(st_bbox(x))) |> 
    do.call(rbind, args = _) |> 
    st_sfc(crs = crs) 
}

extract <- polys(bb, 1)

bb_ex <- st_bbox(extract)


library(microbenchmark)

mbm <- microbenchmark(
  sf_intersect = st_filter(pois, extract), 
  sf_within = st_filter(pois, extract,.predicate = st_within),
  dplyr_filter = filter(pois, X > bb_ex$xmin,X < bb_ex$xmax, Y > bb_ex$ymin,Y < bb_ex$ymax),
  times = 100
  )

mbm

pois <- st_as_sf(pois)



range_lange <- range(normallandschaft$lange)
range_breite <- range(normallandschaft$breite)


