

library(tidyverse)
library(sf)
library(readxl)


grass <- readxl::read_excel("grassland-data/Grassland_ALLEMA-BDM-WBS_v.5.xlsx", "Data file")

grass_sf <- st_as_sf(grass, coords = c("X_LV95","Y_LV95"),crs = 2056)

ext <- st_bbox(grass_sf) |> st_as_sfc()

grid <- st_make_grid(ext, 5000,square = FALSE)

grid_count <- aggregate(grass_sf[,"Artenreichtum"], grid, FUN = length)
grid_mean <- aggregate(grass_sf[,"Artenreichtum"], grid, FUN = mean, na.rm = TRUE)

grid_agg |> 
  filter(Artenreichtum< 5)

ggplot(na.omit(grid_mean)) + geom_sf(aes(fill = Artenreichtum)) + 
  scale_fill_gradient(low = "blue", high = "red") +
  geom_sf(data = filter(grid_mean, Artenreichtum < 5), fill = "black")

grass$Höhenzone

colnames(grass)

grass$Sömmerung

ggplot(grass, aes(Jahr)) +
    geom_histogram(binwidth = 1)


grass |>
    mutate(Kanton = fct_reorder(Kanton...20, Artenreichtum)) |>
    ggplot(aes(Artenreichtum, Kanton)) +
    geom_boxplot()

grass |>
    mutate(Soemmerung = fct_recode(as.character(Sömmerung), Ja = "1", Nein = "0")) |>
    ggplot(aes(Soemmerung, Artenreichtum, group = Soemmerung)) +
        geom_boxplot() +
  facet_wrap(~Höhenzone)


grass |>
    mutate(Baumschicht = as.double(Baumschicht)) |>
    ggplot(aes(Baumschicht, Artenreichtum)) +
    geom_point()





colnames(grass)


# grass_arten <- grass[,c(1,68:215)]


# grass_arten <- grass_arten[,!str_detect(colnames(grass_arten), "probability|probabiity")]


# colnames(grass_arten)


# grass_arten_l <- grass_arten |>
#     pivot_longer(-Plot.ID) |>
#     mutate(name = str_remove(name,"\\s\\(.+\\)"))


# grass_arten_l <- grass_arten_l |>
#     na.omit() |>
#     group_by(Plot.ID,name) |>
#     summarise(value = TRUE)

# grass_arten_l |>
#     group_by(Plot.ID)  |>
#     add_count(sort = TRUE)


# grass_arten_w <- grass_arten_l |>
#     pivot_wider(names_from = name, values_from = value, values_fill = FALSE) |>
#     ungroup()  |>
#     select(-Plot.ID) |>
#     mutate(across(.fns = ~replace_na(., FALSE)))


# grass_arten_m <- as.matrix(grass_arten_w)

# grass_arten_m[11,]


rowSums(grass_arten_m,na.rm = TRUE)



