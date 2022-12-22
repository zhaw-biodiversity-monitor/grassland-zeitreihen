leaf_map <- leaflet(geodata_i) |> 
  addTiles("https://wmts.geo.admin.ch/1.0.0/ch.swisstopo.swissimage/default/current/3857/{z}/{x}/{y}.jpeg",group = "Swissimage") |>
  addLayersControl(baseGroups = c("Swissimage", "Pixelkarte farbig", "Pixelkarte grau")) |> 
  fitBounds(5.955902,45.81796,10.49206,47.80845 ) |> 
  htmlwidgets::onRender("console.log(100000)")


mypal <- rev(RColorBrewer::brewer.pal(4, "RdBu"))


bivmat <- bivariate_matrix(mypal)
colnames(bivmat) <- 1:4
rownames(bivmat) <- 1:4




mypal <- rev(RColorBrewer::brewer.pal(4, "RdBu"))
show_col(mypal)

sapply(c("RdBu"), \(x) biv_table(x,4,TRUE),simplify = FALSE) |> 
  (\(x)do.call(rbind, x))() |> 
  ggplot(aes(x, z, fill = col)) +
  geom_raster() +
  scale_fill_identity() +
  labs(x = "Artenvielfalt",y = "Anzahl Nachweise") +
  coord_equal() +
  facet_wrap(~pal) +
  theme(panel.background = element_blank(),axis.text = element_blank(),axis.ticks = element_blank())

