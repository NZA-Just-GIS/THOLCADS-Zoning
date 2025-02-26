library(sf)
shapefiles <- st_read("~/Downloads/DATA_DOWNLOAD/SHAPES/HOLC_DATA.geojson")
ggplot(shapefiles) +
  geom_sf(fill = "#69b3a2", color = "white") +
  theme_void()