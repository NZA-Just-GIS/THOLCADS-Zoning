#########################################################################
#########################################################################
###                                                                   ###
###              Map mentions of Zoning in ADS
###                                                                   ###
#########################################################################
#########################################################################

## prepare workspace
source("scripts_nza/00_preamble.R")

## Now read your files (local paths)
shapefile <- st_read("~/Downloads/R/THOLCADS-Zoning/DATA_DOWNLOAD/SHAPES/HOLC_Data.geojson") # this is my local path  
## Appending classification results to avoid re-running it again
zoning_analysis_results5 <- read_excel("Downloads/R/zoning_analysis_results5.xlsx") # this is my local saved file
zoning_analysis_results5$unique_id <- ADS_combo$unique_id
write.xlsx(zoning_analysis_results5, "zoning_analysis_results6.xlsx") # this file is on the shared box

## Add geometries
zoning_analysis_results5 <- merge(
  zoning_analysis_results5, 
  shapefile[, c("UNIQUE_ID", "geometry")], 
  by.x = "unique_id",  
  by.y = "UNIQUE_ID", 
  all.x = TRUE
)

##Creates binary flag across rows 
zoning_analysis_results5$zoning_flag <- ifelse(
  zoning_analysis_results5$fav_inf_class == "zoning" | 
    zoning_analysis_results5$det_inf_class == "zoning" | 
    zoning_analysis_results5$remarks_class == "zoning", 
  1, 0
)

## Convert to sf object
zoning_analysis_results5 <- st_as_sf(zoning_analysis_results5)

# Explicitly set factor levels
zoning_analysis_results5$holc_grade <- factor(zoning_analysis_results5$holc_grade, 
                                            levels = c("A", "B", "C", "D", "E"))

## Use the same  palette for both layers
palette <- c("A" = "#4daf4a", "B" = "#377eb8", "C" = "#F1C40F", "D" = "#C0392B", "E" = "gray60")


## First layer consisting of HOLC grade with transparency at 0.25
layer1 <- mapview(
  zoning_analysis_results5,
  zcol = "holc_grade",      
  col.regions = palette,    
  layer.name = "HOLC Grade",
  legend = TRUE,
  alpha.regions = 0.25
)

## Second layer with ust zoning areas with high transparency
zoning_areas <- zoning_analysis_results5[zoning_analysis_results5$zoning_flag == 1, ]
layer2 <- mapview(
  zoning_areas,
  zcol = "holc_grade",      
  col.regions = palette,    
  layer.name = "Areas with Zoning",
  legend = FALSE,  # No need for second legend since colors are same
  alpha.regions = 1
)

## Combine layers
combined_map <- layer1 + layer2
mapshot(combined_map, file = "holc_zoning_map.html")
combined_map