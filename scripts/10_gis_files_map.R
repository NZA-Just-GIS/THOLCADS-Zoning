
#########################################################################
#########################################################################
###                                                                   ###
###                           Make shapes                             ###
###                                                                   ###
#########################################################################
#########################################################################

## PREPARE WORKSPACE
source("scripts/00_preamble.R")


#################################################################################
## LOAD HOLC data
#################################################################################

##-----------------------------------------------------
##  GeoJSON Import
##-----------------------------------------------------

holc_json <- st_read("tables/holc_json.GeoJSON")  # import
summary(holc_json)  # inspect


## Convert from ST to SF
holc_sf <- st_as_sf(holc_json) %>%
  # clean up unique ID
  mutate(
    UNIQUE_ID = paste(state, city, holc_id, sep = "_"),
    UNIQUE_ID = str_replace_all(UNIQUE_ID, ",", ""),  # remove all commas
    UNIQUE_ID = str_replace_all(UNIQUE_ID, "\\.", ""),  # remove all periods
    UNIQUE_ID = str_replace(UNIQUE_ID, " and ", ""),  # remove "and"s
    UNIQUE_ID = str_replace_all(UNIQUE_ID, "[[:space:]]", "")  # remove all spaces
  ) %>%
  dplyr::select(UNIQUE_ID, geometry) %>%
  print() # inspect


##--------------------------------------------------
##  Import table
##--------------------------------------------------

holc_tables <- read_csv("DATA_DOWNLOAD/TABLES/ADS_FINAL.csv") %>%
  print()


#################################################################################
## Join and save
#################################################################################

##-------------------
## Join
##-------------------

holc_join <- holc_sf %>%
  left_join(holc_tables, by = "UNIQUE_ID") %>%
  # remove cities missing table data
  drop_na(HOLC_ID) %>%
  # drop txt & flag vars
  dplyr::select(UNIQUE_ID:MORT_FHA, contains("FLAG")) %>%
  glimpse()
  

##-----------------------
## Save as shapefile
##-----------------------

# Create new folder
dir.create("DATA_DOWNLOAD/SHAPES")
dir.create("DATA_DOWNLOAD/SHAPES/ESRI_SHP")

st_write(
  holc_join,
  dsn = "DATA_DOWNLOAD/SHAPES/ESRI_SHP",
  layer = "HOLC_DATA",
  driver = "ESRI Shapefile"
)


##---------------------------
## Save as GeoJSON file
##---------------------------

st_write(
  holc_join,
  dsn = "DATA_DOWNLOAD/SHAPES/HOLC_DATA.GeoJSON",
  layer = "HOLC_DATA.GeoJSON",
  driver = "GeoJSON",
  layer_options = "OVERWRITE=true"
)



#################################################################################
## Map and inspect
#################################################################################

## Clean file
HOLC <- holc_join %>%
  dplyr::select(
    UNIQUE_ID:REGION, 
    MID_AGE, P_BLACK, P_FB, FB_GROUP,
    MID_INC, OCC_CLASS, REPAIR, MORT_AV, MORT_FHA,
    contains("FLAG")
    ) %>%
  dplyr::rename(`N'hood Grade` = HOLC_GRADE) %>%
  print()


## Set HOLC colors
palette <- c("#4daf4a", "#377eb8", "#F1C40F", "#C0392B", "gray60")


## Create interactive map
mapview(
  HOLC,  # geo file
  alpha.regions = 0.5,  # polygon transparency
  zcol = "N'hood Grade",  # data to display
  col.regions = palette,  # colors to display
  lwd = 1.5  # line width
  )



##----------------------------------------------------
##  Export interactive Map as HTML if preferred
##----------------------------------------------------

## Set mapview options to allow export
mapviewOptions(fgb = FALSE)

## create map object
 map_obj <- mapview(
    HOLC,  # geo file
    alpha.regions = 0.5,  # polygon transparency
    zcol = "N'hood Grade",  # data to display
    col.regions = palette,  # colors to display
    lwd = 1.5  # line width
 )@map %>% 
   # set zoom on NYC
   setView(lng = -73.98743, lat = 40.7743, zoom = 10)
 
## Save out as HTML
 mapshot(
    map_obj, 
    url = "holc_map.html",
    remove_controls = c("homeButton", "drawToolbar", "easyButton")
    )

## Delete extra folder
unlink("holc_map_files", recursive = TRUE)


##-----------------------------------------------------
## Clean up --> deletes these files permanently
##-----------------------------------------------------

for(i in unique(c("Building_Age", "Black", "Foreign_Born", "Income", "Occupation", "Repair_Mortgage"))){
   
   file <- paste0("DATA_DOWNLOAD/ADS_", i, ".csv")
   
   # delete
   unlink(file, recursive = TRUE)
   
}
