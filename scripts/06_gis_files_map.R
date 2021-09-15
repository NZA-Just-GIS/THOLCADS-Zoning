
#########################################################################
#########################################################################
###                                                                   ###
###                           Make shapes                             ###
###                                                                   ###
#########################################################################
#########################################################################

## PREPARE WORKSPACE
source("scripts/00_preamble.R")

#packages(tmap)
#packages(mapview)


#################################################################################
## LOAD HOLC data
#################################################################################

##-----------------------------------------------------
##  GeoJSON Import
##-----------------------------------------------------

## For once data is saved locally
holc_json <- rgdal::readOGR("tables/holc_json.GeoJSON")  # import
summary(holc_json)  # inspect


## Convert from ST to SF
holc_sf <- st_as_sf(holc_json) %>%
  mutate(UNIQUE_ID = paste(state, city, holc_id, sep = "_")) %>%
  select(UNIQUE_ID, geometry) %>%
  print() # inspect


##--------------------------------------------------
##  Import table
##--------------------------------------------------

holc_tables <- read_csv("DATA_DOWNLOAD/ADS_FINAL.csv") %>%
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
  glimpse()
  

##-----------------------
## Save as shapefile
##-----------------------

# Create new folder
dir.create("SHAPES")
dir.create("SHAPES/ESRI_SHP")

st_write(
  holc_join,
  dsn = "SHAPES/ESRI_SHP",
  layer = "HOLC_DATA",
  driver = "ESRI Shapefile"
)

##---------------------------
## Save as GeoJSON file
##---------------------------

st_write(
  holc_join,
  dsn = "SHAPES/HOLC_DATA.GeoJSON",
  layer = "HOLC_DATA.GeoJSON",
  driver = "GeoJSON",
  layer_options = "OVERWRITE=true"
)



#################################################################################
## Map and inspect
#################################################################################

packages(mapview)

## Set HOLC colors
palette <- c("#4daf4a", "#377eb8", "#F1C40F", "#C0392B", "darkgray")

## Create interactive map
mapview(
  holc_join,
  alpha.regions = 0.5,
  zcol = "HOLC_GRADE",
  col.regions = palette,
  lwd = 2
  )
