
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

## Load mapview package
packages(mapview)

## Clean file
HOLC <- holc_join %>%
  select(UNIQUE_ID:REGION, MID_AGE, P_BLACK, P_FOR_BORN, FB_TEXT) %>%
  rename(`N'hood Grade` = HOLC_GRADE) %>%
  print()

## Set HOLC colors
palette <- c("#4daf4a", "#377eb8", "#F1C40F", "#C0392B", "darkgray")

## Create interactive map
mapview(
  HOLC,  # geo file
  alpha.regions = 0.5,  # polygon transparency
  zcol = "N'hood Grade",  # data to display
  col.regions = palette,  # colors to display
  lwd = 1.5  # line width
  )
