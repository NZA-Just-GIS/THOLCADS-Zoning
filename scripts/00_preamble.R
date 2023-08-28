

#########################################################################
#########################################################################
###                                                                   ###
###                             PREAMBLE                              ###
###                                                                   ###
#########################################################################
#########################################################################

## PREPARE WORKSPACE
rm(list = ls())  # clear environment
options(scipen = 999) 
options(digits = 6)
getwd()  # ~/HOLC_ADS

## Load or install packages
packages <- function(x) {
  x <- deparse(substitute(x))
  installed_packages <- as.character(installed.packages()[, 1])
  
  if (length(intersect(x, installed_packages)) == 0) {
    install.packages(pkgs = x, dependencies = TRUE, repos = "http://cran.r-project.org")
  }
  
  library(x, character.only = TRUE)
  rm(installed_packages) # Remove From Workspace
}


packages(tidyverse)
#packages(httr)  # for NHGIS API
#packages(jsonlite)  # for NHGIS API
packages(foreign)  # write out DBFs (for ArcGIS)
packages(sf)  # for spatial/ArcGIS Pro data
packages(tidycensus)  # for getting state/county names/abbrevs.
packages(rgdal)  # import GeoJSON
packages(rapportools)  # for is.empty fn
packages(openxlsx)  # for reading/writing files as Excel workbooks
packages(mapview)
packages(strex)  # for str_after_nth command
packages(downloader)
packages(leaflet)  # for setView
require(dplyr)

