
#########################################################################
#########################################################################
###                                                                   ###
###             Generate Table Outputs and Summary Statistics         ###
###                                                                   ###
#########################################################################
#########################################################################

## PREPARE WORKSPACE
source("scripts/00_preamble.R")


##############################################
##  LOAD DATA
##############################################

##-----------------------------------------------
## Grab Desc. Sheet data
##-----------------------------------------------

ads <- NULL
for(i in seq(1:3)){
  
  temp <- openxlsx::read.xlsx(
    "DATA_DOWNLOAD/TABLES/ADS_organized.xlsx",
    sheet = i
  )
  
  temp1 <- temp %>%
    select(state:ads_type) %>%
    as_tibble()
  
  ads <- bind_rows(ads, temp1)
  
}

ads  # inspect


##-----------------------------------------------
## Grab cleaned data
##-----------------------------------------------

filelist <- list.files("DATA_DOWNLOAD", "*.csv")
filelist

for(i in 1:length(filelist)){
  
  file <- paste0("DATA_DOWNLOAD/", filelist[i])
  
  temp <- read_csv(file)
  
  name <- paste0("ads", i)
  
  assign(name, temp)
  
}


## create dataframe
df <- ads %>%
  left_join(ads1, by = "unique_id") %>%
  left_join(ads2, by = "unique_id") %>%
  left_join(ads3[c(1, 4, 5, 6)], by = "unique_id") %>%
  rename(fb_group = group) %>%
  print()


##------------------------------------
##  Clean up Save out!!
##------------------------------------

## Clean
df_clean <- df %>%
  rename_all(toupper) %>%
  rename(
    BLK_TEXT = BLACK,
    P_BLACK = BLK_NUM,
    P_FOR_BORN = FB_NUM,
    FB_TEXT = TEXT,
    FB_GROUP = FB_GROUP
  ) %>%
  select(
    UNIQUE_ID, STATE, CITY, METRO, HOLC_GRADE, HOLC_ID, REGION, ADS_TYPE, 
    P_BLACK, BLK_TEXT, MIN_AGE, MID_AGE, MAX_AGE, P_FOR_BORN, FB_TEXT
  ) %>%
  print()


## Save
write_csv(df_clean, "DATA_DOWNLOAD/TABLES/ADS_FINAL.csv")
  

##############################################
##  Generate Summary Stat Tables
##############################################

## Look at building age, % black, and % foreign born by region & nhood
df_org <- df %>%
  mutate(
    fb_miss = ifelse(is.na(fb_num), 1, 0),
    city_state = paste(city, state, sep = ", ")
    ) %>%
  group_by(region) %>%
  mutate(cities = length(unique(city_state))) %>%
  group_by(holc_grade, region, cities) %>%
  summarize(
    bdg_age_mid = mean(mid_age, na.rm = TRUE),
    #bdg_age_avg = mean(avg_age, na.rm = TRUE),
    black = mean(blk_num, na.rm = TRUE),
    fb = mean(fb_num, na.rm = TRUE),
    fb_miss = sum(fb_miss),
    nhoods = dplyr::n()
  ) %>% 
  filter(holc_grade != "E") %>%
  select(holc_grade, region, bdg_age_mid:fb_miss, cities, nhoods) %>%
  arrange(region, holc_grade) %>%
  rename(
    "HOLC Grade" = holc_grade,
    "Region" = region,
    "Building Age Midpoint" = bdg_age_mid,
    "Black (%)" = black,
    "Foreign Born (%)" = fb,
    "Missing For. Born" = fb_miss,
    "HOLC Cities" = cities,
    "HOLC Neighborhoods" = nhoods
  ) %>%
  print()



##---------------------------------
## SAVE OUT
##---------------------------------

write.xlsx(
  df_org,
  file = "DATA_DOWNLOAD/TABLES/Sum_Stats.xlsx"
)


######################################################
## Organize table by Metro Areas by Region
######################################################

## Prepare Cities
df_cities <- df %>%
  mutate(city_state = paste(city, state, sep = ", ")) %>%
  group_by(city_state, metro, region) %>%
  summarize(holc_nhoods = dplyr::n()) %>%
  print()


## Midwest
mw <- df_cities %>% filter(region == "MW") %>%
  select(-region) %>%
  arrange(-holc_nhoods, metro) %>%
  rename(
    City = city_state,
    Metro = metro,
    "HOLC Neighborhoods" = holc_nhoods
  ) %>%
  print()

## Northeast
ne <- df_cities %>% filter(region == "NE") %>%
  select(-region) %>%
  arrange(-holc_nhoods, metro) %>%
  rename(
    City = city_state,
    Metro = metro,
    "HOLC Neighborhoods" = holc_nhoods
  ) %>%
  print()

## South
s <- df_cities %>% filter(region == "S") %>%
  select(-region) %>%
  arrange(-holc_nhoods, metro) %>%
  rename(
    City = city_state,
    Metro = metro,
    "HOLC Neighborhoods" = holc_nhoods
  ) %>%
  print()

## West
w <- df_cities %>% filter(region == "W") %>%
  select(-region) %>%
  arrange(-holc_nhoods, metro) %>%
  rename(
    City = city_state,
    Metro = metro,
    "HOLC Neighborhoods" = holc_nhoods
  ) %>%
  print()


##---------------------------------
## SAVE OUT
##---------------------------------

## make list for xlsx
df_list <- list(
  "midwest" = mw, 
  "northeast" = ne, 
  "south" = s,
  "west" = w
)


## Save out as xlsx
openxlsx::write.xlsx(
  df_list,
  "DATA_DOWNLOAD/TABLES/Cities_by_Region.xlsx"
)



#####################################################
##  Move holc_cities to DATA_DOWNLOAD FOLDER
#####################################################

## Read in
holc_cities <- read_csv("tables/holc_cities.csv") %>%
  print()


## Save out
write_csv(holc_cities, "DATA_DOWNLOAD/TABLES/HOLC_Cities.csv")


##---------------------------------
## Clean up
##---------------------------------

for(i in unique(c("Building_Age", "Black", "Foreign_Born"))){
  
  file <- paste0("DATA_DOWNLOAD/ADS_", i, ".csv")
  
  # delete
  unlink(file, recursive = TRUE)
  
}

