
#########################################################################
#########################################################################
###                                                                   ###
###                 RUN BASIC PRELIMINARY ANALYES                     ###
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
    "output/area_desc_sheets.xlsx",
    sheet = i
  )
  
  temp1 <- temp %>%
    select(state:ads_type) %>%
    as_tibble()
  
  ads <- bind_rows(ads, temp1)
  
}

ads

##-----------------------------------------------
## Grab cleaned data
##-----------------------------------------------

filelist <- list.files("output", "*.csv")
filelist

for(i in 1:length(filelist)){
  
  file <- paste0("output/", filelist[i])
  
  temp <- read_csv(file)
  
  name <- paste0("ads", i)
  
  assign(name, temp)
  
}


## create dataframe
df <- ads %>%
  left_join(ads1, by = "unique_id") %>%
  left_join(ads2[c(1, 4)], by = "unique_id") %>%
  left_join(ads3[c(1, 4, 6)], by = "unique_id") %>%
  print()


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
  file = "output/Sum_Stats.xlsx"
)


######################################################
## Organize table by Metro Areas by Region
######################################################

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
  "output_prelim/cities_by_region.xlsx"
)


##############################################
##  Make Bar Graph
##############################################

## Building Age
bldg_age <- df_org %>%
  ggplot(aes(x = Region, y = `Building Age Midpoint`)) +
  geom_bar(aes(fill = `HOLC Grade`), stat = "identity", width = 0.8, position = position_dodge(0.9)) +
  scale_fill_manual(values = c("#4daf4a", "#377eb8", "#F1C40F", "#C0392B")) +
  theme_light()

bldg_age

## Percent Black
pblk <- df_org %>%
  ggplot(aes(x = Region, y = `Black (%)`)) +
  geom_bar(aes(fill = `HOLC Grade`), stat = "identity", width = 0.8, position = position_dodge(0.9)) +
  scale_fill_manual(values = c("#4daf4a", "#377eb8", "#F1C40F", "#C0392B")) +
  theme_light()

pblk

## Percent For. Born
pfb <- df_org %>%
  ggplot(aes(x = Region, y = `Foreign Born (%)`)) +
  geom_bar(aes(fill = `HOLC Grade`), stat = "identity", width = 0.8, position = position_dodge(0.9)) +
  scale_fill_manual(values = c("#4daf4a", "#377eb8", "#F1C40F", "#C0392B")) +
  theme_light()

pfb


##-----------------------------------
## SAVE FIGURES as TIFFs
##-----------------------------------

## Create new folder and export
dir.create("bar_graphs")


## Save out as TIFFs
graph_list <- list(bldg_age, pblk, pfb)
for(i in 1:length(graph_list)){
  
  if(i == 1){
    name <- "bldg_age"
  } else if(i == 2){
    name <- "pblk"
  } else{
    name <- "pfb"
  }
  
  filename = paste0("bar_graphs/", name, ".tif")
  tiff(filename, width = 420, height = 365)
  print(graph_list[i])
  dev.off()
  
}





