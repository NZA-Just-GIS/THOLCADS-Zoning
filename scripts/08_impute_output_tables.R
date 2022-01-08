
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
  # Black
  left_join(ads1, by = "unique_id") %>%
  # Building Age
  left_join(ads2, by = "unique_id") %>%
  # 'Foreign Born'
  left_join(ads3, by = "unique_id") %>%
  # Income
  left_join(ads4, by = "unique_id") %>%
  # Occupation
  left_join(ads5, by = "unique_id") %>%
  # Repair, Mortgage
  left_join(ads6, by = "unique_id") %>%
  distinct() %>%
  print()


##------------------------------------
##  Clean up
##------------------------------------

## Clean
df_clean <- df %>%
  dplyr::rename_all(toupper) %>%
  dplyr::rename(
    P_BLACK = BLK_NUM,
    P_FB = FB_NUM,
    BLACK_TXT = BLK_TXT,
    INC_TXT = INC_OCC_TXT
    ) %>%
  select(
    # basic info
    UNIQUE_ID, STATE, CITY, METRO, HOLC_GRADE, HOLC_ID, REGION, ADS_TYPE, 
    # race/fb
    P_BLACK, P_FB, FB_GROUP,
    # occupation/income
    OCC_CLASS, MID_INC,
    # building age, repair/mortgage data
    MID_AGE, REPAIR, MORT_AV, MORT_FHA,
    # text
    BLACK_TXT, FB_TXT,
    OCC_TXT, INC_TXT,
    AGE_TXT, REPAIR_TXT, MORT_TXT
  ) %>%
  # factor reorder
  mutate_at(
    vars(REPAIR, MORT_AV),
      ~factor(.,levels = c("Good", "Fair-Good", "Fair", "Fair-Poor", "Poor", "Other_NA"))
  ) %>%
  mutate(
    OCC_CLASS =
      factor(OCC_CLASS, levels = c("Upper", "Up_Mid", "Mid_Mix", "Low_Mid", "Lower", "Other_NA"))
  ) %>%
  # fix age --> all zeros become 0.5
  mutate(MID_AGE = ifelse(MID_AGE < 0.5, 0.5, MID_AGE)) %>%
  distinct() %>%
  glimpse()



###################################################################################
### FILL in MISSING Data
###################################################################################

## prep
df_clean1 <- df_clean %>%
  ## needed for joins
  mutate(
    OCC_CLASS = paste0(OCC_CLASS, "_OC"),
    REPAIR = paste0(REPAIR, "_R"),
    MORT_AV = paste0(MORT_AV, "_AV")
    ) %>%
  print()


##------------------------
##  % Foreign Born
##------------------------

## regression
reg1 <- df_clean1 %>% 
  # keep only nhoods w/ > 0 FB pop
  filter(P_FB > 0) %>%
  lm(
    log(P_FB) ~
      METRO +
      OCC_CLASS +
      HOLC_GRADE +
      MORT_AV +
      P_BLACK,
    data = .
  )

summary(reg1)


## get coefficients
df_coef <- bind_cols(
  rownames(summary(reg1)$coefficients), 
  summary(reg1)$coefficients[,1]
) %>% 
  dplyr::rename(
    variables = 1, 
    coef = 2
  ) %>%
  # remove var names to allow join
  mutate(
    var2 = str_replace(variables, "METRO|OCC_CLASS|HOLC_GRADE|MORT_AV", "")
  ) %>%
  print()


## make results table
reg_results <- reg1$model %>% 
  as_tibble() %>%
  # remove value
  select(-1) %>% 
  distinct() %>% 
  # fill out data set
  tidyr::expand(METRO, OCC_CLASS, HOLC_GRADE, MORT_AV) %>%
  bind_cols(
    reg1$coefficients[1], # Intercept
    reg1$coefficients["P_BLACK"]  # % Black
  ) %>%
  dplyr::rename(
    Intercept = 5,
    coef_BLACK = 6
  ) %>%
  # join to coefficient df
  left_join(df_coef[-1], by = c("METRO" = "var2")) %>%
  left_join(df_coef[-1], by = c("OCC_CLASS" = "var2"), suffix = c("_METRO", "_OCC")) %>%
  left_join(df_coef[-1], by = c("HOLC_GRADE" = "var2")) %>%
  left_join(df_coef[-1], by = c("MORT_AV" = "var2"), suffix = c("_HOLC", "_MORT")) %>% 
  #rename(coef_HOLC = coef) %>%
  # change NAs to zeros
  mutate_at(vars(contains("coef_")), ~ifelse(is.na(.), 0, .)) %>%
  distinct() %>%
  print()


## add to data and update
df_fix <- df_clean1 %>% 
  left_join(reg_results, by = c("METRO", "OCC_CLASS", "HOLC_GRADE", "MORT_AV")) %>%
  mutate(
    FB_FLAG = ifelse(is.na(P_FB), 1, 0),  # flag imputed cases
    FB_est = exp(Intercept + coef_BLACK*P_BLACK + coef_METRO + coef_OCC + coef_HOLC + coef_MORT),  # regression estimates
    P_FB = ifelse(is.na(P_FB), FB_est, P_FB)  # replace MID_INC var
  ) %>%
  distinct() %>%
  select(UNIQUE_ID:MORT_FHA, contains("TXT"), FB_FLAG) %>%
  glimpse()



##------------------------
##  Income
##------------------------

## regression
reg1 <- lm(
  log(MID_INC) ~
    REGION +
    OCC_CLASS +
    HOLC_GRADE +
    REPAIR +
    P_BLACK,
  data = df_fix
)

# summary
summary(reg1)


## get coefficients
df_coef <- bind_cols(
  rownames(summary(reg1)$coefficients), 
  summary(reg1)$coefficients[,1]
  ) %>% 
  dplyr::rename(
    variables = 1, 
    coef = 2
    ) %>%
  # remove var names to allow join
  mutate(
    var2 = str_replace(variables, "REGION|OCC_CLASS|HOLC_GRADE|REPAIR", "")
  ) %>%
  print()


## make results table
reg_results <- reg1$model %>% 
  as_tibble() %>%
  # remove value
  select(-1) %>% 
  distinct() %>% 
  # fill out data set
  tidyr::expand(REGION, OCC_CLASS, HOLC_GRADE, REPAIR) %>%
  bind_cols(
    reg1$coefficients[1], # Intercept
    reg1$coefficients["P_BLACK"]  # % Black
    ) %>%
  dplyr::rename(
    Intercept = 5,
    coef_BLACK = 6
    ) %>%
  # join to coefficient df
  left_join(df_coef[-1], by = c("REGION" = "var2")) %>%
  left_join(df_coef[-1], by = c("OCC_CLASS" = "var2"), suffix = c("_REGION", "_OCC")) %>%
  left_join(df_coef[-1], by = c("HOLC_GRADE" = "var2")) %>%
  left_join(df_coef[-1], by = c("REPAIR" = "var2"), suffix = c("_HOLC", "_REPAIR")) %>%
  #rename(coef_HOLC = coef) %>%
  # change NAs to zeros
  mutate_at(vars(contains("coef_")), ~ifelse(is.na(.), 0, .)) %>%
  distinct() %>%
  print()


## add to data and update
df_fix1 <- df_fix %>% 
  left_join(reg_results, by = c("REGION", "OCC_CLASS", "HOLC_GRADE", "REPAIR")) %>%
  mutate(
    INC_FLAG = ifelse(is.na(MID_INC), 1, 0),  # flag imputed cases
    INC_est = exp(Intercept + coef_BLACK*P_BLACK + coef_REGION + coef_OCC + coef_HOLC + coef_REPAIR),  # regression estimates
    MID_INC = ifelse(is.na(MID_INC), INC_est, MID_INC)  # replace MID_INC var
  ) %>%
  distinct() %>%
  select(UNIQUE_ID:MORT_FHA, contains("TXT"), contains("FLAG")) %>%
  glimpse()



##------------------------
##  Building Age
##------------------------


## regression
reg1 <- lm(
  log(MID_AGE) ~
    STATE +
    REPAIR +
    HOLC_GRADE +
    MID_INC +
    P_FB +
    P_BLACK,
  data = df_fix1
)

# summary
summary(reg1)


## get coefficients
df_coef <- bind_cols(
  rownames(summary(reg1)$coefficients), 
  summary(reg1)$coefficients[,1]
) %>% 
  dplyr::rename(
    variables = 1, 
    coef = 2
  ) %>%
  # remove var names to allow join
  mutate(
    var2 = str_replace(variables, "STATE|REPAIR|HOLC_GRADE", "")
  ) %>%
  print()


## make results table
reg_results <- reg1$model %>% 
  as_tibble() %>%
  select(-1) %>% 
  distinct() %>% 
  # fill out data set
  tidyr::expand(STATE, REPAIR, HOLC_GRADE) %>%
  bind_cols(
    reg1$coefficients[1],  #Intercept
    reg1$coefficients["P_BLACK"],  # % Black
    reg1$coefficients["P_FB"],  # % Foreign Born
    reg1$coefficients["MID_INC"]  # Income
    ) %>%
  dplyr::rename(
    Intercept = 4,
    coef_BLACK = 5,
    coef_FB = 6,
    coef_INC = 7
    ) %>%
  # join to coefficient df
  left_join(df_coef[-1], by = c("STATE" = "var2")) %>%
  left_join(df_coef[-1], by = c("REPAIR" = "var2"), suffix = c("_STATE", "_REPAIR")) %>%
  left_join(df_coef[-1], by = c("HOLC_GRADE" = "var2")) %>%
  dplyr::rename(coef_HOLC = coef) %>%
  # change NAs to zeros
  mutate_at(vars(contains("coef_")), ~ifelse(is.na(.), 0, .)) %>%
  distinct() %>%
  print()



## add to data and update
df_fix2 <- df_fix1 %>% 
  left_join(reg_results, by = c("STATE", "REPAIR", "HOLC_GRADE")) %>%
  mutate(
    AGE_FLAG = ifelse(is.na(MID_AGE), 1, 0),
    AGE_est = exp(Intercept + coef_BLACK*P_BLACK + coef_FB*P_FB + coef_INC*MID_INC + coef_STATE + coef_REPAIR + coef_HOLC),
    MID_AGE = ifelse(is.na(MID_AGE), AGE_est, MID_AGE)
  ) %>%
  distinct() %>%
  select(UNIQUE_ID:MORT_FHA, contains("TXT"), contains("FLAG")) %>%
  # remove single case missing data
  filter(!is.na(MID_INC) & !is.na(MID_AGE)) %>%
  # fix names
  mutate(
    OCC_CLASS = str_replace(OCC_CLASS, "_OC", ""),
    REPAIR = str_replace(REPAIR, "_R", ""),
    MORT_AV = str_replace(MORT_AV, "_AV", "")
  ) %>%
  glimpse()



##------------------------------
## Save!!
##------------------------------

write_csv(df_fix2, "DATA_DOWNLOAD/TABLES/ADS_FINAL.csv")



##############################################
##  Generate Summary Stat Tables
##############################################

## Look at building age, % black, and % foreign born by region & nhood
df_org <- df %>%
  mutate(
    inc_miss = ifelse(is.na(mid_inc), 1, 0),
    fb_miss = ifelse(is.na(fb_num), 1, 0),
    city_state = paste(city, state, sep = ", ")
    ) %>%
  group_by(region) %>%
  mutate(cities = length(unique(city_state))) %>%
  group_by(holc_grade, region, cities) %>%
  summarize(
    bdg_age_mid = mean(mid_age, na.rm = TRUE),
    mid_inc = mean(mid_inc, na.rm = TRUE),
    black = mean(blk_num, na.rm = TRUE),
    fb = mean(fb_num, na.rm = TRUE),
    inc_miss = sum(inc_miss),
    fb_miss = sum(fb_miss),
    nhoods = dplyr::n()
  ) %>% 
  filter(holc_grade != "E") %>%
  select(holc_grade, region, bdg_age_mid:fb_miss, cities, nhoods) %>%
  arrange(region, holc_grade) %>%
  dplyr::rename(
    "HOLC Grade" = holc_grade,
    "Region" = region,
    "Building Age Midpoint" = bdg_age_mid,
    "Family Income Midpoint" = mid_inc,
    "Black (%)" = black,
    "Foreign Born (%)" = fb,
    "Missing Family Income" = inc_miss,
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
  file = "DATA_DOWNLOAD/TABLES/Sum_Stats.xlsx",
  overwrite = TRUE
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
  dplyr::rename(
    City = city_state,
    Metro = metro,
    "HOLC Neighborhoods" = holc_nhoods
  ) %>%
  print()

## Northeast
ne <- df_cities %>% filter(region == "NE") %>%
  select(-region) %>%
  arrange(-holc_nhoods, metro) %>%
  dplyr::rename(
    City = city_state,
    Metro = metro,
    "HOLC Neighborhoods" = holc_nhoods
  ) %>%
  print()

## South
s <- df_cities %>% filter(region == "S") %>%
  select(-region) %>%
  arrange(-holc_nhoods, metro) %>%
  dplyr::rename(
    City = city_state,
    Metro = metro,
    "HOLC Neighborhoods" = holc_nhoods
  ) %>%
  print()

## West
w <- df_cities %>% filter(region == "W") %>%
  select(-region) %>%
  arrange(-holc_nhoods, metro) %>%
  dplyr::rename(
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
  "DATA_DOWNLOAD/TABLES/Cities_by_Region.xlsx",
  overwrite = TRUE
)



#####################################################
##  Move holc_cities to DATA_DOWNLOAD FOLDER
#####################################################

## Read in
holc_cities <- read_csv("tables/holc_cities.csv") %>%
  print()


## Save out
write_csv(holc_cities, "DATA_DOWNLOAD/TABLES/HOLC_Cities.csv")





