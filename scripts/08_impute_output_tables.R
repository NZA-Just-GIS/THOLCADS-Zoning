
#########################################################################
#########################################################################
###                                                                   ###
###             Generate Table Outputs and Summary Statistics         ###
###                                                                   ###
#########################################################################
#########################################################################

## PREPARE WORKSPACE
source("scripts/00_preamble.R")
packages(ggpubr)  # for ggdensity plot
packages(olsrr)  # for alternative density plot
packages(cowplot)  # for compbine residual plots
require(dplyr)


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
    dplyr::select(state:ads_type) %>%
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
  dplyr::select(
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
    AGE_TXT, REPAIR_TXT, MORT_TXT,
    # flags
    B_FLAG, FB_FLAG
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
  # fix NA Black (NA = 0)
  mutate(
    P_BLACK = ifelse(is.na(P_BLACK), 0, P_BLACK),
    P_FB = ifelse(is.na(P_FB) & is.na(FB_GROUP), 0, P_FB)
  ) %>%
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


##-------------------------------------------------------------------
##  % Foreign Born
##-------------------------------------------------------------------


## cleaned data -> only FB of greater than 0%
df_clean2 <- df_clean1 %>%
  filter(P_FB > 0) %>%
  print()


## regression
reg1 <- df_clean2 %>%
  lm(
    log(P_FB) ~
    STATE +
    OCC_CLASS +
    HOLC_GRADE +
    REPAIR +
    MORT_AV +
    P_BLACK,
    data = .
  )

summary(reg1)
#plot(reg1)
ggdensity(residuals(reg1))



## remove outlier residuals
df_clean3 <- df_clean2 %>%
  mutate(
    resid = residuals(reg1),
    sd2 = 2*sd(residuals(reg1)),
    outs = ifelse(abs(resid) > sd2, 1, 0)
  ) %>%
  filter(outs == 0) %>%
  print()



## regression 2 --> removed residual outliers
reg_fb <- df_clean3 %>%
  lm(
    log(P_FB) ~
      STATE +
      OCC_CLASS +
      HOLC_GRADE +
      REPAIR +
      MORT_AV +
      P_BLACK,
    data = .
  )

summary(reg_fb)
#plot(reg_fb)
ggdensity(residuals(reg_fb))


## predict missing vars with regression results
est_data <- df_clean1 %>%
  filter(P_FB > 0 | is.na(P_FB))

estimate <- predict(reg_fb, newdata = est_data, se.fit = TRUE, interval = "confidence", level = 0.9)  # 90% CI


## rest of data for rejoining
df_rest <- df_clean1 %>%
  filter(!UNIQUE_ID %in% est_data$UNIQUE_ID) %>%
  print()


## create predicted and join
df_predict <- bind_cols(est_data, as.matrix(estimate)[1], estimate[2]) %>%
  dplyr::rename(se = ncol(.)) %>%
  # transform logged variables
  mutate(
    fit = exp(fit),
    lwr = exp(lwr),
    upr = exp(upr),
    FB_ME = upr - fit
  ) %>%
  print()
  

## rejoin with original data 
df_fix <- df_predict %>%
  bind_rows(df_rest) %>%
  # fix FB estimate
  mutate(
    FB_FLAG = ifelse(is.na(P_FB), 1, FB_FLAG),
    FB_ME = ifelse(is.na(P_FB), FB_ME, NA),
    P_FB = ifelse(is.na(P_FB), fit, P_FB)
    ) %>%
  dplyr::select(-c(fit:se)) %>%
  print()


##------------------------------------------------------------------
##  Income
##------------------------------------------------------------------


df_clean <- df_fix %>%
  drop_na(MID_INC)


## regression
reg1 <- lm(
  log(MID_INC) ~
    REGION +
    OCC_CLASS +
    HOLC_GRADE +
    REPAIR +
    P_FB +
    P_BLACK,
  data = df_fix
)

# summary
summary(reg1)
#plot(reg1)
ggdensity(residuals(reg1))


## remove outlier residuals
df_clean1 <- df_clean %>%
  mutate(
    resid = residuals(reg1),
    sd2 = 2*sd(residuals(reg1)),
    outs = ifelse(abs(resid) > sd2, 1, 0)
  ) %>%
  filter(outs == 0) %>%
  print()


## remove residual outliers
reg_inc <- lm(
  log(MID_INC) ~
    REGION +
    OCC_CLASS +
    HOLC_GRADE +
    REPAIR +
    P_FB +
    P_BLACK,
  data = df_clean1
)

summary(reg_inc)
#plot(reg_inc)
ggdensity(residuals(reg_inc))


## predict missing vars with regression results
est_data <- df_fix %>%
  filter(is.na(MID_INC)) %>%
  filter(HOLC_GRADE != "E") %>%
  print()

estimate <- predict(reg_inc, newdata = est_data, se.fit = TRUE, interval = "confidence", level = 0.9)  # 90% CI


## rest of data for rejoining
df_rest <- df_fix %>%
  filter(!UNIQUE_ID %in% est_data$UNIQUE_ID) %>%
  print()


## create predicted and join
df_predict <- bind_cols(est_data, as.matrix(estimate)[1], estimate[2]) %>%
  dplyr::rename(se = ncol(.)) %>%
  # transform logged variables
  mutate(
    fit = exp(fit),
    lwr = exp(lwr),
    upr = exp(upr),
    INC_ME = upr - fit
  ) %>%
  print()


## rejoin with original data 
df_fix1 <- df_predict %>%
  bind_rows(df_rest) %>%
  # fix FB estimate
  mutate(
    INC_FLAG = ifelse(is.na(MID_INC), 1, 0),
    INC_ME = ifelse(is.na(MID_INC), INC_ME, NA),
    MID_INC = ifelse(is.na(MID_INC), fit, MID_INC)
  ) %>%
  dplyr::select(-c(fit:se)) %>%
  print()



##---------------------------------------------------------
##  Building Age
##---------------------------------------------------------

df_clean <- df_fix1 %>%
  drop_na(MID_AGE)

## regression
reg1 <- lm(
  log(MID_AGE) ~
    STATE +
    REPAIR +
    HOLC_GRADE +
    MID_INC +
    P_FB +
    P_BLACK,
  data = df_clean
)

# summary
summary(reg1)
#plot(reg1)
ggdensity(residuals(reg1))


## remove outlier residuals and NAs from Inc.
df_clean1 <- df_clean %>%
  mutate(
    resid = residuals(reg1),
    sd2 = 2*sd(residuals(reg1)),
    outs = ifelse(abs(resid) > sd2, 1, 0)
  ) %>%
  filter(outs == 0) %>%
  print()


## remove residual outliers
reg_age <- lm(
  log(MID_AGE) ~
    STATE +
    OCC_CLASS +
    HOLC_GRADE +
    REPAIR +
    P_FB +
    P_BLACK,
  data = df_clean1
)

summary(reg_age)
#plot(reg2)
ggdensity(residuals(reg_age))


## predict missing vars with regression results
est_data <- df_fix1 %>%
  filter(is.na(MID_AGE)) %>%
  filter(HOLC_GRADE != "E") %>%
  print()

estimate <- predict(reg_age, newdata = est_data, se.fit = TRUE, interval = "confidence", level = 0.9)  # 90% CI

## rest of data for rejoining
df_rest <- df_fix1 %>%
  filter(!UNIQUE_ID %in% est_data$UNIQUE_ID) %>%
  print()


## create predicted and join
df_predict <- bind_cols(est_data, as.matrix(estimate)[1], estimate[2]) %>%
  dplyr::rename(se = ncol(.)) %>%
  # transform logged variables
  mutate(
    fit = exp(fit),
    lwr = exp(lwr),
    upr = exp(upr),
    AGE_ME = upr - fit
  ) %>%
  print()


## rejoin with original data 
df_fix2 <- df_predict %>%
  bind_rows(df_rest) %>%
  # fix FB estimate
  mutate(
    AGE_FLAG = ifelse(is.na(MID_AGE), 1, 0),
    AGE_ME = ifelse(is.na(MID_AGE), AGE_ME, NA),
    MID_AGE = ifelse(is.na(MID_AGE), fit, MID_AGE)
  ) %>%
  dplyr::select(-c(fit:se)) %>%
  dplyr::select(UNIQUE_ID:FB_FLAG, INC_FLAG, AGE_FLAG, FB_ME, INC_ME, AGE_ME) %>%
  # fix names
  mutate(
    OCC_CLASS = str_replace(OCC_CLASS, "_OC", ""),
    REPAIR = str_replace(REPAIR, "_R", ""),
    MORT_AV = str_replace(MORT_AV, "_AV", "")
  ) %>%
  print()


## Save!!
##------------------------------

write_csv(df_fix2, "DATA_DOWNLOAD/TABLES/ADS_FINAL.csv")



##------------------------------------------------------
## Residual Plots -- density & histogram
##------------------------------------------------------

## set text size
strip_text <- 18
axis_title <- 16
axis_text <- 14

## Foreign born
plot_fb <- ols_plot_resid_hist(reg_fb) + 
  ggtitle("") +
  ylab("Density") +
  xlab("") +
  scale_y_continuous(labels = scales::comma, breaks = seq(0, 1200, 200)) +
  #scale_x_continuous(breaks = seq(-3, 3, 1)) +
  xlim(-2.5, 2.5) +
  theme_bw() +
  facet_grid(. ~ "% \"Foreign Born\"") +
  theme(
    strip.text.x = element_text(size = strip_text, face = "bold"),
    axis.title = element_text(size = axis_title),
    axis.text = element_text(size = axis_text)
  )


plot_fb


## density plot of residuals
plot_inc <- ols_plot_resid_hist(reg_inc) +
  ggtitle("") +
  ylab("Density") +
  xlab("") +
  scale_y_continuous(labels = scales::comma, breaks = seq(0, 4000, 1000)) +
  #scale_x_continuous(breaks = seq(-3, 3, 0.5)) +
  xlim(-2.5, 2.5)+
  theme_bw() +
  facet_grid(. ~ "Family Income") +
  theme(
    strip.text.x = element_text(size = strip_text, face = "bold"),
    axis.title = element_text(size = axis_title),
    axis.text = element_text(size = axis_text)
  )


plot_inc


## Income
plot_age <- ols_plot_resid_hist(reg_age) + 
  ggtitle("") +
  ylab("Density") +
  xlab("Residuals") +
  scale_y_continuous(labels = scales::comma, breaks = seq(0, 6000, 1000)) +
  #scale_x_continuous(breaks = seq(-3, 3, 1), ) +
  xlim(-2.5, 2.5)+
  theme_bw() +
  facet_grid(. ~ "Building Age") +
  theme(
    strip.text.x = element_text(size = strip_text, face = "bold"),
    axis.title = element_text(size = axis_title),
    axis.text = element_text(size = axis_text)
    )

plot_age


## Cowplot
##-------------------------------------
prow <- cowplot::plot_grid(
  plot_fb,
  plot_inc,
  plot_age,
  labels = "auto",
  label_size = 20,
  ncol = 1
)

prow


## Save plots
##----------------------------------------

## create folder
dir.create("DATA_DOWNLOAD/APPENDIX")


## Save out plots
tiff("DATA_DOWNLOAD/APPENDIX/Residuals.tif", width = 550, height = 1100)
prow
dev.off()



###################################################################################
##  Generate Summary Stat Tables
###################################################################################

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
  dplyr::summarize(
    bdg_age_mid = mean(mid_age, na.rm = TRUE),
    mid_inc = mean(mid_inc, na.rm = TRUE),
    black = mean(blk_num, na.rm = TRUE),
    fb = mean(fb_num, na.rm = TRUE),
    inc_miss = base::sum(inc_miss),
    fb_miss = base::sum(fb_miss),
    nhoods = dplyr::n()
  ) %>% 
  filter(holc_grade != "E") %>%
  dplyr::select(holc_grade, region, bdg_age_mid:fb_miss, cities, nhoods) %>%
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
  dplyr::summarize(holc_nhoods = dplyr::n()) %>%
  print()


## Midwest
mw <- df_cities %>% filter(region == "MW") %>%
  dplyr::select(-region) %>%
  arrange(-holc_nhoods, metro) %>%
  dplyr::rename(
    City = city_state,
    Metro = metro,
    "HOLC Neighborhoods" = holc_nhoods
  ) %>%
  print()

## Northeast
ne <- df_cities %>% filter(region == "NE") %>%
  dplyr::select(-region) %>%
  arrange(-holc_nhoods, metro) %>%
  dplyr::rename(
    City = city_state,
    Metro = metro,
    "HOLC Neighborhoods" = holc_nhoods
  ) %>%
  print()

## South
s <- df_cities %>% filter(region == "S") %>%
  dplyr::select(-region) %>%
  arrange(-holc_nhoods, metro) %>%
  dplyr::rename(
    City = city_state,
    Metro = metro,
    "HOLC Neighborhoods" = holc_nhoods
  ) %>%
  print()

## West
w <- df_cities %>% filter(region == "W") %>%
  dplyr::select(-region) %>%
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




