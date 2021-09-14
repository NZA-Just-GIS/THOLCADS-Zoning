
#########################################################################
#########################################################################
###                                                                   ###
###                 RUN BASIC PRELIMINARY ANALYES                     ###
###                                                                   ###
#########################################################################
#########################################################################

## PREPARE WORKSPACE
source("scripts/00_preamble.R")

dir.create("output_prelim")


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
##  Look at data DATA
##############################################

## Look at building age, % black, and % foreign born by region & nhood
df_org <- df %>%
  group_by(holc_grade, region) %>%
  summarize(
    bdg_age_mid = mean(mid_age, na.rm = TRUE),
    bdg_age_avg = mean(avg_age, na.rm = TRUE),
    black = mean(blk_num, na.rm = TRUE),
    fb = mean(fb_num, na.rm = TRUE),
    n = dplyr::n()
  ) %>% 
  filter(holc_grade != "E") %>%
  arrange(region, holc_grade) %>%
  print()


## Cities
df_cities <- df %>%
  mutate(city_state = paste(city, state, sep = ", ")) %>%
  group_by(city_state, metro, region) %>%
  summarize(holc_nhoods = dplyr::n()) %>%
  print()


##---------------------------------
## SAVE OUT
##---------------------------------

write_csv(df_org, "output_prelim/df_org.csv")
write_csv(df_cities, "output_prelim/df_cities.csv")


##------------------------------------------
## Make sheets
##------------------------------------------

mw <- df_cities %>% filter(region == "MW") %>%
  select(-region) %>%
  arrange(-holc_nhoods, metro) %>%
  print()

ne <- df_cities %>% filter(region == "NE") %>%
  select(-region) %>%
  arrange(-holc_nhoods, metro) %>%
  print()

s <- df_cities %>% filter(region == "S") %>%
  select(-region) %>%
  arrange(-holc_nhoods, metro) %>%
  print()

w <- df_cities %>% filter(region == "W") %>%
  select(-region) %>%
  arrange(-holc_nhoods, metro) %>%
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
##  Look at data DATA More
##############################################

## Look at building age, % black, and % foreign born by region & nhood --> weighted by city
df_means_of_means <- df %>%
  group_by(holc_grade, metro) %>%
  mutate(
    bdg_age_mid = mean(mid_age, na.rm = TRUE),
    bdg_age_avg = mean(avg_age, na.rm = TRUE),
    black = mean(blk_num, na.rm = TRUE),
    fb = mean(fb_num, na.rm = TRUE)
  ) %>%
  select(holc_grade, region, bdg_age_mid:fb) %>%
  distinct() %>%
  group_by(holc_grade, region) %>%
  summarize(
    bdg_age_mid = mean(bdg_age_mid, na.rm = TRUE),
    bdg_age_avg = mean(bdg_age_avg, na.rm = TRUE),
    black = mean(black, na.rm = TRUE),
    fb = mean(fb, na.rm = TRUE),
    n = dplyr::n()
  ) %>% 
  filter(holc_grade != "E") %>%
  arrange(region, holc_grade) %>%
  print()

write_csv(df_means_of_means, "output_prelim/df_means_of_means.csv")

##############################################
##  Make bar graph
##############################################

df_bar <- df_org %>%
  select(-bdg_age_mid, -n) %>%
  dplyr::rename(
    avg_age = bdg_age_avg,
    pblk = black,
    pfb = fb
    ) %>%
  # pivot_longer(
  #   cols = avg_age:pfb,
  #   names_to = "variable",
  #   values_to = "value"
  # ) %>%
  print()

## Building Age
bldg_age <- 
  ggplot(df_bar, aes(x = region, y = avg_age)) +
  geom_bar(aes(fill = holc_grade), stat = "identity", width = 0.8, position = position_dodge(0.9)) +
  #scale_fill_manual(values = c("#09C109", "#2155CE", "#E4ED2A", "#DE351D")) +
  scale_fill_manual(values = c("#4daf4a", "#377eb8", "#F1C40F", "#C0392B")) +
  labs(fill = "HOLC Grade") +
  xlab("Region") +
  ylab("Average Building Age") +
  theme_light()

bldg_age

## Percent Black
pblk <- 
  ggplot(df_bar, aes(x = region, y = pblk)) +
  geom_bar(aes(fill = holc_grade), stat = "identity", width = 0.8, position = position_dodge(0.9)) +
  #scale_fill_manual(values = c("#09C109", "#2155CE", "#E4ED2A", "#DE351D")) +
  scale_fill_manual(values = c("#4daf4a", "#377eb8", "#F1C40F", "#C0392B")) +
  labs(fill = "HOLC Grade") +
  xlab("Region") +
  ylab("% Black") +
  theme_light()

pblk

## Percent For. Born
pfb <- 
  ggplot(df_bar, aes(x = region, y = pfb)) +
  geom_bar(aes(fill = holc_grade), stat = "identity", width = 0.8, position = position_dodge(0.9)) +
  #scale_fill_manual(values = c("#09C109", "#2155CE", "#E4ED2A", "#DE351D")) +
  scale_fill_manual(values = c("#4daf4a", "#377eb8", "#F1C40F", "#C0392B")) +
  labs(fill = "HOLC Grade") +
  xlab("Region") +
  ylab("% Foreign Born") +
  theme_light()


pfb


## Create new folder and export
dir.create("figures_prelim")

## Save out as PNGs
graph_list <- list(bldg_age, pblk, pfb)
for(i in 1:length(graph_list)){
  
  if(i == 1){
    name <- "bldg_age"
  } else if(i == 2){
    name <- "pblk"
  } else{
    name <- "pfb"
  }
  
  filename = paste0("figures_prelim/", name, ".tif")
  tiff(filename, width = 420, height = 365)
  print(graph_list[i])
  dev.off()
  
}


##############################################
##  Regression
##############################################

reg_data <- df %>%
  drop_na(mid_age, avg_age, blk_num, fb_num) %>%
  filter(holc_grade %in% c("B", "C")) %>%
  mutate(
    holc_grade = ifelse(holc_grade == "C", 1, 0)
    ) %>%
  #mutate_at(vars(avg_age, fb_num), funs(scale(., scale = FALSE))) %>%
  print()

reg1 <- glm(
  holc_grade ~
    region*blk_num +
    region*fb_num +
    region*avg_age,
  family = binomial(link = "logit"),
  data = reg_data
)

summary(reg1)


##############################################
##  Correlations
##############################################

cor_data <- df %>%
  drop_na(mid_age, avg_age, blk_num, fb_num) %>%
  print()



