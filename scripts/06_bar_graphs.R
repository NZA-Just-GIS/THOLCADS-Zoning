
#########################################################################
#########################################################################
###                                                                   ###
###                         MAKE BAR GRAPHS                           ###
###                                                                   ###
#########################################################################
#########################################################################

## PREPARE WORKSPACE
source("scripts/00_preamble.R")


##############################################
##  LOAD Bar Graph DATA
##############################################

df_org <- read.xlsx("DATA_DOWNLOAD/TABLES/Sum_Stats.xlsx", sheet = 1) %>%
  # rename columns
  rename_all(funs(str_replace_all(., "\\.", " "))) %>%
  # fix remaining issue
  rename("Missing For. Born" = "Missing For  Born") %>%
  print()


##################################################################
##  Make Bar Graph for Bldg Age, % Black, and % FB
##################################################################

## Building Age
bldg_age <- df_org %>%
  ggplot(aes(x = Region, y = `Building Age Midpoint`)) +
  geom_bar(aes(fill = `HOLC Grade`), stat = "identity", width = 0.8, position = position_dodge(0.9)) +
  scale_fill_manual(values = c("#4daf4a", "#377eb8", "#F1C40F", "#C0392B")) +
  labs(fill = "HOLC\nGrade") +
  theme_light() +
  theme(
    axis.title = element_text(size = 13.5),
    axis.text = element_text(size = 11),
    legend.title = element_text(size = 13.5),
    legend.text = element_text(size = 11)
  ) +
  ylim(0, 40)


bldg_age


## Percent Black
pblk <- df_org %>%
  ggplot(aes(x = Region, y = `Black (%)`)) +
  geom_bar(aes(fill = `HOLC Grade`), stat = "identity", width = 0.8, position = position_dodge(0.9)) +
  scale_fill_manual(values = c("#4daf4a", "#377eb8", "#F1C40F", "#C0392B")) +
  labs(fill = "HOLC\nGrade") +
  theme_light() +
  theme(
    axis.title = element_text(size = 13.5),
    axis.text = element_text(size = 11),
    legend.title = element_text(size = 13.5),
    legend.text = element_text(size = 11)
  ) +
  ylim(0, 50)

pblk


## Percent For. Born
pfb <- df_org %>%
  ggplot(aes(x = Region, y = `Foreign Born (%)`)) +
  geom_bar(aes(fill = `HOLC Grade`), stat = "identity", width = 0.8, position = position_dodge(0.9)) +
  scale_fill_manual(values = c("#4daf4a", "#377eb8", "#F1C40F", "#C0392B")) +
  labs(fill = "HOLC\nGrade") +
  ylab("\"Foreign Born\" (%)") + 
  theme_light() +
  theme(
    axis.title = element_text(size = 13.5),
    axis.text = element_text(size = 11),
    legend.title = element_text(size = 13.5),
    legend.text = element_text(size = 11)
  ) +
  ylim(0, 50)

pfb


##-----------------------------------
## SAVE FIGURES as TIFFs
##-----------------------------------

## Create new folder and export
dir.create("DATA_DOWNLOAD/BAR_GRAPHS")


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
  
  filename = paste0("DATA_DOWNLOAD/BAR_GRAPHS/", name, ".tif")
  tiff(filename, width = 420, height = 365)
  print(graph_list[i])
  dev.off()
  
}


##############################################################
##  Make Bar Graph for % FB Nationality Breakdowns
##############################################################

## clean up
rm(list = setdiff(ls(), "packages"))

## load ADS_FINAL
df <- read_csv("DATA_DOWNLOAD/TABLES/ADS_FINAL.csv") %>%
  print()

##----------------------------
## create table
##----------------------------

df1 <- df %>% 
  # remove one strange case
  filter(HOLC_GRADE != "E" & !is.na(HOLC_ID)) %>%
  # select for mentions of specific nationality/ethnicity groupings
  mutate(
    `E. Asian/PI` = ifelse(str_detect(FB_TEXT, regex("orient|chine|jap|filip", ignore_case = T)), 1, 0),
    `"Latin"` = ifelse(str_detect(FB_TEXT, regex("latin|mex|cuba|puerto", ignore_case = T)), 1, 0),
    Italian = ifelse(str_detect(FB_TEXT, regex("italian", ignore_case = T)), 1, 0),
    Nordic = ifelse(str_detect(FB_TEXT, regex("nord|norw|swed|finn|danish|dane", ignore_case = T)), 1, 0),
    German = ifelse(str_detect(FB_TEXT, regex("german", ignore_case = T)), 1, 0),
    Polish = ifelse(str_detect(FB_TEXT, regex("polish|pole", ignore_case = T)), 1, 0),
    Irish = ifelse(str_detect(FB_TEXT, regex("irish", ignore_case = T)), 1, 0),
    Jewish = ifelse(str_detect(FB_TEXT, regex("jew", ignore_case = T)), 1, 0),
    `E. European` = ifelse(str_detect(FB_TEXT, regex("slav|slay|hungar|russian|eastern euro|czec|bohem|yugo|slove|slova|croat|lithu", ignore_case = T)), 1, 0),
    Greek = ifelse(str_detect(FB_TEXT, regex("greek", ignore_case = T)), 1, 0),
    British = ifelse(str_detect(FB_TEXT, regex("brit|english|welsh|scot", ignore_case = T)), 1, 0),
    Black = ifelse(P_BLACK > 0, 1, 0),
    American = ifelse(str_detect(FB_TEXT, regex("american", ignore_case = T)) & 
                         !str_detect(FB_TEXT, regex("mex|latin|jew|german|native", ignore_case = T)), 1, 0),
    None = ifelse(P_FOR_BORN == 0 & P_BLACK == 0, 1, 0),
    Total = 1
  ) %>%
  # organize by HOLC grade and Region
  group_by(HOLC_GRADE, REGION) %>%
  summarize(
    `E. Asian/PI` = sum(`E. Asian/PI`),
    `"Latin"` = sum(`"Latin"`),
    Italian = sum(Italian),
    Nordic = sum(Nordic),
    German = sum(German),
    Polish = sum(Polish),
    Irish = sum(Irish),
    Jewish = sum(Jewish),
    `E. European` = sum(`E. European`),
    Greek = sum(Greek),
    British = sum(British),
    Black = sum(Black),
    American = sum(American),
    None = sum(None),
    Total = sum(Total),
    n = n()
  ) %>%
  # make data long
  pivot_longer(
    cols = `E. Asian/PI`:Total,
    names_to = "nationality",
    values_to = "nhoods"
  ) %>%
  # calculate by nationality
  group_by(nationality) %>%
  mutate(nat_sum = sum(nhoods)) %>%
  # organize into correct format & calculate
  group_by(nationality, HOLC_GRADE, nat_sum) %>%
  summarize(p_nat_sum = sum(nhoods) / nat_sum * 100) %>%
  arrange(nationality, HOLC_GRADE) %>%
  distinct() %>%
  print()


##---------------------------------
## Make graph
##---------------------------------

## prep data frame
df_graph <- df1 %>%
  # reorder factor
  mutate(
     nationality = 
       factor(
         nationality,
         levels = 
           c(
             "E. Asian/PI", "Black", "\"Latin\"", "Greek", 
             "Italian",  "Polish", "E. European", "Irish", 
             "Jewish", "Nordic", "German", "British", "American",
             "None", "Total"
             )
       )
     ) %>%
  print()


## Set up graph
g1 <- df_graph %>%
  ggplot(aes(x = nationality, y = p_nat_sum, fill = HOLC_GRADE)) +
  # set position to "stack"
  geom_bar(position = "stack", stat = "identity") +
  # assign colors
  scale_fill_manual(values = c("#4daf4a", "#377eb8", "#F1C40F", "#C0392B")) +
  # set limits for y axis
  scale_y_continuous(limits = c(0, 101), expand = c(0, 0)) +
  # change labels
  labs(
    x = "Population Group",
    y = "Neighborhoods by Grade (%)",
    fill = "HOLC\nGrade"
  ) +
  # set theme
  theme_light() +
  # adjust text
  theme(
    axis.title = element_text(size = 13.5),
    axis.text = element_text(size = 11),
    axis.text.x = element_text(angle = 45, vjust = 1.15, hjust = 1.1),
    legend.title = element_text(size = 13.5),
    legend.text = element_text(size = 11)
  )
  

g1  # display graph


##-----------------------------------------------------
## Second graph -- order by # of neighborhoods
##-----------------------------------------------------

## Look at N values
df_order <- df_graph %>% 
  ungroup() %>%
  select(nationality, nat_sum) %>%
  distinct() %>%
  arrange(-nat_sum) %>%
  rename(group = 1, n = nat_sum) %>%
  print()


## prep data frame
df_graph2 <- df1 %>%
  filter(!nationality %in% c("None", "Total")) %>%
  mutate(Neighborhoods = p_nat_sum / 100 * nat_sum) %>%
  # reorder factor
  mutate(
   nationality = 
     factor(
       nationality,
       levels = unique(df1$nationality[order(df1$nat_sum, decreasing = TRUE)])
     )
  ) %>%
  print()
 

 
##-----------------------------------------------------
## Second graph -- order by # of neighborhoods
##-----------------------------------------------------
g2 <- df_graph2 %>%
  ggplot(aes(x = nationality, y = Neighborhoods, fill = HOLC_GRADE)) +
  # set position to "stack"
  geom_bar(position = "stack", stat = "identity") +
  # assign colors
  scale_fill_manual(values = c("#4daf4a", "#377eb8", "#F1C40F", "#C0392B")) +
  # set limits for y axis
  scale_y_continuous(limits = c(0, 1250), expand = c(0, 0), breaks = seq(0, 1200, 250)) +
  # change labels
  labs(
   x = "Population Group",
   y = "Neighborhoods",
   fill = "HOLC\nGrade"
  ) +
  # set theme
  theme_light() +
  # adjust text
  theme(
   axis.title = element_text(size = 13.5),
   axis.text = element_text(size = 11),
   axis.text.x = element_text(angle = 45, vjust = 1.15, hjust = 1.1),
   legend.title = element_text(size = 13.5),
   legend.text = element_text(size = 11)
  )
 
 
g2  # display graph


##-----------------------------------------------------
## Third graph -- Break down N'hoods by Black %
##-----------------------------------------------------

## % Black graph
blk_graph <- df %>% 
  filter(HOLC_GRADE != "E" & !is.na(HOLC_ID)) %>% 
  mutate(
   black = 
     case_when(
       P_BLACK == 0 ~ "0.0", 
       P_BLACK > 0 & P_BLACK < 2 ~ "0.1 to 1.9", 
       P_BLACK >= 2 & P_BLACK < 5 ~ "2.0 to 4.9", 
       P_BLACK >= 5 & P_BLACK < 10 ~ "5.0 to 9.9", 
       P_BLACK >= 10 & P_BLACK < 20 ~ "10 to 19.9", 
       P_BLACK >= 20 & P_BLACK < 50 ~ "20 to 49.9", 
       TRUE ~ "50 and more"
     )
  ) %>% 
  #set factor levels
  mutate(
   black = 
    factor(
      black, 
      levels = c("0.0", "0.1 to 1.9", "2.0 to 4.9", "5.0 to 9.9", "10 to 19.9", "20 to 49.9", "50 and more"))
  ) %>%
  # Collect variables
  group_by(black, HOLC_GRADE) %>% 
  summarize(n = n()) %>%
  ungroup() %>%
  # calc. percentages
  group_by(black) %>%
  mutate(p_total = n / sum(n) * 100) %>%
  print()


## Set up graph
g3 <- blk_graph %>%
  ggplot(aes(x = black, y = p_total, fill = HOLC_GRADE)) +
  # set position to "stack"
  geom_bar(position = "stack", stat = "identity") +
  # assign colors
  scale_fill_manual(values = c("#4daf4a", "#377eb8", "#F1C40F", "#C0392B")) +
  # set limits for y axis
  scale_y_continuous(limits = c(0, 101), expand = c(0, 0)) +
  # change labels
  labs(
    x = "% Black",
    y = "Neighborhoods (%)",
    fill = "HOLC\nGrade"
  ) +
  # set theme
  theme_light() +
  # adjust text
  theme(
    axis.title = element_text(size = 13.5),
    axis.text = element_text(size = 11),
    axis.text.x = element_text(angle = 45, vjust = 1.15, hjust = 1.1),
    legend.title = element_text(size = 13.5),
    legend.text = element_text(size = 11)
  )


g3  # display graph


## Get summary of breakdown (N)
blk_graph %>%
  group_by(black) %>%
  summarize(n = sum(n)) %>%
  print()


##------------------------------------
##  SAVE OUT
##------------------------------------

## Save out as TIFFs
graph_list <- list(g1, g2, g3)
for(i in 1:length(graph_list)){
  
  if(i == 1){
    name <- "pct_group"
  } else if(i == 2){
    name <- "num_group"
  } else{
    name <- "pblk_ranges"
  }
  
  filename = paste0("DATA_DOWNLOAD/BAR_GRAPHS/", name, ".tif")
  tiff(filename, width = 600, height = 411)
  print(graph_list[i])
  dev.off()
  
}

