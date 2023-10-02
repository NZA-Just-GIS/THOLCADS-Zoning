
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
  rename_all(~str_replace_all(., "\\.", " ")) %>%
  # fix remaining issue
  dplyr::rename("Missing For. Born" = "Missing For  Born") %>%
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
  theme_bw() +
  theme(
    axis.title = element_text(size = 12, face = "bold"),
    axis.text = element_text(size = 10),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10),
    legend.key.width = unit(1.1, "cm"),
    legend.key.height = unit(0.5, "cm"),
    legend.position = "bottom"
  ) +
  ylim(0, 40) +
  ylab("Bldg. Age Midpoint (years)") +
  guides(fill = guide_legend(label.position = "bottom"))

bldg_age


## Family Income
fam_inc <- df_org %>%
  mutate(`Family Income Midpoint` = `Family Income Midpoint` / 1000) %>%
  ggplot(aes(x = Region, y = `Family Income Midpoint`)) +
  geom_bar(
    aes(fill = `HOLC Grade`),
    stat = "identity", 
    width = 0.8, 
    position = position_dodge(0.9)
    ) +
  scale_fill_manual(values = c("#4daf4a", "#377eb8", "#F1C40F", "#C0392B")) +
  labs(fill = "HOLC\nGrade") +
  theme_bw() +
  theme(
    axis.title = element_text(size = 12, face = "bold"),
    axis.text = element_text(size = 10),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10),
    legend.key.width = unit(1.1, "cm"),
    legend.key.height = unit(0.5, "cm"),
    legend.position = "bottom"
  ) +
  ylab("Family Income Midpoint (000s)") +
  scale_y_continuous(
    labels = scales::dollar_format(),
    limits = c(0, 15),
    breaks = seq(0, 15, 5)
    )+
  guides(fill = guide_legend(label.position = "bottom"))

fam_inc


## Percent Black
pblk <- df_org %>%
  ggplot(aes(x = Region, y = `Black (%)`)) +
  geom_bar(aes(fill = `HOLC Grade`), stat = "identity", width = 0.8, position = position_dodge(0.9)) +
  scale_fill_manual(values = c("#4daf4a", "#377eb8", "#F1C40F", "#C0392B")) +
  labs(fill = "HOLC\nGrade") +
  theme_bw() +
  theme(
    axis.title = element_text(size = 12, face = "bold"),
    axis.text = element_text(size = 10),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10),
    legend.key.width = unit(1.1, "cm"),
    legend.key.height = unit(0.5, "cm"),
    legend.position = "bottom"
  ) +
  ylim(0, 50)+
  guides(fill = guide_legend(label.position = "bottom"))

pblk


## Percent For. Born
pfb <- df_org %>%
  ggplot(aes(x = Region, y = `Foreign Born (%)`)) +
  geom_bar(aes(fill = `HOLC Grade`), stat = "identity", width = 0.8, position = position_dodge(0.9)) +
  scale_fill_manual(values = c("#4daf4a", "#377eb8", "#F1C40F", "#C0392B")) +
  labs(fill = "HOLC\nGrade") +
  ylab("\"Foreign Born\" (%)") +
  theme_bw() +
  theme(
    axis.title = element_text(size = 12, face = "bold"),
    axis.text = element_text(size = 10),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10),
    legend.key.width = unit(1.1, "cm"),
    legend.key.height = unit(0.5, "cm"),
    legend.position = "bottom"
  ) +
  ylim(0, 50) +
  guides(fill = guide_legend(label.position = "bottom"))

pfb


##-----------------------------------
## SAVE FIGURES as TIFFs
##-----------------------------------

## Create new folder and export
dir.create("DATA_DOWNLOAD/BAR_GRAPHS")


## Save out as TIFFs
# graph_list <- list(bldg_age, fam_inc, pblk, pfb)
# for(i in 1:length(graph_list)){
#   
#   if(i == 1){
#     name <- "bldg_age"
#   } else if(i == 2){
#     name <- "fam_inc"
#   } else if(i == 3){
#     name <- "pblk"
#   } else{
#     name <- "pfb"
#   }
#   
#   filename = paste0("DATA_DOWNLOAD/BAR_GRAPHS/", name, ".tif")
#   tiff(filename, width = 420, height = 365)
#   print(graph_list[i])
#   dev.off()
#   
# }


##---------------------------------------
## Combine for single, 2x2 figure
##---------------------------------------

#packages(egg)
packages(ggpubr)  # for arranging plots


## create 2x2 plot
prow2 <- ggarrange(
  pblk + 
    facet_wrap(~"Black") +
    theme(
      strip.text = element_text(size = 13, face = "bold", color = "black"),
      axis.title.x = element_blank(),
      legend.position = "none"
    ),
  pfb +
    facet_wrap(~"\"Foreign Born\" ") +
    theme(
      strip.text = element_text(size = 13, face = "bold", color = "black"),
      axis.title.x = element_blank(),
      legend.position = "none"
    ),
  fam_inc +
    facet_wrap(~"Family Income") +
    theme(
      strip.text = element_text(size = 13, face = "bold", color = "black"),
      legend.position = "none"
    ),
  bldg_age +
    facet_wrap(~"Building Age") +
    theme(
      strip.text = element_text(size = 13, face = "bold", color = "black"),
      legend.position = "none"
    ),
  nrow = 2,
  ncol = 2,
  common.legend = TRUE,
  legend = "bottom",
  align = "v"
)

prow2


## save out
tiff("DATA_DOWNLOAD/BAR_GRAPHS/fourplots.tif", width = 800, height = 600)
print(prow2)
dev.off()


png("DATA_DOWNLOAD/BAR_GRAPHS/fourplots.png", width = 800, height = 600)
print(prow2)
dev.off()


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
    `E. Asian/PI` = ifelse(str_detect(FB_GROUP, regex("orient|chine|jap|filip", ignore_case = T)), 1, 0),
    `"Latin"` = ifelse(str_detect(FB_GROUP, regex("latin|mex|cuba|puerto", ignore_case = T)), 1, 0),
    Italian = ifelse(str_detect(FB_GROUP, regex("italian", ignore_case = T)), 1, 0),
    Nordic = ifelse(str_detect(FB_GROUP, regex("nord|norw|swed|finn|danish|dane", ignore_case = T)), 1, 0),
    German = ifelse(str_detect(FB_GROUP, regex("german", ignore_case = T)), 1, 0),
    Polish = ifelse(str_detect(FB_GROUP, regex("polish|pole", ignore_case = T)), 1, 0),
    Irish = ifelse(str_detect(FB_GROUP, regex("irish", ignore_case = T)), 1, 0),
    Jewish = ifelse(str_detect(FB_GROUP, regex("jew", ignore_case = T)), 1, 0),
    `E. European` = ifelse(str_detect(FB_GROUP, regex("slav|slay|hungar|russian|eastern euro|czec|bohem|yugo|slove|slova|croat|lithu", ignore_case = T)), 1, 0),
    Greek = ifelse(str_detect(FB_GROUP, regex("greek", ignore_case = T)), 1, 0),
    British = ifelse(str_detect(FB_GROUP, regex("brit|english|welsh|scot", ignore_case = T)), 1, 0),
    Black = ifelse(P_BLACK > 0, 1, 0),
    American = ifelse(str_detect(FB_GROUP, regex("american", ignore_case = T)) & 
                         !str_detect(FB_GROUP, regex("mex|latin|jew|german|native", ignore_case = T)), 1, 0),
    None = ifelse(P_FB == 0 & P_BLACK == 0, 1, 0),
    Total = 1
  ) %>%
  # organize by HOLC grade and Region and sum counts
  group_by(HOLC_GRADE, REGION) %>%
  summarize_at(
     vars(`E. Asian/PI`:Total),
     ~base::sum(., na.rm = TRUE)
  ) %>%
  # make data long
  pivot_longer(
    cols = `E. Asian/PI`:Total,
    names_to = "nationality",
    values_to = "nhoods"
  ) %>%
  # calculate by nationality
  group_by(nationality) %>%
  mutate(nat_sum = base::sum(nhoods)) %>%
  # organize into correct format & calculate
  group_by(nationality, HOLC_GRADE, nat_sum) %>%
  dplyr::summarize(p_nat_sum = base::sum(nhoods) / nat_sum * 100) %>%
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
             "Jewish", "German","Nordic", "British", "American",
             "None", "Total"
             )
       )
     ) %>%
  print()


## Check factor order
df_graph %>%
  filter(HOLC_GRADE == "D") %>%
  arrange(-p_nat_sum)


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
  theme_bw() +
  # adjust text
  theme(
    axis.title = element_text(size = 15.5, face = "bold"),
    axis.text = element_text(size = 13.5),
    axis.text.x = element_text(angle = 45, vjust = 1.15, hjust = 1.1),
    legend.title = element_text(size = 15.5),
    legend.text = element_text(size = 13.5)
  )
  

g1  # display graph


## Save out
tiff("DATA_DOWNLOAD/BAR_GRAPHS/fb_graph.tif", width = 825, height = 600)
print(g1)
dev.off()


png("DATA_DOWNLOAD/BAR_GRAPHS/fb_graph.png", width = 825, height = 600)
print(g1)
dev.off()



##-----------------------------------------------------
## Second graph -- order by # of neighborhoods
##-----------------------------------------------------

## Look at N values
df_order <- df_graph %>% 
  ungroup() %>%
  dplyr::select(nationality, nat_sum) %>%
  distinct() %>%
  arrange(-nat_sum) %>%
  dplyr::rename(group = 1, n = nat_sum) %>%
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
  theme_bw() +
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
  dplyr::summarize(n = dplyr::n()) %>%
  ungroup() %>%
  # calc. percentages
  group_by(black) %>%
  mutate(p_total = n / base::sum(n) * 100) %>%
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
  theme_bw() +
  # adjust text
  theme(
    axis.title = element_text(size = 13.5),
    axis.text = element_text(size = 11.5),
    axis.text.x = element_text(angle = 45, vjust = 1.15, hjust = 1.1),
    legend.title = element_text(size = 13.5),
    legend.text = element_text(size = 11.5)
  )


g3  # display graph


## Get summary of breakdown (N)
blk_graph %>%
  group_by(black) %>%
  dplyr::summarize(n = base::sum(n)) %>%
  print()

png("DATA_DOWNLOAD/BAR_GRAPHS/blk_graph.png", width = 750, height = 500)
print(g3)
dev.off()



##--------------------------------------------------------------
## Fourth graph -- Break down N'hoods by Occupation Class
##--------------------------------------------------------------

## Prep data
occ_graph <- df %>% 
  filter(HOLC_GRADE != "E" & !is.na(HOLC_ID)) %>% 
  # remove Other/NA
  #filter(OCC_CLASS != "Other_NA") %>%
  group_by(HOLC_GRADE, OCC_CLASS) %>%
  dplyr::summarize(n = dplyr::n()) %>%
  ungroup() %>%
  # calc. percentages
  group_by(HOLC_GRADE) %>%
  mutate(p_total = n / base::sum(n) * 100) %>%
  mutate(
    OCC_CLASS =
      factor(
        OCC_CLASS,
        level = c("Upper", "Up_Mid", "Mid_Mix", "Low_Mid", "Lower", "Other_NA")
      )
  ) %>%
  arrange(HOLC_GRADE, -p_total) %>%
  print()


## Set up graph
g_occ <- occ_graph %>%
  ggplot(aes(x = HOLC_GRADE, y = p_total, fill = OCC_CLASS)) +
  # set position to "stack"
  geom_bar(position = "stack", stat = "identity") +
  # assign colors
  # scale_fill_brewer(
  #   labels = c("\"Upper\"", "Upper\nMiddle", "Middle/\nMixed", "Lower\nMiddle", "\"Lower\"", "Other/NA"),
  #   palette = "BrBG", 
  #   direction = -1
  #   ) +
  scale_fill_manual(
    labels = c("\"Upper\"", "Up-\nMid.", "Mid./\nMix", "Low-\nMid.", "\"Lower\"", "Other/\nNA"),
    values = c("#018571", "#80cdc1", "#f5f5f5", "#dfc27d", "#a6611a", "grey70")
  ) +
  # set limits for y axis
  scale_y_continuous(limits = c(0, 101), expand = c(0, 0)) +
  # change labels
  labs(
    x = "HOLC Grade",
    y = "Neighborhoods (%)",
    fill = "Occupation\nClass"
  ) +
  # set theme
  theme_bw() +
  # adjust text
  # adjust text
  theme(
    axis.title = element_text(size = 13),
    axis.text = element_text(size = 11.5),
    legend.title = element_blank(),
    legend.text = element_text(size = 10.5),
    legend.key.width = unit(1.3, "cm"),
    legend.key.height = unit(0.5, "cm"),
    legend.position = "bottom"
  ) +
  guides(fill = guide_legend(label.position = "bottom", nrow = 1, byrow = TRUE))

g_occ


##--------------------------------------------------------------
## Fifth graph -- Repair
##--------------------------------------------------------------

## Prep data
repair_graph <- df %>% 
  filter(HOLC_GRADE != "E" & !is.na(HOLC_ID)) %>% 
  # remove Other/NA
  #filter(REPAIR != "Other_NA") %>%
  group_by(HOLC_GRADE, REPAIR) %>%
  dplyr::summarize(n = dplyr::n()) %>%
  ungroup() %>%
  # calc. percentages
  group_by(HOLC_GRADE) %>%
  mutate(p_total = n / base::sum(n) * 100) %>%
  mutate(
    REPAIR =
      factor(
        REPAIR,
        level = c("Good", "Fair-Good", "Fair", "Fair-Poor", "Poor", "Other_NA")
      )
  ) %>%
  arrange(HOLC_GRADE, -p_total) %>%
  print()


## Set up graph
g_rep <- repair_graph %>%
  ggplot(aes(x = HOLC_GRADE, y = p_total, fill = REPAIR)) +
  # set position to "stack"
  geom_bar(position = "stack", stat = "identity") +
  # assign colors
  # scale_fill_brewer(
  #   labels = c("Good", "Fair-\nGood", "Fair", "Fair-\nPoor", "Poor"),
  #   palette = "BrBG", 
  #   direction = -1
  # ) +
  scale_fill_manual(
    labels = c("Good", "Fair-\nGood", "Fair", "Fair-\nPoor", "Poor", "Other/\nNA"),
    values = c("#018571", "#80cdc1", "#f5f5f5", "#dfc27d", "#a6611a", "grey70")
  ) +
  # set limits for y axis
  scale_y_continuous(limits = c(0, 101), expand = c(0, 0)) +
  # change labels
  labs(
    x = "HOLC Grade",
    y = "Neighborhoods (%)",
    fill = "Housing\nCondition"
  ) +
  # set theme
  theme_bw() +
  # adjust text
  theme(
    axis.title = element_text(size = 12.5),
    axis.text = element_text(size = 11.5),
    legend.title = element_blank(),
    legend.text = element_text(size = 10.5),
    legend.key.width = unit(1.3, "cm"),
    legend.key.height = unit(0.5, "cm"),
    legend.position = "bottom"
  ) +
  guides(fill = guide_legend(label.position = "bottom", nrow = 1, byrow = TRUE))


g_rep


##--------------------------------------------------------------
## Sixth graph -- Mortgage Availability
##--------------------------------------------------------------

## Prep data
mort_graph <- df %>% 
  filter(HOLC_GRADE != "E" & !is.na(HOLC_ID)) %>% 
  # remove Other/NA
  #filter(MORT_AV != "Other_NA") %>%
  group_by(HOLC_GRADE, MORT_AV) %>%
  dplyr::summarize(n = dplyr::n()) %>%
  ungroup() %>%
  # calc. percentages
  group_by(HOLC_GRADE) %>%
  mutate(p_total = n / base::sum(n) * 100) %>%
  mutate(
    MORT_AV =
      factor(
        MORT_AV,
        level = c("Good", "Fair-Good", "Fair", "Fair-Poor", "Poor", "Other_NA")
      )
  ) %>%
  arrange(HOLC_GRADE, -p_total) %>%
  print()


## Set up graph
g_mort <- mort_graph %>%
  ggplot(aes(x = HOLC_GRADE, y = p_total, fill = MORT_AV)) +
  # set position to "stack"
  geom_bar(position = "stack", stat = "identity") +
  # assign colors
  # scale_fill_brewer(
  #   labels = c("Ample", "Limited-\nAmple", "Limited/\nConditional", "Limited-\nRestricted", "Restricted", "Other/NA"),
  #   palette = "BrBG", 
  #   direction = -1
  # ) +
  scale_fill_manual(
    labels = c("Ample", "Ltd.-\nAmple", "Ltd./\nCond.", "Ltd.-\nRestr.", "Restr.", "Other/\nNA"),
    values = c("#018571", "#80cdc1", "#f5f5f5", "#dfc27d", "#a6611a", "grey70")
  ) +
  # set limits for y axis
  scale_y_continuous(limits = c(0, 101), expand = c(0, 0)) +
  # change labels
  labs(
    x = "HOLC Grade",
    y = "Neighborhoods (%)",
    fill = "Mortgage\nAvailability"
  ) +
  # set theme
  theme_bw() +
  # adjust text
  theme(
    axis.title = element_text(size = 12.5),
    axis.text = element_text(size = 11.5),
    legend.title = element_blank(),
    legend.text = element_text(size = 10.5),
    legend.key.width = unit(1.3, "cm"),
    legend.key.height = unit(0.5, "cm"),
    legend.position = "bottom"
  ) +
  guides(
    fill = guide_legend(
      label.position = "bottom",
      nrow = 1, 
      byrow = TRUE
      )
    )


g_mort



##------------------------------------
##  Create single figure
##------------------------------------

qual_charts <- ggarrange(
  g_rep +
    facet_wrap(~"Home Repair Status") +
    theme(
      strip.text = element_text(size = 13, face = "bold", color = "black"),
      axis.title.y = element_text(size = 12.5, face = "bold"),
      axis.title.x = element_blank()
    ),
  g_mort +
    facet_wrap(~"Mortgage Availability") +
    theme(
      strip.text = element_text(size = 13, face = "bold", color = "black"),
      axis.title.y = element_blank(),
      axis.title.x = element_text(size = 12.5, face = "bold")
    ),
  g_occ +
    facet_wrap(~"Occupation Class") +
    theme(
      strip.text = element_text(size = 13, face = "bold", color = "black"),
      axis.title.y = element_blank(),
      axis.title.x = element_blank()
    ),
  nrow = 1,
  ncol = 3,
  align = "h"
)

qual_charts


##------------------------------------
##  SAVE OUT
##------------------------------------

## Save out as a TIFF
tiff("DATA_DOWNLOAD/BAR_GRAPHS/qual_charts.tif", width = 900, height = 450)
print(qual_charts)
dev.off()


## Save out as a PNG
png("DATA_DOWNLOAD/BAR_GRAPHS/qual_charts.png", width = 900, height = 450)
print(qual_charts)
dev.off()


## Save out as individual TIFFs
# graph_list <- list(g1, g2, g3, g4, g5, g6)
# for(i in 1:length(graph_list)){
#   
#   if(i == 1){
#     name <- "pct_group"
#   } else if(i == 2){
#     name <- "num_group"
#   } else if(i == 3){
#     name <- "pblk_ranges"
#   } else if(i == 4){
#     name <- "occ_class"
#   } else if(i == 5){
#     name <- "repair"
#   } else{
#     name <- "mortgage_av"
#   }
#   
#   filename = paste0("DATA_DOWNLOAD/BAR_GRAPHS/", name, ".tif")
#   tiff(filename, width = 600, height = 411)
#   print(graph_list[i])
#   dev.off()
#   
# }

