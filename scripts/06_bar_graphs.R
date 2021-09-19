
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


##############################################
##  Make Bar Graph
##############################################

## Building Age
bldg_age <- df_org %>%
  ggplot(aes(x = Region, y = `Building Age Midpoint`)) +
  geom_bar(aes(fill = `HOLC Grade`), stat = "identity", width = 0.8, position = position_dodge(0.9)) +
  scale_fill_manual(values = c("#4daf4a", "#377eb8", "#F1C40F", "#C0392B")) +
  theme_light() +
  theme(
    axis.title = element_text(size = 13.5),
    axis.text = element_text(size = 11),
    legend.title = element_text(size = 13.5),
    legend.text = element_text(size = 11)
  ) +
  ylim(0, 50)


bldg_age


## Percent Black
pblk <- df_org %>%
  ggplot(aes(x = Region, y = `Black (%)`)) +
  geom_bar(aes(fill = `HOLC Grade`), stat = "identity", width = 0.8, position = position_dodge(0.9)) +
  scale_fill_manual(values = c("#4daf4a", "#377eb8", "#F1C40F", "#C0392B")) +
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




