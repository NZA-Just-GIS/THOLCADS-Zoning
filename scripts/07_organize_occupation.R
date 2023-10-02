
#########################################################################
#########################################################################
###                                                                   ###
###                             Occupation                            ###
###                                                                   ###
#########################################################################
#########################################################################


## PREPARE WORKSPACE
source("scripts/00_preamble.R")


##############################################################
## Get occupation 
##############################################################

## Grab data
occ <- NULL
for(i in seq(1,3)){
  
  temp <- openxlsx::read.xlsx(
    "DATA_DOWNLOAD/TABLES/ADS_organized.xlsx",
    sheet = i
  )
  
  # occupation (occupation)
  if(i == 1){
    
    temp1 <- temp %>%
      dplyr::select(unique_id, ads_type, inh_type) %>%
      dplyr::rename(occ = inh_type) %>%
      as_tibble()
    
  } else{
    # occupation
    temp1 <- temp %>%
      dplyr::select(unique_id, ads_type, occupation) %>%
      dplyr::rename(occ = occupation) %>%
      as_tibble()
    
  }
  
  occ <- bind_rows(occ, temp1)
  
}

occ


##----------------------------------
##  organize
##----------------------------------

occ1 <- occ %>%
  mutate(
    higher = 
      ifelse(
        str_detect(occ, 
                   regex("profess|upper|department head|bus\\. men|Millionares|
                         railway officials|white-colar|white celar|white cellar|
                         executiv|commercial|white-collar|white collar|exec\\.|
                         large income|Busienss|upper class|better people|wealth|
                         ^best$|^good$|bext|Officers of plants|well-to-do|prosperous|
                         upper income|better fam|upper bracket|execs|business|Manufacturers|
                         wealthy|manager|capitalist|high income|best people|higher income|high class", 
                         ignore_case = T)
                   ),
        1, 0
      ),
    middle = 
      ifelse(
        str_detect(
          occ, 
          regex("cleric|grocer|merchant|middle|rooming house|state empl|better paid emp|superint|
                good middle-class|^middle-class|hospital work|teacher|salaried|clerk|skilled|keeper|
                artisan|service|office|salaried work", 
                ignore_case = T)
        ),
        1, 0
      ),
    lower = 
      ifelse(
        str_detect(
          occ, 
          regex("domestic|agricult|unskilled|quarry|cheap lab|cannery|oil well|oil refinery|
                servant|workingmen|firemen|truck driv|gardener|steel work|packing plant|marble work|
                shop men|Wage-earner|shopmen|packing house|stock yard|Railraod shop|employees of monsant|
                Ind. Employ|Labroers|smelt|factory|flat work|ordinary work|farm|shoe work|wpa|shoework|
                laboeing|poor|^workers$|mill work|miner|shipyard|shipbuild|low wage|working men|homesteader|
                trades|rubber work|bums|the worst|parasite|cheaper class|rural res|squatters|drifters|wage earner|
                industrial |common|daily work|chicken rancher|hands|low class|low-class|low grade|low-grade|
                working people|working class|lower", 
                ignore_case = T)
        ),
        1, 0
      ),
    mixed = 
      ifelse(
        str_detect(
          occ, 
          regex("Miscellaneous|every class|various|heterogeneous|varied|mixture|
                every type|all type|mixed type|all classes|all changes", 
                ignore_case = T)
        ),
        1, 0
      ),
    lower = 
      ifelse(
        str_detect(
          occ, 
          regex("RR work|RR emp|rr shop|rr train|Traimen|R\\.R\\.|R\\. R\\.|railroad men|railroad work|
                
                railroad emp|railroad shop|railroad train|shop workers|navy yard", 
                ignore_case = T)
        ),
        1, lower
      ),
    middle = 
      ifelse(
        str_detect(occ, regex("^skilled| skilled", ignore_case = T)), 
        1, middle
      ),
    lower = 
      ifelse(
        str_detect(occ, regex("mechanic|mechs|labor", ignore_case = T)) &
          !str_detect(occ, regex("^skilled mechanic| skilled mechanic| skilled lab|^skilled lab", ignore_case = T)),
        1, lower
      ),
    lower = 
      ifelse(str_detect(occ, regex("relief", ignore_case = T)) &
               !str_detect(occ, regex("none on relief", ignore_case = T)),
             1, lower),
    mixed = 
      ifelse(str_detect(occ, regex("mixed", ignore_case = TRUE)) &
               !str_detect(unique_id, "Indianapolis|Richmond"),
             1, mixed)
  ) %>%
  mutate(
    other = ifelse(higher == 0 & middle == 0 & lower == 0 & mixed == 0, 1, 0)
  ) %>%
  print()


##-------------------------------------------
## Organize into single column "class"
##-------------------------------------------

occ2 <- occ1 %>%
  mutate(
    class =
      ifelse(higher == 1 & middle == 0 & lower == 0 & mixed == 0, "bus_prof", "Other_NA"),
    class =
      ifelse(higher == 1 & middle == 1 & lower == 0, "upper_mid", class),
    class = 
      ifelse(higher == 0 & middle == 1 & lower == 0, "middle", class),
    class = 
      ifelse(higher == 0 & middle == 1 & lower == 1, "lower_mid", class),
    class =
      ifelse(higher == 0 & middle == 0 & lower == 1 & mixed == 0, "working_poor", class),
    class =
      ifelse((higher == 1 & lower == 1 & other == 0) &
               (!class %in% c("bus_prof", "upper_mid", "middle", "lower_mid", "working_poor")), "mixed", class),
    class =
      ifelse(mixed == 1 &
               !class %in% c("upper", "upper_mid", "middle", "lower_mid", "lower"), "mixed", class),
    class =
      ifelse(is.na(class), "other_NA", class),
    # fix indiv. cases
    class =
      ifelse(unique_id == "MA_Haverhill_A1", "bus_prof", class),
    class = 
      ifelse(unique_id %in% c("IL_Aurora_C5", "WA_Tacoma_D6", "MI_Detroit_D24", "MI_Detroit_D31"), "working_poor", class),  # Detroit based on income quantiles
    class = 
      ifelse(unique_id == "CA_LosAngeles_C4", "upper_mid", class),
    class = 
      ifelse(unique_id == "IL_Chicago_C57", "middle", class),
    class =
      ifelse(unique_id == "MI_Detroit_D45", "lower_mid", class)  # based on income quantiles for Detroit
  ) %>%
  # make middle-mixed combined class
  mutate(
    class =
      ifelse(class == "middle" | class == "mixed", "middle_mixed", class)
  ) %>%
  # rename
  mutate(
    class =
      case_when(
        class == "bus_prof" ~ "Upper",
        class == "upper_mid" ~ "Up_Mid",
        class == "middle_mixed" ~ "Mid_Mix",
        class == "lower_mid" ~ "Low_Mid",
        class == "working_poor" ~ "Lower",
        TRUE ~ "Other_NA"
      )
  ) %>%
  print()


## Inspect
occ2 %>% 
  group_by(class) %>%
  count


## finalize
ads_occ <- occ2 %>%
  dplyr::select(unique_id, class, occ) %>%
  dplyr::rename(
    occ_class = class,
    occ_txt = occ
    ) %>%
  print()


## Save out
write_csv(ads_occ, "DATA_DOWNLOAD/ADS_Occupation.csv")

