
#########################################################################
#########################################################################
###                                                                   ###
###           Organize HU Repair & Mortgage Availability              ###
###                                                                   ###
#########################################################################
#########################################################################


## PREPARE WORKSPACE
source("scripts/00_preamble.R")


## Grab data
ads <- NULL
for(i in seq(1:3)){
  
  temp <- openxlsx::read.xlsx(
    "DATA_DOWNLOAD/TABLES/ADS_organized.xlsx",
    sheet = i
  )
  
  ## organize vars: repair, sales demand (eventually dropped), and mortgage availability for buyers
  temp1 <- temp %>%
      select(state:ads_type, repair, mort_av_buy) %>%
      as_tibble()

  ## bind together
  ads <- bind_rows(ads, temp1)
  
}

ads  # inspect


##############################################################
## Correct special cases in Atlantic City, Akron
##############################################################

## Load Chicago fix data
others_fix <- read_csv("tables/others_fix.csv") %>%
  print()


## Join
ads2 <- ads %>%
  left_join(others_fix, by = "unique_id", suffix = c("", "_fix")) %>%
  mutate(
    repair = ifelse(!is.na(repair_fix), repair_fix, repair),
    mort_av_buy = ifelse(!is.na(mort_av_buy_fix), mort_av_buy_fix, mort_av_buy)
  ) %>%
  select(-c(repair_fix:ncol(.))) %>%
  print()


##############################################################
## Fix Vars
##############################################################

ads_fix <- ads2 %>%
  # Repair fix
  mutate(
    repair_vgood = ifelse(str_detect(repair, regex("very good|excellent|ecellent|excelllent|first class|absolutely new|^new ", ignore_case = T)), 1, 0),
    repair_good = ifelse(str_detect(repair, regex("good|giood|goood|pride of ownership|well kept", ignore_case = T)) & !str_detect(repair, regex("very", ignore_case = T)), 1, 0),
    repair_fair = ifelse(str_detect(repair, regex("fair|fine|fari|all varieties|new, but cheap|houses settle|medium|^spotty$|^varied$|all conditions|all kinds of", ignore_case = T)), 1, 0),
    repair_poor = ifelse(str_detect(repair, regex("poor|bad", ignore_case = T)) & !str_detect(repair, regex("very", ignore_case = T)), 1, 0),
    repair_vpoor = ifelse(str_detect(repair, regex("very poor|very bad|terrible", ignore_case = T)), 1, 0),
    repair_dilap = ifelse(str_detect(repair, regex("dilapidated|rotten", ignore_case = T)), 1, 0),
    repair_need_rep = ifelse(str_detect(repair, regex("repair", ignore_case = T)), 1, 0),
    repair_na = ifelse(is.na(repair) | repair %in% c("N/A N\\/A N\\/A", "N/A N\\/A", "N\\/A"), 1, 0)
  ) %>%
  # Remaining
  mutate(repair_oth = ifelse(rowSums(.[str_detect(names(.), "repair_")], na.rm = T) < 1, 1, 0)) %>%
  # Mortgage availability
  mutate(
    mort_ample = ifelse(
      str_detect(
        mort_av_buy, 
        regex("ample|^7-1938|best|favorable|xcellent|amle|amoke|ampel|aple|amplo|plentiful|good|^yes|smple|plenty|^available$|^\\*available$", ignore_case = T)),
      1, 0),
    mort_ample = ifelse(str_detect(mort_av_buy, regex("maximum", ignore_case = T)) & metro == "Cleveland", 1, mort_ample),
    mort_lim = ifelse(
      str_detect(mort_av_buy, regex("limited|limit|available only|available to|available up to|available for|select|fair|ltd|little|some|conservative|limted|questionable|small|not easy|slow|only available|available only|available up to", ignore_case = T)) &
        !str_detect(mort_av_buy, regex("very", ignore_case = T)), 
      1, 0),
    mort_vlim = ifelse(str_detect(mort_av_buy, regex("very limited|very ltd|very/ limited|very llimited|Very limiited|very little|scarce|nominal|doubtful|hard to get|poor|meager|difficult|restricted", ignore_case = T)), 1, 0),
    mort_none = ifelse(str_detect(mort_av_buy, regex("none|not available|^no$|nil|^0$|not available|none available", ignore_case = T)), 1, 0),
    mort_per = ifelse(str_detect(mort_av_buy, "25%|30%|35%|40%|45%|50%|55%|60%|65%|70%|75%|80%|40 percent|^50$|\\$16,000|\\$20,000"), 1, 0),
    mort_na = ifelse(
      is.na(mort_av_buy) |
        mort_av_buy %in% c("N/A", "-", "*"), 
      1, 0),
    mort_fha = ifelse(str_detect(mort_av_buy, regex("fha|fea", ignore_case = T)), 1, 0)
    ) %>%
  # capture conditional
  mutate(
    mort_per = ifelse(mort_per == 1 | 
                        str_detect(mort_av_buy, 
                                   regex("^where sewer|^low ratio|^singles$|^land value|^upon personal credit|5\\.5%$|^B&LA 6%$|	B&L- 66 2/3 at 6%|^only for|^B&L 6%$", ignore_case = T)), 
                      1, 0),
    mort_per = ifelse(mort_per == 1 | mort_av_buy %in% c("B&L- 66 2/3 at 6%", "December 30, 1899"), 1, 0)
  ) %>%
  # remaining
  mutate(mort_oth = ifelse(rowSums(.[str_detect(names(.), "mort_") & !str_detect(names(.), "mort_av")], na.rm = T) < 1, 1, 0)) %>%
  print()

## Inspect
#View(ads_fix)


##############################################################
## Clean 
##############################################################


##--------------------------------------
## Make binary variables
##--------------------------------------

ads_fix1 <- ads_fix %>%
  mutate(
    repair_good = ifelse(repair_vgood == 1 | repair_good == 1, 1, 0),
    repair_poor = ifelse(repair_poor == 1 | repair_vpoor == 1 | repair_dilap == 1 | repair_need_rep == 1, 1, 0),
    mort_good = mort_ample,
    mort_fair = ifelse(mort_lim == 1 | mort_per == 1, 1, 0),
    mort_poor = ifelse(mort_vlim == 1 | mort_none == 1, 1, 0)
  ) %>%
  select(
    state:ads_type, 
    repair_good, repair_fair, repair_poor, repair_na, repair_oth, repair,
    mort_good, mort_fair, mort_poor, mort_fha, mort_na, mort_oth, mort_av_buy
    ) %>%
  # change NAs to zeros
  mutate_at(
    vars(
      contains("good"), 
      contains("fair"),
      contains("poor"),
      contains("oth"),
      contains("fha")
      ),
    ~ifelse(is.na(.), 0, .)
    ) %>%
  print()



##--------------------------------------
## Create single categories
##--------------------------------------

ads_fix2 <- ads_fix1 %>%
  mutate(
    repair_cat =
      case_when(
        repair_good == 1 & repair_fair == 0 & repair_poor == 0 ~ "Good",
        repair_good == 1 & repair_fair == 1 & repair_poor == 0 ~ "Fair-Good",
        repair_good == 0 & repair_fair == 0 & repair_poor == 1 ~ "Poor",
        repair_good == 0 & repair_fair == 1 & repair_poor == 1 ~ "Fair-Poor",
        repair_na == 1 | repair_oth == 1 ~ "Other_NA",
        TRUE ~ "Fair"
      ),
    mort_cat =
      case_when(
        mort_good == 1 & mort_fair == 0 & mort_poor == 0 ~ "Good",
        mort_good == 1 & mort_fair == 1 & mort_poor == 0 ~ "Fair-Good",
        mort_good == 0 & mort_fair == 0 & mort_poor == 1 ~ "Poor",
        mort_good == 0 & mort_fair == 1 & mort_poor == 1 ~ "Fair-Poor",
        mort_na == 1 | mort_oth == 1 ~ "Other_NA",
        TRUE ~ "Fair"
      ),
  ) %>%
  # organize
  select(
    state:ads_type,
    repair_cat, repair,
    mort_cat, mort_fha, mort_av_buy
  ) %>%
  # fix fair-good mort_av
  mutate(
    mort_cat = ifelse(
      mort_cat == "Fair-Good" & str_detect(mort_av_buy, regex("available up to", ignore_case = T)),
      "Fair", mort_cat
    )
  ) %>%
  # add factors
  mutate(
    repair_cat = factor(
      repair_cat, 
      levels = c("Good", "Fair-Good", "Fair", "Fair-Poor", "Poor", "Other_NA")
      ),
    mort_cat = factor(
      mort_cat, 
      levels = c("Good", "Fair-Good", "Fair", "Fair-Poor", "Poor", "Other_NA")
    )
  ) %>%
  # fix names
  dplyr::rename(
    repair_txt = repair,
    repair = repair_cat,
    mort_txt = mort_av_buy,
    mort_av = mort_cat
  ) %>%
  # clean (remove sales demand)
  select(unique_id, repair, mort_av, mort_fha, repair_txt, mort_txt) %>%
  print()



## Save out
write_csv(ads_fix2, "DATA_DOWNLOAD/ADS_Repair_Mortgage.csv")




