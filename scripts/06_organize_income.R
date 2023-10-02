
#########################################################################
#########################################################################
###                                                                   ###
###                         "Family" Income                           ###
###                                                                   ###
#########################################################################
#########################################################################


## PREPARE WORKSPACE
source("scripts/00_preamble.R")


## Grab data
ads37 <- NULL
for(i in seq(1:3)){
  
  temp <- openxlsx::read.xlsx(
    "DATA_DOWNLOAD/TABLES/ADS_organized.xlsx",
    sheet = i
  )
  
  # occupation (occupation)
  if(i == 3){
    
    ads3940 <- temp %>%
      dplyr::select(unique_id, ads_type, occupation) %>%
      dplyr::rename(fam_inc = ncol(.)) %>%
      as_tibble()
    
  } else{
    # family income (fam_inc)
    temp1 <- temp %>%
      dplyr::select(unique_id, ads_type, fam_inc) %>%
      as_tibble()
    
    ads37 <- bind_rows(ads37, temp1)
    
  }
  
}

## Inspect
ads37 
ads3940


##############################################################
## Correct ads37
##############################################################

fix1 <- ads37 %>%
  mutate(
    inc_txt = fam_inc,
    fam_inc = str_replace_all(fam_inc, "\\$", ""),
    fam_inc = str_replace_all(fam_inc, ",", ""),
    fam_inc = str_replace_all(fam_inc, "\\.5 M", "500"),
    fam_inc = str_replace_all(fam_inc, "\\.4 M", "400"),
    fam_inc = str_replace_all(fam_inc, "\\.6 M", "600"),
    fam_inc = str_replace_all(fam_inc, "\\.5M", "500"),
    fam_inc = str_replace_all(fam_inc, " M", "000"),
    fam_inc = str_replace_all(fam_inc, "M", "000"),
    fam_inc = str_replace_all(fam_inc, "\\.00$", ""),
    fam_inc = ifelse(str_detect(fam_inc, "\\.[:digit:]000"), str_replace(fam_inc, "0$", ""), fam_inc), # get rid of extra "0" on decimals
    fam_inc = str_replace_all(fam_inc, "\\.", ""),
    fam_inc = str_replace_all(fam_inc, "\\s*\\([^\\)]+\\)", ""),  # remove parentheses & their contents
    # fix dashes
    fam_inc = str_replace_all(fam_inc, "^1-", "1000 "),
    fam_inc = str_replace_all(fam_inc, "^2-|^2 -|^2 to", "2000 "),
    fam_inc = str_replace_all(fam_inc, " 2-", " 2000 "),
    fam_inc = str_replace_all(fam_inc, "^12", "1200 "),
    fam_inc = str_replace_all(fam_inc, "^1 1/2", "1500 "),
    fam_inc = str_replace_all(fam_inc, "^1 1/2", "1500 "),
    fam_inc = str_replace_all(fam_inc, "^3-|^3 -", "3000 "),
    fam_inc = str_replace_all(fam_inc, "^4-", "4000 "),
    fam_inc = str_replace_all(fam_inc, "24-", "2400 "),
    fam_inc = str_replace_all(fam_inc, "^5-", "5000 "),
    fam_inc = str_replace_all(fam_inc, "^15-", "1500 "),
    fam_inc = str_replace_all(fam_inc, "^25-", "2500 "),
    fam_inc = str_replace_all(fam_inc, "^6-", "6000 "),
    fam_inc = str_replace_all(fam_inc, "^18-", "1800 "),
    fam_inc = str_replace_all(fam_inc, "3$", "3000"),
    fam_inc = str_replace_all(fam_inc, " 5$", "5000"),
    fam_inc = str_replace_all(fam_inc, "15$", "1500"),
    fam_inc = str_replace_all(fam_inc, "75$", "7500"),
    fam_inc = str_replace_all(fam_inc, "6$", "6000"),
    fam_inc = ifelse(unique_id %in% c("MI_Pontiac_A3", "GA_Atlanta_A1", "SC_Columbia_A2"), str_replace(fam_inc, "10", "10000"), fam_inc),
    fam_inc = str_replace_all(fam_inc, "-", " "),
    fam_inc = str_replace_all(fam_inc, "005000", "00 5000"),
    fam_inc = str_replace_all(fam_inc, "003000", "00 3000"),
    ) %>%
  dplyr::rename(var = fam_inc) %>%
  # add numbers to var_num
  mutate(
    var = str_replace(var, regex("new", ignore_case = TRUE), "0"),  # covert "new" to zero
    var_num1 = str_extract(var, "[:digit:]+"),
    var_num2 = strex::str_after_nth(var, "[:digit:]+", 1),
    var_num2 = str_extract(var_num2, "[:digit:]+"),
    var_num3 = strex::str_after_nth(var, "[:digit:]+", 2),
    var_num3 = str_extract(var_num3, "[:digit:]+"),
    var_num4 = strex::str_after_nth(var, "[:digit:]+", 3),
    var_num4 = str_extract(var_num4, "[:digit:]+"),
    var_num5 = strex::str_after_nth(var, "[:digit:]+", 4),
    var_num5 = str_extract(var_num5, "[:digit:]+"),
    var_join = ifelse(is.na(var_num1), toupper(str_extract(var, "^[:alpha:]+")), NA)
  ) %>%
  # manual override for special cases
  mutate(
    var_num1 = ifelse(unique_id %in% c("OR_Portland_C7", "GA_Atlanta_D16"), 1000, var_num1),
    var_num1 = ifelse(unique_id %in% c("OH_Portsmouth_D1", "OH_Portsmouth_D2"), 900, var_num1),
    var_num2 = ifelse(unique_id %in% c("OH_Portsmouth_D1", "OH_Portsmouth_D2"), 1200, var_num2),
    var_num5 = ifelse(unique_id %in% c("AL_Birmingham_A1", "AL_Birmingham_A2"), 100000, var_num3)
  ) %>%
  arrange(unique_id) %>%
  print()


##-------------------------------
## Fine tune
##-------------------------------

ads37_fixed <- fix1 %>%
  # make numeric
  mutate_at(vars(var_num1:var_num5), ~as.numeric(.)) %>%
  # fix selected cases
  mutate(
    var_num1 = ifelse(var_num1 < 100, var_num1 * 100, var_num1),
    var_num2 = ifelse(unique_id == "MA_Brockton_C002", 1200, var_num2),
    var_num2 = ifelse(unique_id == "FL_Miami_A009", 50000, var_num2),
    var_num1 = ifelse(var_num2 == 50, var_num1 + var_num2, var_num1),
    var_num1 = ifelse(var == "1200 000 & up", 12000, var_num1),
    var_num2 = ifelse(var_num2 < 100, NA, var_num2),
    var_num3 = ifelse(var_num3 < 100, NA, var_num3),
    var_num1 = ifelse(var_num1 == 3020, 300, var_num1),
    var_num2 = ifelse(
      unique_id %in% c("FL_Miami_C18", "OH_Toledo_A4", "OH_Toledo_A11", "WI_MilwaukeeCo_A1"), 
      var_num2 * 10, var_num2
      ),
    var_num1 = ifelse(
      unique_id %in% c("CA_SanJose_B1", "CO_Denver_B7","IL_Rockford_A6", "NY_Queens_B9", "OH_Youngstown_D2"), 
      var_num1 / 10, var_num1
      ),
    var_num2 = ifelse(unique_id == "IN_TerreHaute_B1", 3000, var_num2),
    var_num1 = ifelse(unique_id == "NY_NiagaraFalls_B2", 1500, var_num1),
    var_num3 = ifelse(unique_id == "FL_Miami_C19", var_num3 * 10, var_num3),
    var_num3 = ifelse(unique_id == "OH_Toledo_C21", 1800, var_num3),
    var_num1 = ifelse(
      is.na(var_num1) & 
        is.na(var_num2) &
        is.na(var_num3) &
        is.na(var_num4) &
        is.na(var_num5),
      as.numeric(str_extract(var, "[:digit:]+")),
      var_num1
      )
    ) %>%
  # make long
  pivot_longer(
      cols = var_num1:var_num5,
      names_to = "var_nums",
      values_to = "value"
    ) %>%
  # correct "and up" cases
  mutate(
    and_up = ifelse(str_detect(var, " up") & !str_detect(var, "up to"), 1.5, 1)  # multiplier for "and up"
  ) %>%
  group_by(unique_id, and_up) %>%
  dplyr::summarize(
     min = base::min(value, na.rm = TRUE),
     max = base::max(value, na.rm = TRUE) * and_up,  # multiple
     mid = (min + max) / 2,
     mid = ifelse(is.nan(mid), NA, mid)
  ) %>%
  mutate_at(vars(min:mid), ~ifelse(is.infinite(.), 0, .)) %>%
  mutate_at(vars(min, max), ~ifelse(min == 0 & max == 0, NA, .)) %>%
  ungroup() %>%
  dplyr::select(-and_up) %>%
  distinct() %>%
  print()



##############################################################
## Correct ads3940
##############################################################

## Inspect
fix1 <- ads3940 %>%
  mutate(
    inc_txt = fam_inc,
    fam_inc = str_replace_all(fam_inc, "[a-zA-LN-Z]+", ""),
    fam_inc = str_replace_all(fam_inc, "\\$|&", " "),
    fam_inc = str_replace_all(fam_inc, ",", ""),
    fam_inc = str_replace_all(fam_inc, "-", " "),
    fam_inc = str_replace_all(fam_inc, "\\d+%", ""),
    var_num1 = str_extract(fam_inc, "[:digit:]+"),
    var_num2 = strex::str_after_nth(fam_inc, "[:digit:]+", 1),
    var_num2 = str_extract(var_num2, "[:digit:]+"),
    var_num3 = strex::str_after_nth(fam_inc, "[:digit:]+", 2),
    var_num3 = str_extract(var_num3, "[:digit:]+"),
    var_num4 = strex::str_after_nth(fam_inc, "[:digit:]+", 3),
    var_num4 = str_extract(var_num4, "[:digit:]+"),
    monthly = ifelse(str_detect(inc_txt, "month|mo\\."), 12, 1),  # multiplier for monthly salary (some Chicago n'hoods)
    and_up = ifelse(str_detect(inc_txt, " up") & !str_detect(inc_txt, "up to"), 1.5, 1)  # multiplier for "and up"
  ) %>%
  # make numeric
  mutate_at(vars(var_num1:var_num4), ~as.numeric(.) * monthly) %>%
  # fix select mistakes
  mutate(
    var_num1 = 
      ifelse(unique_id %in% c("CA_LosAngeles_D4", "CA_LosAngeles_D38"), var_num1 / 10, var_num1),
    var_num1 =
      ifelse(unique_id %in% c("MI_Detroit_D20"), 700, var_num1),
    var_num1 =
      ifelse(unique_id %in% c("CA_LosAngeles_C164"), var_num1 * 10, var_num1),
    var_num2 =
      ifelse(unique_id %in% c("CA_LosAngeles_A1", "CA_LosAngeles_A28"), var_num2 * 10, var_num2),
    var_num2 =
      ifelse(unique_id %in% c("CA_LosAngeles_D69"), var_num2 + 1000, var_num2),
    var_num2 =
      ifelse(unique_id %in% c("CA_LosAngeles_B85"), as.numeric(str_sub(var_num1, 5, 8)), var_num2),
    var_num1 =
      ifelse(unique_id %in% c("CA_LosAngeles_B85"), as.numeric(str_sub(var_num1, 1, 4)), var_num1)
  ) %>%
  # remove cases with # < 100 (mistakes)
  mutate_at(vars(var_num1:var_num4), ~ifelse(. < 100, NA, .)) %>%
  print()


## Get final output
ads3940_fixed <- fix1 %>%
  pivot_longer(
    cols = var_num1:var_num4,
    names_to = "var_nums",
    values_to = "value"
  ) %>%
  group_by(unique_id, and_up) %>%
  dplyr::summarize(
    min = base::min(value, na.rm = TRUE),
    max = base::max(value, na.rm = TRUE) * and_up,  # multiple
    mid = (min + max) / 2,
    mid = ifelse(is.nan(mid), NA, mid)
  ) %>%
  mutate_at(vars(min:mid), ~ifelse(is.infinite(.), 0, .)) %>%
  mutate_at(vars(min, max), ~ifelse(min == 0 & max == 0, NA, .)) %>%
  ungroup() %>%
  dplyr::select(-and_up) %>%
  distinct() %>%
  print()



##############################################################
## Combine
##############################################################

## bind text
ads_text <- bind_rows(ads37, ads3940) %>%
  dplyr::select(-ads_type) %>%
  print()


# make final
ads_income <- bind_rows(ads37_fixed, ads3940_fixed) %>%
  dplyr::select(unique_id, min, mid, max) %>%
  dplyr::rename(
    min_inc = min,
    mid_inc = mid,
    max_inc = max
  ) %>%
  # add text
  left_join(ads_text, by = "unique_id") %>%
  dplyr::rename(inc_occ_txt = fam_inc) %>%
  # fix cases of under 200
  mutate(mid_inc = ifelse(mid_inc < 200, NA, mid_inc)) %>%
  print()


## Save out
write_csv(ads_income, "DATA_DOWNLOAD/ADS_Income.csv")

