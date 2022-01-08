
#########################################################################
#########################################################################
###                                                                   ###
###                     PREPARE BLDG AGE COLUMN                       ###
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

  temp1 <- temp %>%
    select(state:ads_type, avg_age) %>%
    as_tibble()

  ads <- bind_rows(ads, temp1)
  
}

ads  # inspect

## Clean up
rm(temp, temp1)


#################################################################################
## Correct avg age
#################################################################################

## data frame prep
numbers <- read_csv("tables/numbers.csv") %>%
  mutate(alpha_no = toupper(alpha_no)) %>%
  print()


##----------------------------------------
## Prepare
##----------------------------------------

## Establish variable of Interest
var <- "avg_age"

## Extract all other numbers
ads_pre_prep <- ads %>%
  # keep only unique identifier & variable of interest
  select(unique_id, var, region) %>%
  # keep generic name
  dplyr::rename(var = 2) %>%
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
    var_num6 = strex::str_after_nth(var, "[:digit:]+", 5),
    var_num6 = str_extract(var_num6, "[:digit:]+"),
    var_join = ifelse(is.na(var_num1), toupper(str_extract(var, "^[:alpha:]+")), NA)
  ) %>%
  left_join(numbers, by = c("var_join" = "alpha_no")) %>%
  mutate(var_num1 = ifelse(!is.na(var_join), no, var_num1)) %>%
  select(-var_join, -no) %>%
  pivot_longer(
    cols = var_num1:var_num6,
    names_to = "var_nums",
    values_to = "value"
  ) %>%
  mutate(
    up_to = ifelse(str_detect(var, regex("up to", ignore_case = TRUE)), 0.75, 1),  # ID "up to" (n = 173)
    value = as.numeric(value),
    value = value * up_to
    ) %>%
  group_by(unique_id) %>%
  summarize(
    min = min(value, na.rm = TRUE),
    max = max(value, na.rm = TRUE),
    midpt = (min + max) / 2#,
    #avg = mean(value, na.rm = TRUE)
    ) %>%
  mutate(
    min = ifelse(is.infinite(min), NA, min),
    max = ifelse(is.infinite(max), NA, max),
    midpt = ifelse(is.nan(midpt), NA, midpt)#,
    #avg = ifelse(is.nan(avg), NA, avg)
  ) %>%
  print()


## View
#ads_prep %>% filter(is.na(midpt)) %>% View()  # n = 335

## NAs and Inf indicate spaces that went unfilled by appraisers


##-----------------------------------------------
## Correct special cases 
##-----------------------------------------------

## Load Chicago fix data
age_fix <- read_csv("tables/age_fix.csv") %>%
  print()

## Join
ads_prep <- ads_pre_prep %>%
  left_join(age_fix, by = "unique_id", suffix = c("", "_fix")) %>%
  mutate(
    min = ifelse(!is.na(min_fix), min_fix, min),
    max = ifelse(!is.na(max_fix), max_fix, max),
    midpt = ifelse(!is.na(midpt_fix), midpt_fix, midpt)
  ) %>%
  select(-c(min_fix:midpt_fix)) %>%
  print()


##--------------------------------------------------------
##  Join back and rename column
##--------------------------------------------------------

ads_bdg_age <- ads_prep %>%
  dplyr::rename(
    min_age = min,
    max_age = max,
    mid_age = midpt
  ) %>%
  # fix special case
  mutate_at(
    vars(min_age:mid_age),
    ~case_when(
      unique_id %in% c("WA_Spokane_D8") ~ 25,
      unique_id %in% c("CA_LosAngeles_A30") ~ 6,
      TRUE ~ .
    )
    ) %>%
  # add text
  left_join(ads[c("unique_id", "avg_age")], by = "unique_id") %>%
  dplyr::rename(age_txt = avg_age) %>%
  print()


##--------------------------------------------------------
##  Save out!!
##--------------------------------------------------------

write_csv(ads_bdg_age, "DATA_DOWNLOAD/ADS_Building_Age.csv")

