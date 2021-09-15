
#########################################################################
#########################################################################
###                                                                   ###
###                      PREPARE % BLACK COLUMN                       ###
###                                                                   ###
#########################################################################
#########################################################################

## PREPARE WORKSPACE
source("scripts/00_preamble.R")


## Grab data
ads <- NULL
for(i in seq(1:3)){
  
  temp <- openxlsx::read.xlsx(
    "DATA_DOWNLOAD/ADS_organized.xlsx",
    sheet = i
  )
  
  temp1 <- temp %>%
    select(state:ads_type, for_born, black, infilt, avg_age, mort_av_buy) %>%
    as_tibble()
  
  ads <- bind_rows(ads, temp1)
  
}

ads  # inspect

## Clean up
rm(temp, temp1)


#################################################################################
## Correct Foreign Born estimates
#################################################################################

##----------------------------------------
## Prepare
##----------------------------------------

## Establish variable of Interest
var <- "black"

## Isolate fractions and convert them to decimals
fractions <- ads %>%
  # keep only unique identifier & variable of interest
  select(unique_id, var) %>%
  # keep generic name
  dplyr::rename(var = 2) %>%
  # keep only descriptions w/ fractions (n = 6)
  filter(str_detect(var, "1/[0-9]+")) %>%
  mutate(
    flag = str_detect(var, "[0-9] [0-9]/[0-9]+"),  # flag cases with a whole number in front (e.g., 2 1/2)
    var_num = str_extract(var, "[0-9]/[0-9]+"),  # grab fractions
    whole = ifelse(flag == TRUE, as.integer(str_extract(var, "[0-9]+")), 0)  # grab whole numbers (if any)
  ) %>% 
  # separate numerator from denominator
  separate(var_num, c("num", "denom"), sep = "/") %>% 
  mutate(
    num = as.integer(num),   # convert to numeric
    denom = as.integer(denom),   # convert to numeric
    fract = num/denom,  # calculate decimal
    var_num = whole + fract  # add whole number in front (if any)
  ) %>%
  # clean up
  select(unique_id, var_num) %>%
  print()


## Extract all other numbers
ads_prep <- ads %>%
  # keep only unique identifier & variable of interest
  select(unique_id, var, region) %>%
  # keep generic name
  dplyr::rename(var = 2) %>%
  # join fractions
  left_join(fractions, by = "unique_id") %>%
  # add numbers to var_num
  mutate(
    var_num = ifelse(is.na(var_num) & str_detect(var, regex("one fam|one colored fam|^1 fam| 1 fam", ignore_case = TRUE)), 0.5, var_num),  # estimates for family size
    var_num = ifelse(is.na(var_num) & str_detect(var, regex("two fam|^2 fam| 2 fam|2 negro fam", ignore_case = TRUE)), 1, var_num),
    var_num = ifelse(is.na(var_num) & str_detect(var, regex("three fam|3 fam|3 scattered fam", ignore_case = TRUE)), 1.5, var_num),
    var_num = ifelse(is.na(var_num) & str_detect(var, regex("four fam|4 fam|4 scattered fam", ignore_case = TRUE)), 2, var_num),
    var_num = ifelse(is.na(var_num) & str_detect(var, regex("five fam|5 fam|5 scattered fam", ignore_case = TRUE)), 2.5, var_num),
    var_num = ifelse(is.na(var_num) & str_detect(var, regex("six fam|6 fam|6 scattered fam", ignore_case = TRUE)), 3, var_num),
    var_num = ifelse(is.na(var_num) & str_detect(var, regex("seven fam|7 fam|7 scattered fam", ignore_case = TRUE)), 3.5, var_num),
    var_num = ifelse(is.na(var_num) & str_detect(var, regex("eight families", ignore_case = TRUE)), 4, var_num),
    var_num = ifelse(is.na(var_num) & str_detect(var, regex("10 fam", ignore_case = TRUE)), 5, var_num),
    var_num = ifelse(is.na(var_num) & str_detect(var, regex("12 fam", ignore_case = TRUE)), 4, var_num),  # specified as 4% bc that is what is on sheet
    var_num = ifelse(is.na(var_num) & str_detect(var, regex("20 scattered fam", ignore_case = TRUE)), 10, var_num),
    var_num = ifelse(is.na(var_num) & str_detect(var, regex("21 scattered fam", ignore_case = TRUE)), 11, var_num),
    var_num = ifelse(is.na(var_num), as.numeric(str_extract(var, "[:digit:]+")), var_num),
    var_num = ifelse(is.na(var_num) & str_detect(var, regex("none|NULL|^no|- No|no conc|nil|^N/A|- n/a", ignore_case = TRUE)), 0, var_num),
    var_num = ifelse(is.na(var_num) & str_detect(var, regex("O no$|^Negro$|no$|^x$|jewish", ignore_case = TRUE)), 0, var_num),
    var_num = ifelse(is.na(var_num) & str_detect(var, "^[:punct:]+$|^[:punct:][:space:][:punct:]"), 0, var_num),
    var_num = ifelse(is.na(var_num) & rapportools::is.empty(var, trim = TRUE), 0, var_num),
    var_num = ifelse(str_detect(var, regex("2nd street", ignore_case = TRUE)), 5, var_num)  # fix peculiar case
  ) %>%
  print()

## View
#ads_prep %>% filter(is.na(var_num)) %>% View()  # n = 113


##--------------------------------------------------------
##  Grab mean values of common descriptors
##--------------------------------------------------------

## prepare df to join
df_rgn <- data.frame(
  "region" = c("MW", "NE", "S", "W"),
  c1 = as.numeric(NA),
  n = 0
) %>%
  as_tibble() %>%
  print()


## Check very few  --> get mean of "very few"
v_few <- ads_prep %>%
  filter(
    str_detect(var, regex("very few", ignore_case = TRUE)) & 
      !is.na(var_num)
    ) %>%
  group_by(region) %>%
  summarize(
    c1 = round(mean(var_num, na.rm = TRUE)),
    n = dplyr::n()
  ) %>%
  mutate(c1 = ifelse(n < 3, weighted.mean(c1, n), c1)) %>%
  bind_rows(df_rgn) %>%
  filter(!duplicated(region)) %>%
  mutate(c1 = ifelse(is.na(c1) | n < 3, weighted.mean(c1, n, na.rm = TRUE), c1)) %>%
  arrange(region) %>%
  print() # c1 = 2


## Check few --> get mean of "few"
few <- ads_prep %>%
  filter(
    str_detect(var, regex("few", ignore_case = TRUE)) &
      !str_detect(var, regex("very", ignore_case = TRUE)) &
      !is.na(var_num)
  ) %>%
  group_by(region) %>%
  summarize(
    c1 = mean(var_num, na.rm = TRUE),
    n = dplyr::n()
  ) %>%
  mutate(c1 = ifelse(n < 3, weighted.mean(c1, n), c1)) %>%
  bind_rows(df_rgn) %>%
  filter(!duplicated(region)) %>%
  mutate(c1 = ifelse(is.na(c1) | n < 3, weighted.mean(c1, n, na.rm = TRUE), c1)) %>%
  arrange(region) %>%
  print()  # varies by region


## Check yes --> get mean of "yes"
yes <- ads_prep %>%
  filter(
    str_detect(var, regex("yes", ignore_case = TRUE)) &
      !str_detect(var, regex("few", ignore_case = TRUE)) &
      !str_detect(var, regex("some", ignore_case = TRUE)) &
      !str_detect(var, regex("small", ignore_case = TRUE)) &
      !str_detect(var, regex("negligible", ignore_case = TRUE)) &
      !str_detect(var, regex("substantial", ignore_case = TRUE)) &
      !is.na(var_num)
  ) %>%
  group_by(region) %>%
  summarize(
    c1 = mean(var_num, na.rm = TRUE),
    n = dplyr::n()
  ) %>%
  mutate(c1 = ifelse(n < 3, weighted.mean(c1, n), c1)) %>%
  bind_rows(df_rgn) %>%
  filter(!duplicated(region)) %>%
  mutate(c1 = ifelse(is.na(c1) | n < 3, weighted.mean(c1, n, na.rm = TRUE), c1)) %>%
  arrange(region) %>%
  print()  # varies by region


## Check negligible --> get mean of "negligible"
negligible <- ads_prep %>%
  filter(
    str_detect(var, regex("negligible", ignore_case = TRUE)) &
      !is.na(var_num)
  ) %>%
  group_by(region) %>%
  summarize(
    c1 = mean(var_num, na.rm = TRUE),
    n = dplyr::n()
  ) %>%
  mutate(c1 = ifelse(n < 3, weighted.mean(c1, n), c1)) %>%
  bind_rows(df_rgn) %>%
  filter(!duplicated(region)) %>%
  mutate(c1 = ifelse(is.na(c1) | n < 3, weighted.mean(c1, n, na.rm = TRUE), c1)) %>%
  ## comes up as null --> filling in neglible with 0.5
  mutate(c1 = ifelse(is.na(c1), 0.5, c1)) %>%
  arrange(region) %>%
  print()  # varies by region


## Check nominal --> get mean of "nominal"
nom <- ads_prep %>%
  filter(str_detect(var, regex("nominal", ignore_case = TRUE)) & !is.na(var_num)) %>%
  group_by(region) %>%
  summarize(
    c1 = mean(var_num, na.rm = TRUE),
    n = dplyr::n()
  ) %>%
  mutate(c1 = ifelse(n < 3, weighted.mean(c1, n), c1)) %>%
  bind_rows(df_rgn) %>%
  filter(!duplicated(region)) %>%
  mutate(c1 = ifelse(is.na(c1) | n < 3, weighted.mean(c1, n, na.rm = TRUE), c1)) %>%
  arrange(region) %>%
  print()  # nominal = 2


## Check some --> get mean of "some"
some <- ads_prep %>%
  filter(
    str_detect(var, regex("some", ignore_case = TRUE)) & 
      !is.na(var_num)
    ) %>%
  group_by(region) %>%
  summarize(
    c1 = mean(var_num, na.rm = TRUE),
    n = dplyr::n()
  ) %>%
  mutate(c1 = ifelse(n < 3, weighted.mean(c1, n), c1)) %>%
  bind_rows(df_rgn) %>%
  filter(!duplicated(region)) %>%
  mutate(c1 = ifelse(is.na(c1) | n < 3, weighted.mean(c1, n, na.rm = TRUE), c1)) %>%
  ## some = ~5%
  mutate(c1 = ifelse(is.na(c1), 5, c1)) %>%
  arrange(region) %>%
  print()  # some = 2


## Check small --> get mean of "small"
small <- ads_prep %>%
  filter(str_detect(var, regex("small", ignore_case = TRUE)) & !is.na(var_num)) %>%
  group_by(region) %>%
  summarize(
    c1 = mean(var_num, na.rm = TRUE),
    n = dplyr::n()
  ) %>%
  mutate(c1 = ifelse(n < 3, weighted.mean(c1, n), c1)) %>%
  bind_rows(df_rgn) %>%
  filter(!duplicated(region)) %>%
  mutate(c1 = ifelse(is.na(c1) | n < 3, weighted.mean(c1, n, na.rm = TRUE), c1)) %>%
  ## comes up as null --> filling in neglible with 0.5
  mutate(c1 = ifelse(is.na(c1), 0.5, c1)) %>%
  arrange(region) %>%
  print()  # small = 1


## Check substantial --> get mean of "substantial"
substantial <- ads_prep %>%
  filter(
    str_detect(var, regex("substantial", ignore_case = TRUE)) &
      !is.na(var_num)
  ) %>%
  group_by(region) %>%
  summarize(
    c1 = mean(var_num, na.rm = TRUE),
    n = dplyr::n()
  ) %>%
  # NAs produced --> conservatively estimate as 10%
  mutate(c1 = ifelse(n < 3, weighted.mean(c1, n), c1)) %>%
  bind_rows(df_rgn) %>%
  filter(!duplicated(region)) %>%
  mutate(c1 = ifelse(is.na(c1) | n < 3, weighted.mean(c1, n, na.rm = TRUE), c1)) %>%
  ## comes up as null --> filling in substantial w/ 10
  mutate(c1 = ifelse(is.na(c1), 10, c1)) %>%
  arrange(region) %>%
  print()  # substantial = 10

## Check substantial --> get mean of "substantial"
several <- ads_prep %>%
  filter(
    str_detect(var, regex("several", ignore_case = TRUE)) &
      !is.na(var_num)
  ) %>%
  group_by(region) %>%
  summarize(
    c1 = mean(var_num, na.rm = TRUE),
    n = dplyr::n()
  ) %>%
  # NAs produced --> conservatively estimate as 10%
  mutate(c1 = ifelse(n < 3, weighted.mean(c1, n), c1)) %>%
  bind_rows(df_rgn) %>%
  filter(!duplicated(region)) %>%
  mutate(c1 = ifelse(is.na(c1) | n < 3, weighted.mean(c1, n, na.rm = TRUE), c1)) %>%
  ## comes up as null --> filling in substantial w/ 10
  mutate(c1 = ifelse(is.na(c1), 5, c1)) %>%
  arrange(region) %>%
  print()  # several = 5


## Check threatening --> get mean of "threatening"
threat <- ads_prep %>%
  filter(
    str_detect(var, regex("threat", ignore_case = TRUE)) &
      !is.na(var_num)
  ) %>%
  group_by(region) %>%
  summarize(
    c1 = mean(var_num, na.rm = TRUE),
    n = dplyr::n()
  ) %>%
  mutate(c1 = ifelse(n < 3, weighted.mean(c1, n), c1)) %>%
  bind_rows(df_rgn) %>%
  filter(!duplicated(region)) %>%
  mutate(c1 = ifelse(is.na(c1) | n < 3, weighted.mean(c1, n, na.rm = TRUE), c1)) %>%
  arrange(region) %>%
  print() 



##--------------------------------------------------------
##  Extract For. Born. Estimates for remaining Null
##--------------------------------------------------------

ads_null <- NULL
for(i in unique(c("MW", "NE", "S", "W"))){
  
  temp <- ads_prep %>%
    filter(is.na(var_num) & region == i) %>%
    mutate(
      var_num = 
        case_when(
          str_detect(var, regex("very few", ignore_case = TRUE)) ~ v_few$c1[v_few$region == i],
          str_detect(var, regex("few|fwe", ignore_case = TRUE)) &
            !str_detect(var, regex("very", ignore_case = TRUE)) ~ few$c1[few$region == i],
          str_detect(var, regex("negligible", ignore_case = TRUE)) ~ negligible$c1[negligible$region == i],
          str_detect(var, regex("nominal", ignore_case = TRUE)) ~ nom$c1[nom$region == i],
          str_detect(var, regex("some", ignore_case = TRUE)) ~ some$c1[some$region == i],
          str_detect(var, regex("small", ignore_case = TRUE)) ~ small$c1[small$region == i],
          str_detect(var, regex("substantial", ignore_case = TRUE)) ~ substantial$c1[substantial$region == i],
          str_detect(var, regex("several", ignore_case = TRUE)) ~ several$c1[several$region == i],
          str_detect(var, regex("threat", ignore_case = TRUE)) ~ threat$c1[threat$region == i],
          str_detect(var, regex("yes", ignore_case = TRUE)) &
            !str_detect(var, regex("few", ignore_case = TRUE)) &
            !str_detect(var, regex("some", ignore_case = TRUE)) &
            !str_detect(var, regex("small", ignore_case = TRUE)) &
            !str_detect(var, regex("negligible", ignore_case = TRUE)) &
            !str_detect(var, regex("substantial", ignore_case = TRUE)) ~ yes$c1[yes$region == i],
          TRUE ~ var_num
        )
    ) 
  
  ads_null <- bind_rows(ads_null, temp)
  
}

ads_null  # inspect

## check for null
#ads_null %>% filter(is.na(var_num)) %>% View()


##--------------------------------------------------------
##  Join back and rename column
##--------------------------------------------------------

ads_blk <- ads_prep %>%
  left_join(ads_null[c(1,4)], by = c("unique_id"), suffix = c("", "2")) %>%
  mutate(blk_num = ifelse(is.na(var_num2), var_num, var_num2)) %>%
  dplyr::rename(black = var) %>%
  select(unique_id, black, blk_num) %>%
  print()


##--------------------------------------------------------
##  Save out!!
##--------------------------------------------------------

write_csv(ads_blk, "DATA_DOWNLOAD/ADS_Black.csv")

