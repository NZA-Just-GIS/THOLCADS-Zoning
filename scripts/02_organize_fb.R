
#########################################################################
#########################################################################
###                                                                   ###
###                  PREPARE FOREIGN BORN COLUMN                      ###
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
    dplyr::select(state:ads_type, for_born, black, infilt, avg_age, mort_av_buy) %>%
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
var <- "for_born"

## Isolate fractions and convert them to decimals
fractions <- ads %>%
  # keep only unique identifier & variable of interest
  dplyr::select(unique_id, var) %>%
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
  dplyr::select(unique_id, var_num) %>%
  print()


## Extract all other numbers
ads_pre_prep <- ads %>%
  # keep only unique identifier & variable of interest
  dplyr::select(unique_id, var, region, holc_grade) %>%
  # keep generic name
  dplyr::rename(var = 2) %>%
  # join fractions
  left_join(fractions, by = "unique_id") %>%
  # add numbers to var_num
  mutate(
    var_num = ifelse(is.na(var_num) & str_detect(var, regex("one mexican family", ignore_case = TRUE)), 0.5, var_num),
    var_num = ifelse(is.na(var_num) & str_detect(var, regex("only 3 italians fam", ignore_case = TRUE)), 1.5, var_num),
    var_num = ifelse(is.na(var_num) & str_detect(var, regex("4 jewish fam", ignore_case = TRUE)), 2, var_num),
    var_num = ifelse(is.na(var_num), as.numeric(str_extract(var, "[:digit:]+")), var_num),
    var_num = ifelse(is.na(var_num) & str_detect(var, regex("NULL|^no|- No|no conc|nil|^N/A|- n/a", ignore_case = TRUE)), 0, var_num),
    var_num = ifelse(is.na(var_num) & 
                       str_detect(var, regex("none", ignore_case = TRUE)) &
                       !str_detect(var, regex("few", ignore_case = TRUE)), ## remove cases with "few none" on W
                     0, var_num),
    var_num = ifelse(is.na(var_num) & 
                       str_detect(var, regex("none", ignore_case = TRUE)) &
                       str_detect(var, regex("few", ignore_case = TRUE)), ## give "few none" 2%
                     2, var_num),
    var_num = ifelse(is.na(var_num) & str_detect(var, "^[:punct:]+$|^[:punct:][:space:][:punct:]"), 0, var_num),
    var_num = ifelse(is.na(var_num) & rapportools::is.empty(var, trim = TRUE), 0, var_num),
    var_num = ifelse(is.na(var_num) & 
                       str_detect(var, regex("one", ignore_case = TRUE)) & 
                       !str_detect(var, regex("none", ignore_case = TRUE)), 
                     1, var_num),
    var_num = ifelse(is.na(var_num) & str_detect(var, regex("two", ignore_case = TRUE)), 2, var_num),
    var_num = ifelse(var_num == 0 & !str_detect(var, "0") & str_detect(var, regex("none sub", ignore_case = TRUE)), NA, var_num),
    var_num = ifelse(var_num == 0 & str_detect(var, regex("few", ignore_case = TRUE)), NA, var_num)
    ) %>%
  print()


# Inspect
#ads_prep %>% filter(is.na(var_num)) %>% View()

##-----------------------------------------------
## Correct special cases in Chicago
##-----------------------------------------------

## Load Chicago fix data
chicago_fix <- read_csv("tables/chicago_fix.csv") %>%
  print()


## Join
ads_prep <- ads_pre_prep %>%
  left_join(chicago_fix, by = "unique_id") %>%
  mutate(
    var_num = ifelse(!is.na(for_born), for_born, var_num),
    var = ifelse(!is.na(fb_text), fb_text, var),
    var = ifelse(str_detect(var, "December"), "", var)  # fix "December" cases in Chicago
    ) %>%
  dplyr::select(-c(black:fb_text)) %>%
  print()


##--------------------------------------------------------
##  Grab mean values of common descriptors by region
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
  filter(str_detect(var, regex("very few", ignore_case = TRUE)) & !is.na(var_num)) %>%
  group_by(region) %>%
  dplyr::summarize(
    c1 = round(mean(var_num, na.rm = TRUE)),
    n = dplyr::n()
  ) %>%
  mutate(c1 = ifelse(n < 3, weighted.mean(c1, n), c1)) %>%
  bind_rows(df_rgn) %>%
  filter(!duplicated(region)) %>%
  mutate(c1 = ifelse(is.na(c1) | n < 3, weighted.mean(c1, n, na.rm = TRUE), c1)) %>%
  # make c1 = 2
  mutate(c1 = ifelse(is.na(c1), 2, c1)) %>%
  arrange(region) %>%
  print() # c1 = 2


## Check few --> get mean of "few"
few <- ads_prep %>%
  filter(
    str_detect(var, regex("^few|few$|^a few|^- few|a few families", ignore_case = TRUE)) &
      !str_detect(var, regex("very", ignore_case = TRUE)) &
    !is.na(var_num)
  ) %>%
  group_by(region) %>%
  dplyr::summarize(
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
  group_by(region, holc_grade) %>%
  dplyr::summarize(
    c1 = mean(var_num, na.rm = TRUE),
    n = dplyr::n()
  ) %>%
  #mutate(c1 = ifelse(n < 3, weighted.mean(c1, n), c1)) %>%
  bind_rows(df_rgn) %>%
  #filter(!duplicated(region)) %>%
  ungroup() %>%
  complete(region, holc_grade) %>%
  mutate(holc_grade = ifelse(is.na(holc_grade), "A", holc_grade)) %>%
  mutate(c1 = ifelse(is.na(c1) | n < 3, 2, c1)) %>%
  arrange(region, holc_grade) %>%
  print()  # varies by region and grade


## Check negligible --> get mean of "negligible"
negligible <- ads_prep %>%
  filter(
    str_detect(var, regex("negligible", ignore_case = TRUE)) &
      !is.na(var_num)
  ) %>%
  group_by(region) %>%
  dplyr::summarize(
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


## Check trace --> get mean of "trace"
trace <- ads_prep %>%
  filter(
    str_detect(var, regex("^trace| trace|trace$", ignore_case = TRUE)) &
      !is.na(var_num)
  ) %>%
  group_by(region) %>%
  dplyr::summarize(
    c1 = mean(var_num, na.rm = TRUE),
    n = dplyr::n()
  ) %>%
  mutate(c1 = ifelse(n < 3, weighted.mean(c1, n), c1)) %>%
  bind_rows(df_rgn) %>%
  filter(!duplicated(region)) %>%
  mutate(c1 = ifelse(is.na(c1) | n < 3, weighted.mean(c1, n, na.rm = TRUE), c1)) %>%
  ## comes up as null --> filling in neglible with 0.5
  mutate(c1 = ifelse(is.na(c1), 2, c1)) %>%
  arrange(region) %>%
  print()  # varies by region


## Check nominal --> get mean of "nominal"
nom <- ads_prep %>%
  filter(str_detect(var, regex("nominal", ignore_case = TRUE)) & !is.na(var_num)) %>%
  group_by(region) %>%
  dplyr::summarize(
    c1 = mean(var_num, na.rm = TRUE),
    n = dplyr::n()
  ) %>%
  mutate(c1 = ifelse(n < 3, weighted.mean(c1, n), c1)) %>%
  bind_rows(df_rgn) %>%
  filter(!duplicated(region)) %>%
  mutate(c1 = ifelse(is.na(c1) | n < 3, weighted.mean(c1, n, na.rm = TRUE), c1)) %>%
  # make nominal = 2%
  mutate(c1 = ifelse(is.na(c1), 2, c1)) %>%
  arrange(region) %>%
  print()  # nominal = 2


## Check some --> get mean of "some"
some <- ads_prep %>%
  filter(str_detect(var, regex("some", ignore_case = TRUE)) & !is.na(var_num)) %>%
  group_by(region) %>%
  dplyr::summarize(
    c1 = mean(var_num, na.rm = TRUE),
    n = dplyr::n()
  ) %>%
  mutate(c1 = ifelse(n < 3, weighted.mean(c1, n), c1)) %>%
  bind_rows(df_rgn) %>%
  filter(!duplicated(region)) %>%
  mutate(c1 = ifelse(is.na(c1) | n < 3, weighted.mean(c1, n, na.rm = TRUE), c1)) %>%
  arrange(region) %>%
  # make some = 2%
  mutate(c1 = ifelse(is.na(c1), 2, c1)) %>%
  print()  # some = 2


## Check small --> get mean of "small"
small <- ads_prep %>%
  filter(str_detect(var, regex("small", ignore_case = TRUE)) & !is.na(var_num)) %>%
  group_by(region) %>%
  dplyr::summarize(
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


## Check mix --> get mean of "mix"
mix <- ads_prep %>%
    filter(
      str_detect(var, regex("mix|all kind|all nat|various", ignore_case = TRUE))  &
        !str_detect(var, regex("few", ignore_case = TRUE)) &
        !is.na(var_num)
    ) %>%
    group_by(region, holc_grade) %>%
    dplyr::summarize(
      c1 = mean(var_num, na.rm = TRUE),
      n = dplyr::n()
    ) %>%
  #mutate(c1 = ifelse(n < 3, weighted.mean(c1, n), c1)) %>%
  #bind_rows(df_rgn) %>%
  #filter(!duplicated(region)) %>%
  #mutate(c1 = ifelse(is.na(c1) | n < 3, weighted.mean(c1, n, na.rm = TRUE), c1)) %>%
  ## comes up as null --> filling in neglible with 0.5
  ungroup() %>%
  complete(region, holc_grade) %>%
  mutate(c1 = ifelse(is.na(c1), 2, c1)) %>%
  arrange(region) %>%
  print()  # mix --> MW: 33.5; NE: 36.7; S: 4.93; W: 20


## Check substantial --> get mean of "substantial"
substantial <- ads_prep %>%
  filter(
    str_detect(var, regex("substantial", ignore_case = TRUE)) &
      !is.na(var_num)
  ) %>%
  group_by(region) %>%
  dplyr::summarize(
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


## Check threatening --> get mean of "threatening"
threat <- ads_prep %>%
  filter(
    str_detect(var, regex("threat", ignore_case = TRUE)) 
  ) %>%
  group_by(region) %>%
  dplyr::summarize(
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
  #mutate(c1 = ifelse(is.na(c1), 2, round(c1))) %>%
  print()  # threat = 2


## Check "none subversive"
none_sub <- ads_prep %>%
  filter(
    str_detect(var, regex("none sub", ignore_case = TRUE)) 
  ) %>%
  group_by(region) %>%
  dplyr::summarize(
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
  print()  # non_sub == 6.89


##--------------------------------------------------------
##  Extract For. Born. Estimates for remaining Null
##--------------------------------------------------------

ads_null <- NULL
for(i in unique(c("MW", "NE", "S", "W"))){
  for(j in unique(c("A", "B", "C", "D"))){
    
    temp <- ads_prep %>%
      filter(is.na(var_num) & region == i & holc_grade == j) %>%
      mutate(
        var_num = 
          case_when(
            str_detect(var, regex("very few", ignore_case = TRUE)) ~ v_few$c1[v_few$region == i],
            str_detect(var, regex("^few|few$|few few|^a few|^- few|a few families", ignore_case = TRUE)) &
              !str_detect(var, regex("very", ignore_case = TRUE)) ~ few$c1[few$region == i],
            str_detect(var, regex("negligible", ignore_case = TRUE)) ~ negligible$c1[negligible$region == i],
            str_detect(var, regex("^trace| trace|trace$", ignore_case = TRUE)) ~ trace$c1[trace$region == i],
            str_detect(var, regex("nominal", ignore_case = TRUE)) ~ nom$c1[nom$region == i],
            str_detect(var, regex("some", ignore_case = TRUE)) ~ some$c1[some$region == i],
            str_detect(var, regex("small", ignore_case = TRUE)) ~ small$c1[small$region == i],
            #str_detect(var, regex("various", ignore_case = TRUE)) ~ various$c1[various$region == i],
            str_detect(var, regex("mix|all kind|all nat|various", ignore_case = TRUE)) ~ mix$c1[mix$region == i & mix$holc_grade == j],
            str_detect(var, regex("substantial", ignore_case = TRUE)) ~ substantial$c1[substantial$region == i],
            str_detect(var, regex("threat", ignore_case = TRUE)) ~ threat$c1[threat$region == i],
            str_detect(var, regex("none sub", ignore_case = TRUE)) ~ none_sub$c1[none_sub$region == i],
            str_detect(var, regex("yes", ignore_case = TRUE)) &
              !str_detect(var, regex("few", ignore_case = TRUE)) &
              !str_detect(var, regex("some", ignore_case = TRUE)) &
              !str_detect(var, regex("small", ignore_case = TRUE)) &
              !str_detect(var, regex("negligible", ignore_case = TRUE)) &
              !str_detect(var, regex("substantial", ignore_case = TRUE)) ~ yes$c1[yes$region == i & yes$holc_grade == j],
            is.na(var_num) & str_detect(var, regex("native|american", ignore_case = T)) ~ 0,
            is.na(var_num) & str_detect(var, regex("none few", ignore_case = T)) ~ 2,
            TRUE ~ var_num
            ),
        fb_flag = 1  # flag that these are estimates
        )
   
    
    ads_null <- bind_rows(ads_null, temp)
    ## Remaining NAs --> Name only nationality, provide no number (need interpolation w/ census data)
    
  }
  
}

ads_null  # inspect

# View remaining nulls
#ads_null %>% filter(is.na(var_num)) %>% View()  # n = 53


##--------------------------------------------------------
##  Extract nationalities
##--------------------------------------------------------

ads_group <- ads_prep %>%
  mutate(
    text = paste(as.character(str_extract_all(var, "[:alpha:]+")), sep = ","),
    text = str_replace(text, "c\\(", ""),
    text = str_replace_all(text, "\"", ""),
    text = str_replace(text, "\\)", ""),
    text = str_replace_all(text, ",", ""),
    text = as.character(ifelse(text %in% c("character(0)", "character(0"), "", text))
  ) %>%
  mutate(
    group = 
      str_replace_all(
        text, 
        regex(
          "known|not |practically|Predominantly|Predominately|possibly|^none|none$|^no | no$|concentration|
          a few|few|small| and |very|substantial|threatening|yes|N A|nominal|less|than |percent|
          about|some|but|defined|nil|whatever|kinds|kind|see remarks|see below|December|or |of
          foreign|^A$|about|number|^one|family|families|none$|scattered", 
          ignore_case = TRUE
          ),  
        ""
        ),
    group = str_trim(group, "both"),
    group = as.character(ifelse(group %in% c("character(0)", "character(0"), "", group))
  ) %>%
  print()



##--------------------------------------------------------
##  Join back and rename column
##--------------------------------------------------------

ads_fb <- ads_prep %>%
  left_join(ads_null[c(1,5,6)], by = c("unique_id"), suffix = c("", "2")) %>%
  left_join(ads_group[c(1,6,7)], by = "unique_id") %>%
  mutate(
    fb_num = ifelse(is.na(var_num2), var_num, var_num2),
    fb_flag = ifelse(is.na(fb_flag), 0, fb_flag)
    ) %>%
  dplyr::rename(for_born = var) %>%
  dplyr::select(unique_id:region, fb_num, text, group, fb_flag) %>%
  # add chicago fix descriptions
  left_join(chicago_fix, by = "unique_id", suffix = c("", "_cf")) %>%
  mutate(text = ifelse(!is.na(fb_text), fb_text, text)) %>%
  dplyr::select(unique_id, fb_num, group, text, fb_flag) %>%
  dplyr::rename(
    fb_group = group,
    fb_txt = text
    ) %>%
  print()


## check null
ads_fb %>%
  filter(is.na(fb_num))


##--------------------------------------------------------
##  Save out!!
##--------------------------------------------------------

write_csv(ads_fb, "DATA_DOWNLOAD/ADS_Foreign_Born.csv")

