
#########################################################################
#########################################################################
###                                                                   ###
###                       LOAD in HOLC Data                           ###
###                                                                   ###
#########################################################################
#########################################################################

## PREPARE WORKSPACE
source("scripts/00_preamble.R")

## Create DATA_DOWNLOAD folder
#dir.create("DATA_DOWNLOAD")


#################################################################################
## LOAD HOLC data
#################################################################################

##-------------------------------------------------------------------------------
##  GeoJSON download
##-------------------------------------------------------------------------------

### Data from Digital Scholarship Lab @ the Univ. of Richmond: https://dsl.richmond.edu/panorama/redlining/#loc=4/40.886/-105.499&text=downloads
## Can skip to line 31 in future after saving this data locally (DSL may be add more data)
#u <- "https://dsl.richmond.edu/panorama/redlining/static/fullDownload.geojson"  # get url
#downloader::download(url = u, destfile = "tables/holc_json.GeoJSON")  # save locally in tables folder

## For once data is saved locally
holc_json <- rgdal::readOGR("tables/holc_json.GeoJSON")  # import
summary(holc_json)  # inspect


##-------------------------------------------------------------------------------
##  GeoJSON Organize
##-------------------------------------------------------------------------------

df <- holc_json %>%
  as_tibble() %>%
  # create unique state-city-neighborhood ID
  mutate(
    unique_id = paste(state, city, holc_id, sep = "_"),
    unique_id = str_replace_all(unique_id, ",", ""),  # remove all commas
    unique_id = str_replace_all(unique_id, "\\.", ""),  # remove all periods
    unique_id = str_replace(unique_id, " and ", ""),  # remove "and"s
    unique_id = str_replace_all(unique_id, "[[:space:]]", "")  # remove all spaces
    ) %>%
  select(-neighborhood_id) %>%
  # remove NAs
  drop_na(holc_id) %>%
  # remove missing data
  filter(area_description_data != "{ \"\": \"\" }") %>%
  print()


##-------------------------------------------------------------------------------
##  Load Prepared file
##-------------------------------------------------------------------------------

holc_cities <- read_csv("tables/holc_cities.csv") %>%
  # add column to include only cities w/ detailed Area Description Sheets
  print()

# state
# city
# metro:       metro region HOLC city is apart of
# nhoods:      number of HOLC neighborhoods in each city
# sheets:      % of HOLC sheets w/ descriptions included
# centroid50:  % of HOLC neighborhood boundaries covered by 1940 tract centroids
# region:      region of country city is in
# yr_sheet:    date on HOLC area description sheet
# ads_type:    type of sheet: early 1937, late 1937, 1939-40, paragraph, none, or unique
# include:     cities w/ either eary 1937, late 1937, or 1939-40 description ADS types


#################################################################################
## ORGANIZE HOLC data --> prep for extracting area descriptions
#################################################################################

## organize area_descr
ads_prep <- df %>%
  # join with holc_cities to get ads_type data
  left_join(
    holc_cities[c(1:3, 6, 8)],   # 1-state; 2-city; 3-metro; 6-region; 8-ads_type (type of sheet)
    by = c("state","city")
    ) %>%
  # keep only cities with detailed Area Description Sheets
  filter(ads_type %in% c("early37", "late37", "x3940"))  %>%
  # split ADS data by "\"
  mutate(split = str_split(area_description_data, "\", \"")) %>%
  unnest(cols = split) %>%
  # separate sections from comments
  separate(split, c("section", "comment"), "\": ") %>%
  select(-name) %>%
  print()


## Clean up "section" and "comment" columns
ads_org <- ads_prep %>%
  mutate(
    section = str_extract(section, "[:alnum:]+"),  # extract alphanumeric characters
    comment = str_replace(comment, "\"", ""),  # remove quotation marks from comment
    comment = str_replace(comment, " \\}", ""),  # remove end braces from comment
    comment = str_replace(comment, "\\\\", ""),  # remove back slashes
    comment = str_replace(comment, "\\\"", ""),  # remove backslash + quot. mark
    comment = str_trim(comment, "both")  # trim out whitespace on ends
  ) %>%
  arrange(unique_id, section) %>%
  # clean up
  select(-area_description_data) %>%
  mutate(
    sect_num = as.integer(str_extract(section, "[:digit:]+")),
    sect_alpha = str_extract(section, "[:alpha:]+")
  ) %>%
  print()


## Clean up workspace
rm(ads_prep)


#################################################################################
## Handle Cities w/ Early 1937 Area Description Sheets (ADS)
#################################################################################

##--------------------------------------------------------
##  ORGANIZE
##--------------------------------------------------------

ads_e37_prep <- ads_org %>%
  # grab only early 1937 data
  filter(ads_type == "early37") %>%
  # remove sections 0 and 1 (non-essential info)
  filter(!sect_num %in% c(0, 1)) %>%
  # remove 11s & 12s that are incorrectly categorized as ones (see Lima, OH)
  filter(!(sect_num %in% c(11, 12) & is.na(sect_alpha))) %>%
  # some formatting mistakes w/ Lima, OH
  filter(city != "Lima") %>%
  # fix section now
  mutate(
    section = paste0(str_pad(sect_num, 2, pad = "0"), sect_alpha),
    section = str_replace(section, "NA", "")
      ) %>%
  select(-sect_num, -sect_alpha) %>%
  arrange(section) %>%
  print()


##--------------------------------------------------------
##  Rename and Put in Final Format
##--------------------------------------------------------

ads_e37 <- ads_e37_prep %>%
  mutate(
    sect_name = 
      case_when(
        # 1-4: BASIC INFO
        #section == "1" ~ "city_name",  # name of city
        section == "02" ~ "terrain",  # description of terrain
        section == "03" ~ "fav_inf",  # favorable influences
        section == "04" ~ "det_inf",  # detrimental influences
        # 5: INHABITANTS
        section == "05a" ~ "inh_type",  # inhabitants type
        section == "05b" ~ "fam_inc",  # estimated annual family income
        section == "05c" ~ "for_born",  # foreign-born
        section == "05d" ~ "black",  # "Negro"
        section == "05e" ~ "infilt",  # "Infiltration of"
        section == "05f" ~ "relief_fams",  # relief families
        section == "05g" ~ "pop_chg",  # "Population is increasing_____; decreasing_______; static."
        # 6: BUILDINGS
        section == "06a" ~ "bdg_type",  # "Type or types"
        section == "06b" ~ "constr_type",  # "Type of construction"
        section == "06c" ~ "avg_age",  # average age
        section == "06d" ~ "repair",  # "Repair"
        # 7: HISTORY (needs further breaking down)
        section == "07" ~ "hist_prices",  # list of historic sale and rental values from 1929, 1933, present
        # 8: OCCUPANCY
        section == "08a" ~ "occ_land",  # "Land"
        section == "08b" ~ "occ_dwell",  # "Dwelling units"
        section == "08c" ~ "occ_own",  # homeowners
        # 9: SALES DEMAND
        section == "09a" ~ "sale_dem",  # sales demand
        section == "09b" ~ "sale_dem2", # sales demand additional note 
        section == "09c" ~ "sale_act",  # sales demand activity
        # 10: RENTAL DEMAND
        section == "10a" ~ "rent_dem",  # rental demand
        section == "10b" ~ "rent_dem2",  #rental demand additional note
        section == "10c" ~ "rent_act",  # rental demand activity
        # 11: NEW CONSTRUCTION
        section == "11a" ~ "new_constr_type",  # new construction type
        section == "11b" ~ "new_constr_amt",  # new consturction type amount last year
        # 12: AVAILABILITY OF MORTGAGE FUNDS
        section == "12a" ~ "mort_av_buy",  # availability of mortgage funds: home purchase
        section == "12b" ~ "mort_av_bld",  # availability of mortgage funds: home building
        # 13: TREND OF DESIRABILITY NEXT 10-15 YEARS
        section == "13" ~ "trend_des",  # trend of desirability over next 10-15 years
        # 14: CLARIFYING REMARKS
        section == "14" ~ "remarks",  # clarifying remarks
        # 15: BIOGRAPHICAL INFO about APPRAISER
        section == "15" ~ "appraiser"  # "Information for this form was obtained from"
       #TRUE ~ as.character(section)
       )
    ) %>%
  # clean up & rearrange
  select(state:ads_type, sect_name, comment) %>%
  # make wide for mutate function
  pivot_wider(
    names_from = sect_name,
    values_from = comment
    ) %>%
  select(-`NA`) %>%
  print()  # n = 2,068


## Clean up workspace
rm(ads_e37_prep)


#################################################################################
## Handle Cities w/ Early 1937 Area Description Sheets (ADS)
#################################################################################

##--------------------------------------------------------
##  ORGANIZE
##--------------------------------------------------------

ads_l37_prep <- ads_org %>%
  # grab only early 1937 data
  filter(ads_type == "late37") %>%
  # remove sections 0 and 1 (non-essential info)
  filter(sect_num != 0) %>%
  # fix section now
  mutate(
    sect_num = ifelse(sect_num > 6, as.character(round(sect_num / 10, 1)), as.character(sect_num)),  # correct decimals
    section = paste0(sect_num, sect_alpha),  # combine section number and alpha character (1a, 1b, etc)
    section = str_replace(section, "NA", "")  # remove NAs in cases w/ no alphas
  ) %>%
  select(-sect_num, -sect_alpha) %>%
  ## don't need this info
  filter(!section %in% c("6", "6.1", "6.2", "6.3")) %>%
  arrange(section) %>%
  print()


##--------------------------------------------------------
##  Rename and Put in Final Format
##--------------------------------------------------------

ads_l37 <- ads_l37_prep %>%
    mutate(
      sect_name = 
        case_when(
          # 1: AREA CHARACTERISTICS
          section == "1a" ~ "terrain",
          section == "1b" ~ "fav_inf",
          section == "1c" ~ "det_inf",
          section == "1d" ~ "p_land_imp",
          section == "1e" ~ "trend_des",
          # 2: INHABITANTS
          section == "2a" ~ "occupation",  # "Occupation"--same as inh_type in e37 perhaps...
          section == "2b" ~ "fam_inc",  # est. annual family income
          section == "2c" ~ "for_born",  # foreign-born
          section == "2d" ~ "black",  # "Negro"
          section == "2e" ~ "infilt",  # "Infiltration of"
          section == "2f" ~ "relief_fams",  # relief families
          section == "2g" ~ "pop_chg",  # "Population is increasing_____; decreasing_______; static________"
          # 3: BUILDINGS
          section == "3.1" ~ "bdg_predom",  # PREDOMINATING %
          section == "3.2" ~ "bdg_oth_type1",  # OTHER TYPE %
          section == "3.3" ~ "bdg_oth_type2",  # OTHER TYPE %"
          section == "3a" ~ "bdg_type",  # building type (by 3 categories: predominating, other1, other2)
          section == "3b" ~ "constr_type",  # construction type (by 3 categories: predominating, other1, other2)
          section == "3c" ~ "avg_age",  # average age of structure
          section == "3d" ~ "repair",  # 
          section == "3e" ~ "occ_dwell",
          section == "3f" ~ "occ_own",
          section == "3g" ~ "constr_past_yr",  # simlar to "new_constr_amt" from e37
          section == "3h" ~ "price29",  # price range in 1929
          section == "3i" ~ "price_mid30s",  # home price for some is 1935, some 1936
          section == "3j" ~ "price_late30s",  # home price for some is 1937 ,some 1938
          section == "3k" ~ "sale_dem",  # sales demand
          section == "3l" ~ "sale_act",  # sales activity
          section == "3m" ~ "rent29",  # rent range in 1929
          section == "3n" ~ "rent_mid30s",  # rent range in '35 or '36
          section == "3o" ~ "rent_late30s",  # rent range in '37 or '38
          section == "3p" ~ "rent_dem",  # rental demand
          section == "3q" ~ "rent_act",  # rental activity
          # 4: AVAILABILITY OF MORTGAGE FUNDS
          section == "4a" ~ "mort_av_buy",  # availability of mortgage funds: home purchase
          section == "4b" ~ "mort_av_bld",  # availability of mortgage funds: home building
          # 5: CLARIFYING REMARKS
          section == "5" ~ "remarks"  # clarifying remarks
          )
      ) %>%
    # rearrange and drop section column
    select(state:ads_type, sect_name, comment) %>%
    # trim whitespace
    mutate(comment = str_trim(comment, "both")) %>%
    # make wide to allow mutation of 'black' column
    pivot_wider(
      names_from = sect_name,
      values_from = comment
      ) %>%
    # remove errant column
    select(-`NA`) %>%
    print()


## Clean up workspace
rm(ads_l37_prep)


################################################################################
## Handle Cities w/ 1939-1940 Area Description Sheets (ADS)
#################################################################################

##--------------------------------------------------------
##  ORGANIZE
##--------------------------------------------------------

ads_x3940_prep <- ads_org %>%
  # grab only early 1937 data
  filter(ads_type == "x3940") %>%
  # remove sections 0  (non-essential info) and 11-15 (rare cases in Bergen Co., NJ)
  filter(sect_num != 0 & !between(sect_num, 11, 15)) %>%
  # fix section now
  mutate(
    sect_num = ifelse(sect_num > 70, as.character(round(sect_num / 10, 1)), as.character(sect_num)),  # correct decimals
    section = paste0(sect_num, sect_alpha),  # combine section number and alpha character (1a, 1b, etc)
    section = str_replace(section, "NA", "")  # remove NAs in cases w/ no alphas
  ) %>%
  select(-sect_num, -sect_alpha) %>%
  arrange(section) %>%
  print()


##--------------------------------------------------------
##  Rename and Put in Final Format
##--------------------------------------------------------

ads_3940 <- ads_x3940_prep %>%
  mutate(
    sect_name = 
      case_when(
        # 1: POPULATION
        section == "1a" ~ "pop_chg",  # same as pop_chg above
        section == "1b" ~ "occupation",  # "Class and occupation"
        section == "1c" ~ "for_born",  # "Foreign Families
        section == "1d" ~ "black",  # "Negro"
        section == "1e" ~ "infilt",  # "Shifting or Infiltration"
        # 2: BUILDINGS
        section == "2" ~ "bdg_predom",  # 
        section == "2a" ~ "bdg_type",
        section == "2b" ~ "constr_type",
        section == "2c" ~ "avg_age",
        section == "2d" ~ "repair",
        section == "2e" ~ "occ_dwell",
        section == "2f" ~ "occ_own",
        section == "2g" ~ "price35",  # home price in 1935
        section == "2h" ~ "price37",  # home price in 1937
        section == "2i" ~ "price39_40", # home price at time of appraisal
        section == "2j" ~ "sale_dem",  # sales demand
        section == "2k" ~ "pred_price_trend",  # predicted price trend for next 6-12 months
        section == "2l" ~ "rent35",  # rents in 1935
        section == "2m" ~ "rent37",  # rents in 1937
        section == "2n" ~ "rent39_40",  # rents in 1939 or 1940
        section == "2o" ~ "rent_dem",  # rental demand
        section == "2p" ~ "pred_rent_trend",  # predicted rent trend for next 6-12 months
        # 3: NEW CONSTRUCTION
        section == "3" ~ "new_constr",  # new construction in past year: "no."; "type & price"; "how selling"
        # 4: OVERHANG OF HOME PROPERTIES
        section == "4a" ~ "overhang_holc",  # HOLC number and price of homes in foreclosure overhang (foreclosed properties for sale)
        section == "4b" ~ "overhang_inst",  # overhang properties owned by other (non-HOLC) institutions
        # 5: SALE OF HOME PROPERTIES
        section =="5" ~ "prop_sales",  # number of years for which 5a and 5b were determined
        section =="5a" ~ "sales_holc",  # number and price of home properties sold by the HOLC
        section == "5b" ~ "sales_inst",  # number and price of home properties sold by other (non-HOLC) institutions
        # 6: MORTGAGE FUNDS
        section == "6" ~ "mort_av_buy",  # availability of mortgage funds (doesn't specify if for building or buying--assume buying)
        # 7: TAX RATES
        section == "71" ~ "tot_tax_rate",  # total tax rate per $1000 in 1939 or 1940
        section == "72" ~ "eff_tax_rate",  # effective tax rate based on 25 sales
        # 8: DESCRIPTION AND CHARACTERISTICS OF AREA
        section == "8" ~ "remarks",  # similar to clarifying remarks on other sheets but prompt is slightly different--asks for "desc." and "chars."
        # 9: LOCATION, SECURITY GRADE, AREA NO., DATE
        section == "9" ~ "loc_grade_no_date"
        )
    ) %>%
  # rearrange and drop section column
  select(state:ads_type, sect_name, comment) %>%
  # trim whitespace
  mutate(comment = str_trim(comment, "both")) %>%
  # make wide to allow mutation of 'black' column
  pivot_wider(
    names_from = sect_name,
    values_from = comment
    ) %>%
  # remove errant column
  select(-`NA`) %>%
  print()


## Clean up
rm(ads_x3940_prep)
  

#####################################################
## SAVE OUT!!
#####################################################

## Add output TABLES folder
#dir.create("DATA_DOWNLOAD/TABLES")

## Create list of dataframes
df_list <- list(
  "e37" = ads_e37, 
  "l37" = ads_l37, 
  "x3940" = ads_3940
  )

## Write as Excel workbook so can store long text
openxlsx::write.xlsx(
  df_list, 
  "DATA_DOWNLOAD/TABLES/ADS_organized.xlsx"
  )


