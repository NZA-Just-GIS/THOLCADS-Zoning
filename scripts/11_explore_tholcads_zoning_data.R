#########################################################################
#########################################################################
###                                                                   ###
###           Explore mentions of Zoning in ADS                       ###
###                                                                   ###
#########################################################################
#########################################################################

## prepare workspace
source("scripts_nza/00_preamble.R")
library(writexl)

## read in complete file
##================================================
# note if you want to download the files take out "~/Downloads"
## first set
ADS_cities1 <- read.xlsx("~/Downloads/DATA_DOWNLOAD/TABLES/ADS_organized.xlsx", sheet = 1) |>
  as_tibble() |>
  select(state:ads_type, fav_inf, det_inf, remarks) |>
  print()


## second set
ADS_cities2 <- read.xlsx("~/Downloads/DATA_DOWNLOAD/TABLES/ADS_organized.xlsx", sheet = 2) |>
  as_tibble() |>
  select(state:ads_type, fav_inf, det_inf, remarks) |>
  print()


## third set
ADS_cities3 <- read.xlsx("~/Downloads/DATA_DOWNLOAD/TABLES/ADS_organized.xlsx", sheet = 3) |>
  as_tibble() |>
  select(state:ads_type, remarks) |>
  print()


## Combine into single dataframe
##======================================================
ADS_combo <- bind_rows(ADS_cities1, ADS_cities2, ADS_cities3) 

## Load in & apply zero-point classification model
##======================================================
##timer
start_time <- Sys.time()

bart_classifier <- hf_load_pipeline(
  model_id = "facebook/bart-large-mnli",  
  task = "zero-shot-classification"
)

## first round classification here it looks at if it is related to zoning
stage1_labels <- c("zoning", "regulations-related", "non-zoning-related")

##second round classification here it looks at if it mentions increasing/decreasing zoning
stage2_labels <- c(
  "increasing zoning restrictions",
  "decreasing/excluding zoning restrictions",
  "stating zoning restrictions"
)

## third round classification here it looks at commercial vs residential zoning
stage3_labels <- c(
  "commercial zoning",
  "residential zoning",
  "commercial and residential zoning"
)

## helper functions to run it through the model
classify_stage1 <- function(texts) {
  results <- lapply(texts, function(text) {
    bart_classifier(
      text,
      candidate_labels = stage1_labels,
      hypothesis_template = "This text is about {}"
    )
  })
  
  data.frame(
    text = texts,
    zoning_score = sapply(results, function(x) round(x$scores[1] * 100, 1)),
    regulations_score = sapply(results, function(x) round(x$scores[2] * 100, 1)),
    non_zoning_score = sapply(results, function(x) round(x$scores[3] * 100, 1)),
    predicted_class = sapply(results, function(x) x$labels[which.max(x$scores)])
  )
}


classify_stage2 <- function(texts) {
  results <- lapply(texts, function(text) {
    bart_classifier(
      text,
      candidate_labels = stage2_labels,
      hypothesis_template = "This text is about {}"
    )
  })
  
  data.frame(
    text = texts,
    increasing_score = sapply(results, function(x) round(x$scores[1] * 100, 1)),
    decreasing_score = sapply(results, function(x) round(x$scores[2] * 100, 1)),
    stating_score = sapply(results, function(x) round(x$scores[3] * 100, 1)),
    zoning_subtype = sapply(results, function(x) x$labels[which.max(x$scores)])
  )
}

classify_stage3 <- function(texts) {
  results <- lapply(texts, function(text) {
    bart_classifier(
      text,
      candidate_labels = stage3_labels,
      hypothesis_template = "This text is about {}"
    )
  })
  
  data.frame(
    text = texts,
    commercial_score = sapply(results, function(x) round(x$scores[1] * 100, 1)),
    residential_score = sapply(results, function(x) round(x$scores[2] * 100, 1)),
    both_score = sapply(results, function(x) round(x$scores[3] * 100, 1)),
    zoning_property_type = sapply(results, function(x) x$labels[which.max(x$scores)])
  )
}

##Apply Classification
stage1_fav_inf <- classify_stage1(ADS_combo$fav_inf)
stage1_det_inf <- classify_stage1(ADS_combo$det_inf)
stage1_remarks <- classify_stage1(ADS_combo$remarks)

## Adds columns needed for analysis
#for loop that loops through the 3 newly created data frames and adds city, metro, and holc grade columns
stage1_list <- list(stage1_fav_inf, stage1_det_inf, stage1_remarks)

# Iterate over the list using an index to modify the original data frames
for (i in seq_along(stage1_list)) {
  stage1_list[[i]]$unique_id <- ADS_combo$unique_id
  stage1_list[[i]]$city <- ADS_combo$city
  stage1_list[[i]]$state <- ADS_combo$state
  stage1_list[[i]]$metro <- ADS_combo$metro
  stage1_list[[i]]$holc_grade <- ADS_combo$holc_grade
}

# Assign back the modified data frames
stage1_fav_inf  <- stage1_list[[1]]
stage1_det_inf  <- stage1_list[[2]]
stage1_remarks  <- stage1_list[[3]]

## Apply second round classification 
apply_stage2 <- function(stage1_df) {
  zoning_texts <- stage1_df[stage1_df$predicted_class == "zoning", ]
  if (nrow(zoning_texts) > 0) {
    stage2_results <- classify_stage2(zoning_texts$text)
    zoning_texts$zoning_subtype <- stage2_results$zoning_subtype
    stage1_df[stage1_df$predicted_class == "zoning", "zoning_subtype"] <- zoning_texts$zoning_subtype
  }
  return(stage1_df)
}

stage2_fav_inf <- apply_stage2(stage1_fav_inf)
stage2_det_inf <- apply_stage2(stage1_det_inf)
stage2_remarks  <- apply_stage2(stage1_remarks)

# Apply third round classification
apply_stage3 <- function(stage2_df) {
  zoning_texts <- stage2_df[stage2_df$predicted_class == "zoning", ]
  if (nrow(zoning_texts) > 0) {
    stage3_results <- classify_stage3(zoning_texts$text)
    zoning_texts$zoning_property_type <- stage3_results$zoning_property_type
    stage2_df[stage2_df$predicted_class == "zoning", "zoning_property_type"] <- zoning_texts$zoning_property_type
  }
  return(stage2_df)
}

stage3_fav_inf <- apply_stage3(stage2_fav_inf)
stage3_det_inf <- apply_stage3(stage2_det_inf)
stage3_remarks <- apply_stage3(stage2_remarks)

#for loop that loops through the 3 newly created data frames and adds city, metro, and holc grade columns
stage3_list <- list(stage3_fav_inf, stage3_det_inf, stage3_remarks)

# Iterate over the list using an index to modify the original data frames
for (i in seq_along(stage3_list)) {
  stage3_list[[i]]$unique_id <- ADS_combo$unique_id
  stage3_list[[i]]$city       <- ADS_combo$city
  stage3_list[[i]]$state      <- ADS_combo$state
  stage3_list[[i]]$metro      <- ADS_combo$metro
  stage3_list[[i]]$holc_grade <- ADS_combo$holc_grade
}

# Assign back the modified data frames
stage3_fav_inf  <- stage3_list[[1]]
stage3_det_inf  <- stage3_list[[2]]
stage3_remarks  <- stage3_list[[3]]


## Looks at subgroups and generates counts
##=====================================================
# Count records with zoning mentions in any column
records_with_zoning <- data.frame(
  total_records = nrow(ADS_combo),
  records_with_zoning = sum(
    stage1_fav_inf$predicted_class == "zoning" |
      stage1_det_inf$predicted_class == "zoning" |
      stage1_remarks$predicted_class == "zoning"
  ),
  pct_with_zoning = round(100 * sum(
    stage1_fav_inf$predicted_class == "zoning" |
      stage1_det_inf$predicted_class == "zoning" |
      stage1_remarks$predicted_class == "zoning"
  ) / nrow(ADS_combo), 1),
  fav_inf_zoning = sum(stage1_fav_inf$predicted_class == "zoning"),
  pct_fav_inf_zoning = round(100 * sum(stage1_fav_inf$predicted_class == "zoning") / nrow(ADS_combo), 1),
  det_inf_zoning = sum(stage1_det_inf$predicted_class == "zoning"),
  pct_det_inf_zoning = round(100 * sum(stage1_det_inf$predicted_class == "zoning") / nrow(ADS_combo), 1),
  remarks_zoning = sum(stage1_remarks$predicted_class == "zoning"),
  pct_remarks_zoning = round(100 * sum(stage1_remarks$predicted_class == "zoning") / nrow(ADS_combo), 1)
)

# Filter the stage3 data frames based on zoning mentions
stage3_fav_inf_zoning <- stage3_fav_inf[stage3_fav_inf$predicted_class == "zoning", ]
stage3_det_inf_zoning <- stage3_det_inf[stage3_det_inf$predicted_class == "zoning", ]
stage3_remarks_zoning <- stage3_remarks[stage3_remarks$predicted_class == "zoning", ]

# Combine the data frames to get all zoning rows
zoning_rows <- dplyr::bind_rows(
  stage3_fav_inf_zoning,
  stage3_det_inf_zoning,
  stage3_remarks_zoning
)

# Count total records by city
city_total_counts <- ADS_combo |>
  mutate(city_state = paste(city, state, sep = ", ")) |>
  group_by(city_state) |>
  summarise(total_records = n())

# Count total records by metro
metro_total_counts <- ADS_combo |>
  group_by(metro) |>
  summarise(total_records = n())

# Count total records by HOLC grade
holc_total_counts <- ADS_combo |>
  group_by(holc_grade) |>
  summarise(total_records = n())

## Creates summary statistics based on city, metro and holc grade.
# City summary
city_summary <- zoning_rows |>
  mutate(city_state = paste(city, state, sep = ", ")) |>
  group_by(city_state) |>
  summarise(
    total_zoning = n(),
    # By restriction type
    increasing = sum(zoning_subtype == "increasing zoning restrictions", na.rm = TRUE),
    decreasing = sum(zoning_subtype == "decreasing/excluding zoning restrictions", na.rm = TRUE),
    stating = sum(zoning_subtype == "stating zoning restrictions", na.rm = TRUE),
    # By property type
    commercial = sum(zoning_property_type == "commercial zoning", na.rm = TRUE),
    residential = sum(zoning_property_type == "residential zoning", na.rm = TRUE),
    both = sum(zoning_property_type == "commercial and residential zoning", na.rm = TRUE)
  ) |>
  # Join with total counts to calculate percentages
  left_join(city_total_counts, by = "city_state") |>
  mutate(
    pct_with_zoning = round(100 * total_zoning / total_records, 1),
    # By restriction type percentages (of zoning mentions)
    pct_increasing = round(100 * increasing / total_zoning, 1),
    pct_decreasing = round(100 * decreasing / total_zoning, 1),
    pct_stating = round(100 * stating / total_zoning, 1),
    # By property type percentages (of zoning mentions)
    pct_commercial = round(100 * commercial / total_zoning, 1),
    pct_residential = round(100 * residential / total_zoning, 1),
    pct_both = round(100 * both / total_zoning, 1)
  )

# Metro summary
metro_summary <- zoning_rows |>
  group_by(metro) |>
  summarise(
    total_zoning = n(),
    # By restriction type
    increasing = sum(zoning_subtype == "increasing zoning restrictions", na.rm = TRUE),
    decreasing = sum(zoning_subtype == "decreasing/excluding zoning restrictions", na.rm = TRUE),
    stating = sum(zoning_subtype == "stating zoning restrictions", na.rm = TRUE),
    # By property type
    commercial = sum(zoning_property_type == "commercial zoning", na.rm = TRUE),
    residential = sum(zoning_property_type == "residential zoning", na.rm = TRUE),
    both = sum(zoning_property_type == "commercial and residential zoning", na.rm = TRUE)
  ) |>
  # Join with total counts to calculate percentages
  left_join(metro_total_counts, by = "metro") |>
  mutate(
    pct_with_zoning = round(100 * total_zoning / total_records, 1),
    # By restriction type percentages (of zoning mentions)
    pct_increasing = round(100 * increasing / total_zoning, 1),
    pct_decreasing = round(100 * decreasing / total_zoning, 1),
    pct_stating = round(100 * stating / total_zoning, 1),
    # By property type percentages (of zoning mentions)
    pct_commercial = round(100 * commercial / total_zoning, 1),
    pct_residential = round(100 * residential / total_zoning, 1),
    pct_both = round(100 * both / total_zoning, 1)
  )

# HOLC grade summary
holc_summary <- zoning_rows |>
  group_by(holc_grade) |>
  summarise(
    total_zoning = n(),
    # By restriction type
    increasing = sum(zoning_subtype == "increasing zoning restrictions", na.rm = TRUE),
    decreasing = sum(zoning_subtype == "decreasing/excluding zoning restrictions", na.rm = TRUE),
    stating = sum(zoning_subtype == "stating zoning restrictions", na.rm = TRUE),
    # By property type
    commercial = sum(zoning_property_type == "commercial zoning", na.rm = TRUE),
    residential = sum(zoning_property_type == "residential zoning", na.rm = TRUE),
    both = sum(zoning_property_type == "commercial and residential zoning", na.rm = TRUE)
  ) |>
  # Join with total counts to calculate percentages
  left_join(holc_total_counts, by = "holc_grade") |>
  mutate(
    pct_with_zoning = round(100 * total_zoning / total_records, 1),
    # By restriction type percentages (of zoning mentions)
    pct_increasing = round(100 * increasing / total_zoning, 1),
    pct_decreasing = round(100 * decreasing / total_zoning, 1),
    pct_stating = round(100 * stating / total_zoning, 1),
    # By property type percentages (of zoning mentions)
    pct_commercial = round(100 * commercial / total_zoning, 1),
    pct_residential = round(100 * residential / total_zoning, 1),
    pct_both = round(100 * both / total_zoning, 1)
  )

# Create cross-tabulation of restriction type and property type
zoning_crosstab <- zoning_rows |>
  group_by(zoning_subtype, zoning_property_type) |>
  summarise(count = n(), .groups = "drop") |>
  # Calculate percentages
  mutate(
    pct_of_total = round(100 * count / nrow(zoning_rows), 1)
  ) |>
  pivot_wider(
    names_from = zoning_property_type, 
    values_from = c(count, pct_of_total),
    values_fill = list(count = 0, pct_of_total = 0)
  )

## Creates 2 outputs (Excel & Word Document)
##======================================================
## Save as Excel
# Combines everything together into one dataframe 

detailed_results <- data.frame(
  city = ADS_combo$city,
  metro = ADS_combo$metro,
  holc_grade = ADS_combo$holc_grade,
  fav_inf = ADS_combo$fav_inf,
  det_inf = ADS_combo$det_inf,
  remarks = ADS_combo$remarks,
  # Favorables
  fav_inf_class = stage3_fav_inf$predicted_class,
  fav_inf_zoning_score = stage3_fav_inf$zoning_score,
  fav_inf_subtype = stage3_fav_inf$zoning_subtype, 
  fav_inf_property_type = stage3_fav_inf$zoning_property_type,
  # Detrimental
  det_inf_class = stage3_det_inf$predicted_class,
  det_inf_zoning_score = stage3_det_inf$zoning_score,
  det_inf_subtype = stage3_det_inf$zoning_subtype, 
  det_inf_property_type = stage3_det_inf$zoning_property_type,
  # Remarks
  remarks_class = stage3_remarks$predicted_class,
  remarks_zoning_score = stage3_remarks$zoning_score,
  remarks_subtype = stage3_remarks$zoning_subtype,
  remarks_property_type = stage3_remarks$zoning_property_type
)

# Save all results to Excel
write_xlsx(list(
  "Detailed_Classifications" = detailed_results,
  "Records_with_Zoning" = records_with_zoning,
  "City_Summary" = city_summary,
  "Metro_Summary" = metro_summary,
  "HOLC_Summary" = holc_summary,
  "Zoning_Crosstab" = zoning_crosstab
), "zoning_analysis_results7.xlsx")

## Create and save Word document
doc <- read_docx()
doc <- doc |>
  body_add_par("Zoning Analysis Results", style = "heading 1")
doc <- doc |>
  body_add_par("Overall Zoning Mentions Summary", style = "heading 2") |>
  body_add_table(records_with_zoning, style = "table_template")
doc <- doc |>
  body_add_par("Zoning Mentions by City", style = "heading 2") |>
  body_add_table(city_summary,  style = "table_template")
doc <- doc |>
  body_add_par("Zoning Mentions by Metro Area", style = "heading 2") |>
  body_add_table(metro_summary,  style = "table_template")
doc <- doc |>
  body_add_par("Zoning Mentions by HOLC Grade", style = "heading 2") |>
  body_add_table(holc_summary,  style = "table_template")
doc <- doc |>
  body_add_par("Cross-tabulation of Zoning Types", style = "heading 2") |>
  body_add_table(zoning_crosstab,  style = "table_template")

# Save Word document
print(doc, target = "zoning_analysis_results7.docx")

#End timer
end_time <- Sys.time()
elapsed_time <- end_time - start_time
print(paste("Total execution time:", round(elapsed_time, 2), "seconds"))