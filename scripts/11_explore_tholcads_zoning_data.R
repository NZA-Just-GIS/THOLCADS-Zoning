#########################################################################
#########################################################################
###                                                                   ###
###           Explore mentions of Zoning in ADS
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

##Apply Classification
stage1_fav_inf <- classify_stage1(ADS_combo$fav_inf)
stage1_det_inf <- classify_stage1(ADS_combo$det_inf)
stage1_remarks <- classify_stage1(ADS_combo$remarks)

## Adds columns needed for analysis
stage1_fav_inf$city <- ADS_combo$city
stage1_fav_inf$metro <- ADS_combo$metro
stage1_fav_inf$holc_grade <- ADS_combo$holc_grade

## Apply second round classification 
zoning_texts <- stage1_fav_inf[stage1_fav_inf$predicted_class == "zoning", ]
stage2_results <- classify_stage2(zoning_texts$text)
zoning_texts$zoning_subtype <- stage2_results$zoning_subtype


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
  fav_inf_zoning = sum(stage1_fav_inf$predicted_class == "zoning"),
  det_inf_zoning = sum(stage1_det_inf$predicted_class == "zoning"),
  remarks_zoning = sum(stage1_remarks$predicted_class == "zoning")
)

## Creates summary statistics based on city, metro and holc grade.
# Currently its just copy and paste but this can probably be written as a for loop
# City summary
city_summary <- zoning_texts |>
  group_by(city) |>
  summarise(
    total_zoning = n(),
    increasing = sum(zoning_subtype == "increasing zoning restrictions"),
    decreasing = sum(zoning_subtype == "decreasing/excluding zoning restrictions"),
    stating = sum(zoning_subtype == "stating zoning restrictions")
  )

# Metro summary
metro_summary <- zoning_texts |>
  group_by(metro) |>
  summarise(
    total_zoning = n(),
    increasing = sum(zoning_subtype == "increasing zoning restrictions"),
    decreasing = sum(zoning_subtype == "decreasing/excluding zoning restrictions"),
    stating = sum(zoning_subtype == "stating zoning restrictions")
  )

# HOLC grade summary
holc_summary <- zoning_texts |>
  group_by(holc_grade) |>
  summarise(
    total_zoning = n(),
    increasing = sum(zoning_subtype == "increasing zoning restrictions"),
    decreasing = sum(zoning_subtype == "decreasing/excluding zoning restrictions"),
    stating = sum(zoning_subtype == "stating zoning restrictions")
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
  fav_inf_class = stage1_fav_inf$predicted_class,
  fav_inf_zoning_score = stage1_fav_inf$zoning_score,
  det_inf_class = stage1_det_inf$predicted_class,
  det_inf_zoning_score = stage1_det_inf$zoning_score,
  remarks_class = stage1_remarks$predicted_class,
  remarks_zoning_score = stage1_remarks$zoning_score
)

# Save all results to Excel
write_xlsx(list(
  "Detailed_Classifications" = detailed_results,
  "Records_with_Zoning" = records_with_zoning,
  "City_Summary" = city_summary,
  "Metro_Summary" = metro_summary,
  "HOLC_Summary" = holc_summary
), "zoning_analysis_results4.xlsx")

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

# Save Word document
print(doc, target = "zoning_analysis_results4.docx")

#End timer
end_time <- Sys.time()
elapsed_time <- end_time - start_time
print(paste("Total execution time:", round(elapsed_time, 2), "seconds"))