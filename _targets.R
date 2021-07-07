library("targets")
source(here::here("R","data.R"))
source(here::here("R","data_transforms.R"))
source(here::here("R","utility.R"))
source(here::here("R","descriptives.R"))
options(tidyverse.quiet = TRUE)
tar_option_set(packages = c("tidyverse", "multcomp","gt"))

list(
  tar_target(
    popweights_file_name,
    here::here("data","populationweights2016.rds"),
    format = "file"
  ),
  tar_target(
    survey_file_name,
    here::here("data","mmtalent_df.rds"),
    format = "file"
  ),
  tar_target(
    popweights_df,
    readRDS(popweights_file_name)
  ),
  tar_target(
    survey_df,
    readRDS(survey_file_name)
  ),
  tar_target(
    weights,
    find_population_weights(survey_df, popweights_df)
  ),
  tar_target(
    mmtalent,
    prepare_data(survey_df, weights)
  ),
  tar_target(
    descriptive_rows_df,
    descriptive_table_rows(mmtalent)
  ),
  tar_target(
    background_balance_rows_df,
    background_balance_rows(mmtalent)
  ),
  tar_target(
    survey_balance_rows_df,
    survey_balance_rows(mmtalent)
  ),
  tar_target(
    survey_balance_rows_timing_df,
    survey_balance_rows_timing(mmtalent)
  ),
  tar_target(
    descriptive_table_formatted,
    format_descriptive_table(descriptive_rows_df)
  ),
  tar_target(
    background_balance_formatted,
    format_background_balance_table(background_balance_rows_df)
  ),
  tar_target(
    survey_balance_formatted,
    format_survey_balance_table(survey_balance_rows_df)
  ),
  tar_target(
    survey_balance_formatted_timing,
    format_survey_balance_table_timing(survey_balance_rows_timing_df)
  )
)


