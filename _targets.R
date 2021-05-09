library("targets")
require("tidyverse")
require("multcomp")
source(here::here("R","data.R"))
source(here::here("R","data_transforms.R"))

options(tidyverse.quiet = TRUE)

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
  )
)


