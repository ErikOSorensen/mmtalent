library("targets")
library("tarchetypes")
library("renv")
library("visNetwork")
source(here::here("R", "data_transforms.R"))
source(here::here("R", "utility.R"))
source(here::here("R", "descriptives.R"))
source(here::here("R", "survey_functions.R"))
source(here::here("R", "experiment_functions.R"))
source(here::here("R", "consort_graphs.R"))
options(tidyverse.quiet = TRUE)
tar_option_set(
  packages = c("tidyverse", "multcomp", "gt", "consort", "sjlabelled","broom","estimatr")
)


list(
  tar_target(
    census_description_file_2017,
    here::here("raw-data", "sc-est2017-alldata6.csv"),
    format = "file"
  ),
  tar_target(
    survey_file_name,
    here::here("data", "mmtalent_df.dta"),
    format = "file"
  ),
  tar_target(
    educational_attainment_file_name,
    here::here("raw-data", "table-1-1.xlsx"),
    format = "file"
  ),
  tar_target(
    income_distribution_file_name,
    here::here("raw-data", "finc07.xls"),
    format = "file"
  ),
  tar_target(survey_df,
             read_dta(survey_file_name)),
  tar_target(mmtalent,
             prepare_data(survey_df)),
  tar_target(
    descriptive_table_gt,
    descriptive_table(
      survey_df,
      income_distribution_file_name,
      educational_attainment_file_name,
      census_description_file_2017
    )
  ),
  tar_target(
    background_balance_rows_df,
    background_balance_rows(mmtalent)
  ),
  tar_target(survey_balance_rows_df,
             survey_balance_rows(mmtalent)),
  tar_target(
    survey_balance_rows_timing_df,
    survey_balance_rows_timing(mmtalent)
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
  ),
  tar_target(
    survey_regressions_list,
    survey_regressions(subjectives_dt)
  ),
  tar_target(
    subjectives_dt,
    long_attitudes(mmtalent) |> subjectives()
  ),
  tar_target(
    implemented_inequality_list,
    implemented_inequality(mmtalent)
  ),
  tar_target(
    implemented_inequality_heterogeneity_list,
    implemented_inequality_heterogeneity(mmtalent)
  ),
  tar_target(
    histogram_distribution_graph,
    histogram_distributions(mmtalent)
  ),
  tar_target(
    average_distributions_graph,
    average_distributions(mmtalent)
  ),
  tar_target(
    survey_heterogeneity,
    survey_heterogeneity_figure(subjectives_dt)
  ),
  tar_target(
    inequality_heterogeneity,
    inequality_heterogeneity_graph(mmtalent)
  ),
  tar_target(extreme_shares_graph,
             extreme_shares(mmtalent)),
  tar_target(consort_diagram,
             mmtalent_consort(survey_df)),
  tar_render(experiment_results, here::here("experiment-results.Rmd")),
  tar_render(survey_results, here::here("survey-results.Rmd")),
  tar_render(descriptive_balance_tables, here::here("descriptive_balance_tables.Rmd"))
)
