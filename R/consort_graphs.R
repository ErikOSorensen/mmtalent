library("tidyverse")
library("ggconsort")
library("haven")
library("sjlabelled")

mmtalent_df <- haven::read_dta(here::here("data","mmtalent_df.dta")) |>
  mutate(treatment = sjlabelled::as_label(treatment))


cohorts <- mmtalent_df |>
  cohort_start("Responded to invitation") |>
  cohort_define(
    consented = .full |> filter(completion_state != 1),
    demographics_ok = consented |> filter(completion_state !=2),
    randomized = demographics_ok |> filter(completion_state !=3),
    ExAnteI = randomized |> filter(treatment=="ExAnteImpersonal"),
    ExAnteP = randomized |> filter(treatment=="ExAntePersonal"),
    ExPostI = randomized |> filter(treatment=="ExPostImpersonal"),
    ExPostP = randomized |> filter(treatment=="ExPostPersonal"),
    excluded1 = anti_join(.full, consented, by="observationid"),
    excluded2 = anti_join(consented, demographics_ok, by="observationid"),
    excluded3 = anti_join(demographics_ok, randomized, by="observationid"),
    cExAnteI = ExAnteI |> filter(completion_state==5),
    cExAnteP = ExAnteP |> filter(completion_state==5),
    cExPostI = ExPostI |> filter(completion_state==5),
    cExPostP = ExPostP |> filter(completion_state==5),
    ExAnteIex = anti_join(ExAnteI, cExAnteI, by="observationid"),
    ExAntePex = anti_join(ExAnteP, cExAnteP, by="observationid"),
    ExPostIex = anti_join(ExPostI, cExPostI, by="observationid"),
    ExPostPex = anti_join(ExPostP, cExPostP, by="observationid")
  ) |>
  cohort_label(
    consented = "Consented",
    demographics_ok = "Completed demographic survey",
    randomized = "Allocated to treatment",
    ExAnteI = "Treatment EAI",
    ExAnteP = "Treatment EAP",
    ExPostI = "Treatment EPI",
    ExPostP = "Treatment EPP",
    excluded1 = "Did not consent",
    excluded2 = "Did not complete demographics form",
    excluded3 = "No available demographic quota",
    cExAnteI = "EAI for analysis",
    cExAnteP = "EAP for analysis",
    cExPostI = "EPI for analysis",
    cExPostP = "EPP for analysis",
    ExAnteIex = "Did not complete",
    ExAntePex = "Did not complete",
    ExPostIex = "Did not complete",
    ExPostPex = "Did not complete"
  )

study_cohorts <- cohorts |>
  consort_box_add("full", 0, 50, cohort_count_adorn(cohorts, .full)) |>
  consort_box_add("exclusions", 40,47, glue::glue(
    '{cohort_count_adorn(cohorts, excluded1)}'
  )) |>
  consort_box_add("consented", 0, 44, cohort_count_adorn(cohorts, consented)) |>
  consort_box_add("demographics_ok", 0, 42, cohort_count_adorn(cohorts, demographics_ok)) |>
  consort_box_add("randomized", 0, 38, cohort_count_adorn(cohorts, randomized)) |>
  consort_box_add(
    "ExAnteI", -36, 34, glue::glue(
    '{cohort_count_adorn(cohorts, ExAnteI)}<br>
    • {cohort_count_adorn(cohorts, ExAnteIex)}'
    )) |>
  consort_box_add(
    "ExAnteP", -12, 34, glue::glue(
      '{cohort_count_adorn(cohorts, ExAnteP)}<br>
    • {cohort_count_adorn(cohorts, ExAntePex)}'
    )) |>
  consort_box_add(
    "ExPostI", 12, 34, glue::glue(
      '{cohort_count_adorn(cohorts, ExPostI)}<br>
    • {cohort_count_adorn(cohorts, ExPostIex)}'
    )) |>
  consort_box_add(
    "ExPostP", 36, 34, glue::glue(
      '{cohort_count_adorn(cohorts, ExPostP)}<br>
    • {cohort_count_adorn(cohorts, ExPostPex)}'
    )) |>
  consort_box_add("cExAnteI", -36, 30, cohort_count_adorn(cohorts, cExAnteI)) |>
  consort_box_add("cExAnteP", -12, 30, cohort_count_adorn(cohorts, cExAnteP)) |>
  consort_box_add("cExPostI", 12, 30, cohort_count_adorn(cohorts, cExPostI)) |>
  consort_box_add("cExPostP", 36, 30, cohort_count_adorn(cohorts, cExPostP)) |>
  consort_arrow_add("full", "bottorm","consented","top") |>
  consort_arrow_add("consented","bottom","demographics_ok","top") |>
  consort_arrow_add("demographics_ok","bottom","randomized","top") |>
  consort_arrow_add("ExAnteI","bottom","cExAnteI","top") |>
  consort_arrow_add("ExAnteP","bottom","cExAnteP","top") |>
  consort_arrow_add("ExPostI","bottom","cExPostI","top") |>
  consort_arrow_add("ExPostP","bottom","cExPostP","top") |>
  consort_line_add(start_x=-36, start_y=36, end_x=36, end_y=36) |>
  consort_arrow_add(start_x=-36, start_y=36, end_x=-36,end_y=34) |>
  consort_arrow_add(start_x=-12, start_y=36, end_x=-12,end_y=34) |>
  consort_arrow_add(start_x=12, start_y=36, end_x=12,end_y=34) |>
  consort_arrow_add(start_x=36, start_y=36, end_x=36,end_y=34) |>
  consort_arrow_add(start_x=0, start_y=38, end_x=0, end_y=36) |>
  consort_arrow_add(start_x=0, start_y=47, end_x=31, end_y=47) |>
  consort_box_add("excluded2", 40, 43, cohort_count_adorn(cohorts, excluded2)) |>
  consort_arrow_add(start_x=0, start_y=43, end_x=24, end_y=43) |>
  consort_box_add("excluded3", 40, 40, cohort_count_adorn(cohorts, excluded3)) |>
  consort_arrow_add(start_x=0, start_y=40, end_x=25, end_y=40)





study_cohorts |>
  ggplot() +
  geom_consort() +
  theme_consort(margin_h = 8, margin_v=1)
ggsave("consort_graph.pdf", width=30, height=24, units="cm")
