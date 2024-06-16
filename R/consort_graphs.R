library("tidyverse")
library("consort")
library("haven")
library("sjlabelled")

mmtalent_consort <- function(data) {
  disposition_data(data) |> apply_dispositions()
}

disposition_data <- function(data) {
  data |>
    mutate(
      exclusion1 = case_when(
        completion_state == 1 ~ "Did not consent to terms",
        completion_state == 2 ~ "Did not complete demographics form"
      ),
      exclusion2 = ifelse(completion_state==3, "No available demographic quota",NA),
      exclusion3 = case_when(
        completion_state == 4 & (is.na(effort_control) | is.na(effort_fair) |
                                   is.na(luck_control) | is.na(luck_fair) |
                                   is.na(talent_control) | is.na(talent_fair)) ~ "fairness/control",
        completion_state == 4 & (is.na(polpref) | is.na(redist_pref)) ~ "policy",
        completion_state == 4 & is.na(payment_low_worker) ~ "no decision"),
      demographics = ifelse(completion_state>2, observationid, NA),
      allocated = ifelse(completion_state>3, observationid, NA),
      treatment = as.character(sjlabelled::as_label(treatment)),
      treatment = case_when(
        treatment == "ExAnteImpersonal" ~ "Trt. EAI",
        treatment == "ExAntePersonal" ~ "Trt. EAP",
        treatment == "ExPostImpersonal" ~ "Trt. EPI",
        treatment == "ExPostPersonal" ~ "trt. EPP"
      ),
      final = ifelse(completion_state==5, observationid, NA)) |>
    select(c(observationid, demographics, allocated, treatment, final, exclusion1, exclusion2, exclusion3))
}

apply_dispositions <- function(data) {
  data |> consort_plot(
    orders  = c(observationid = "Responded to invitation",
                exclusion1 = "Excluded from participation",
                demographics = "Available for allocation",
                exclusion2 = "Excluded from allocation",
                treatment = "Allocated to treatment",
                exclusion3 = "Did not complete",
                final = "Final analysis"),
    side_box = c("exclusion1", "exclusion2", "exclusion3"),
    allocation = "treatment")
}

#out <- mmtalent_consort(mmtalent_df)
#cairo_pdf(file = here::here("graphs","consort.pdf"), width=15, height = 7)
#plot(out)
#dev.off()

