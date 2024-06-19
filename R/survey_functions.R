long_attitudes <- function(mmtalent) {
  mmtalent |>
    mutate(AL=luck_fair,
           AT=talent_fair,
           AE=effort_fair,
           CL=luck_control,
           CT=talent_control,
           CE=effort_control,
           id = row_number()) |>
    dplyr::select(AL, AT, AE, CL, CT, CE, treatment, gender, age, age_high, left, high_income, income_category,
                  high_edu, edu_category, id, wgt) |>
    gather(key = "outcome", value="value", AL, AT, AE, CL, CT, CE) |>
    mutate(outcometype = factor( ifelse(outcome %in% c("AL","AT", "AE"),
                                        "Unfair inequality",
                                        "Belief about individual control"),
                                 levels = c("Unfair inequality","Belief about individual control" )),
           f = fct_collapse(outcome,
                            Luck = c("AL","CL"),
                            Talent = c("AT","CT"),
                            Effort = c("AE", "CE")),
           fo = fct_relevel(f, c("Luck", "Talent", "Effort")))
}

survey_bars <- function(mmtalent_long, gtitle = NULL, numobs = TRUE, ylimits = c(0,10)) {
  g <- mmtalent_long |>
  group_by(outcometype, fo, outcome) |>
    summarize(m = weighted.mean(value, wgt),
              se = weighted.se(value, wgt)) |>
    ggplot(aes(x=fo, y=m)) +
    geom_col() + facet_wrap(.~ outcometype) +
    geom_errorbar(aes(ymin=m-se, ymax=m+se), width=0.2) +
    labs(x = element_blank(),
         y = "Mean outcome \u00B1 s.e.m.",
         title = gtitle) +
    theme_minimal() +
    coord_cartesian(ylim = ylimits) +
    theme(plot.title.position = "plot")
  if (numobs) {
    g <- g + labs(caption = sprintf("n=%d", nrow(mmtalent_long)/6))
  }
  g
}

survey_bars_by_subgroup <- function(mmtalent_long) {
  list(
    mmtalent_long |> filter( age < spatstat.geom::weighted.median(age,wgt, na.rm=TRUE)) |>
      survey_bars(gtitle="Young", numobs = FALSE, ylimits = c(0,8)),
    mmtalent_long |> filter( age >= spatstat.geom::weighted.median(age,wgt, na.rm=TRUE)) |>
      survey_bars(gtitle="Old", numobs = FALSE, ylimits = c(0,8)),
    mmtalent_long |> filter(left==TRUE) |> survey_bars(gtitle="Left", numobs = FALSE, ylimits = c(0,8)),
    mmtalent_long |> filter(left==FALSE) |> survey_bars(gtitle="Right", numobs = FALSE, ylimits = c(0,8)),
    mmtalent_long |> filter(high_income==FALSE) |> survey_bars(gtitle="Low income", numobs = FALSE, ylimits = c(0,8)),
    mmtalent_long |> filter(high_income==TRUE) |> survey_bars(gtitle="High income", numobs = FALSE, ylimits = c(0,8)),
    mmtalent_long |> filter(high_edu==FALSE) |> survey_bars(gtitle="Low education", numobs = FALSE, ylimits = c(0,8)),
    mmtalent_long |> filter(high_edu==TRUE) |> survey_bars(gtitle="High education", numobs = FALSE, ylimits = c(0,8)),
    mmtalent_long |> filter(gender=="female") |> survey_bars(gtitle="Female", numobs = FALSE, ylimits = c(0,8)),
    mmtalent_long |> filter(gender=="male") |> survey_bars(gtitle="Male", numobs = FALSE, ylimits = c(0,8))
  )
}

survey_bars_by_subgroup_cross <- function(mmtalent_long) {
  list(
    mmtalent_long |> filter( age < spatstat.geom::weighted.median(age,wgt, na.rm=TRUE),
                              left==TRUE, high_income==FALSE, high_edu==FALSE, gender=="female") |>
      survey_bars(gtitle="Young/left/low.inc/low.edu/female"),
    mmtalent_long |> filter( age < spatstat.geom::weighted.median(age,wgt, na.rm=TRUE),
                              left==TRUE, high_income==FALSE, high_edu==FALSE, gender=="male") |>
      survey_bars(gtitle="Young/left/low.inc/low.edu/male"),
    mmtalent_long |> filter( age < spatstat.geom::weighted.median(age,wgt, na.rm=TRUE),
                              left==TRUE, high_income==FALSE, high_edu==TRUE, gender=="female") |>
      survey_bars(gtitle="Young/left/low.inc/high.edu/female"),
    mmtalent_long |> filter( age < spatstat.geom::weighted.median(age,wgt, na.rm=TRUE),
                              left==TRUE, high_income==FALSE, high_edu==TRUE, gender=="male") |>
      survey_bars(gtitle="Young/left/low.inc/high.edu/male"),

    mmtalent_long |> filter( age < spatstat.geom::weighted.median(age,wgt, na.rm=TRUE),
                              left==TRUE, high_income==TRUE, high_edu==FALSE, gender=="female") |>
      survey_bars(gtitle="Young/left/high.inc/low.edu/female"),
    mmtalent_long |> filter( age < spatstat.geom::weighted.median(age,wgt, na.rm=TRUE),
                              left==TRUE, high_income==TRUE, high_edu==FALSE, gender=="male") |>
      survey_bars(gtitle="Young/left/high.inc/low.edu/male"),
    mmtalent_long |> filter( age < spatstat.geom::weighted.median(age,wgt, na.rm=TRUE),
                              left==TRUE, high_income==TRUE, high_edu==TRUE, gender=="female") |>
      survey_bars(gtitle="Young/left/high.inc/high.edu/female"),
    mmtalent_long |> filter( age < spatstat.geom::weighted.median(age,wgt, na.rm=TRUE),
                              left==TRUE, high_income==TRUE, high_edu==TRUE, gender=="male") |>
      survey_bars(gtitle="Young/left/high.inc/high.edu/male"),

    mmtalent_long |> filter( age < spatstat.geom::weighted.median(age,wgt, na.rm=TRUE),
                              left==FALSE, high_income==FALSE, high_edu==FALSE, gender=="female") |>
      survey_bars(gtitle="Young/right/low.inc/low.edu/female"),
    mmtalent_long |> filter( age < spatstat.geom::weighted.median(age,wgt, na.rm=TRUE),
                              left==FALSE, high_income==FALSE, high_edu==FALSE, gender=="male") |>
      survey_bars(gtitle="Young/right/low.inc/low.edu/male"),
    mmtalent_long |> filter( age < spatstat.geom::weighted.median(age,wgt, na.rm=TRUE),
                              left==FALSE, high_income==FALSE, high_edu==TRUE, gender=="female") |>
      survey_bars(gtitle="Young/right/low.inc/high.edu/female"),
    mmtalent_long |> filter( age < spatstat.geom::weighted.median(age,wgt, na.rm=TRUE),
                              left==FALSE, high_income==FALSE, high_edu==TRUE, gender=="male") |>
      survey_bars(gtitle="Young/right/low.inc/high.edu/male"),

    mmtalent_long |> filter( age < spatstat.geom::weighted.median(age,wgt, na.rm=TRUE),
                              left==FALSE, high_income==TRUE, high_edu==FALSE, gender=="female") |>
      survey_bars(gtitle="Young/right/high.inc/low.edu/female"),
    mmtalent_long |> filter( age < spatstat.geom::weighted.median(age,wgt, na.rm=TRUE),
                              left==FALSE, high_income==TRUE, high_edu==FALSE, gender=="male") |>
      survey_bars(gtitle="Young/right/high.inc/low.edu/male"),
    mmtalent_long |> filter( age < spatstat.geom::weighted.median(age,wgt, na.rm=TRUE),
                              left==FALSE, high_income==TRUE, high_edu==TRUE, gender=="female") |>
      survey_bars(gtitle="Young/right/high.inc/high.edu/female"),
    mmtalent_long |> filter( age < spatstat.geom::weighted.median(age,wgt, na.rm=TRUE),
                              left==FALSE, high_income==TRUE, high_edu==TRUE, gender=="male") |>
      survey_bars(gtitle="Young/right/high.inc/high.edu/male"),




    mmtalent_long |> filter( age >= spatstat.geom::weighted.median(age,wgt, na.rm=TRUE),
                              left==TRUE, high_income==FALSE, high_edu==FALSE, gender=="female") |>
      survey_bars(gtitle="Old/left/low.inc/low.edu/female"),
    mmtalent_long |> filter( age >= spatstat.geom::weighted.median(age,wgt, na.rm=TRUE),
                              left==TRUE, high_income==FALSE, high_edu==FALSE, gender=="male") |>
      survey_bars(gtitle="Old/left/low.inc/low.edu/male"),
    mmtalent_long |> filter( age  >=  spatstat.geom::weighted.median(age,wgt, na.rm=TRUE),
                              left==TRUE, high_income==FALSE, high_edu==TRUE, gender=="female") |>
      survey_bars(gtitle="Old/left/low.inc/high.edu/female"),
    mmtalent_long |> filter( age >= spatstat.geom::weighted.median(age,wgt, na.rm=TRUE),
                              left==TRUE, high_income==FALSE, high_edu==TRUE, gender=="male") |>
      survey_bars(gtitle="Old/left/low.inc/high.edu/male"),

    mmtalent_long |> filter( age >= spatstat.geom::weighted.median(age,wgt, na.rm=TRUE),
                              left==TRUE, high_income==TRUE, high_edu==FALSE, gender=="female") |>
      survey_bars(gtitle="Old/left/high.inc/low.edu/female"),
    mmtalent_long |> filter( age >= spatstat.geom::weighted.median(age,wgt, na.rm=TRUE),
                              left==TRUE, high_income==TRUE, high_edu==FALSE, gender=="male") |>
      survey_bars(gtitle="Old/left/high.inc/low.edu/male"),
    mmtalent_long |> filter( age >= spatstat.geom::weighted.median(age,wgt, na.rm=TRUE),
                              left==TRUE, high_income==TRUE, high_edu==TRUE, gender=="female") |>
      survey_bars(gtitle="Old/left/high.inc/high.edu/female"),
    mmtalent_long |> filter( age >= spatstat.geom::weighted.median(age,wgt, na.rm=TRUE),
                              left==TRUE, high_income==TRUE, high_edu==TRUE, gender=="male") |>
      survey_bars(gtitle="Old/left/high.inc/high.edu/male"),

    mmtalent_long |> filter( age >= spatstat.geom::weighted.median(age,wgt, na.rm=TRUE),
                              left==FALSE, high_income==FALSE, high_edu==FALSE, gender=="female") |>
      survey_bars(gtitle="Old/right/low.inc/low.edu/female"),
    mmtalent_long |> filter( age >= spatstat.geom::weighted.median(age,wgt, na.rm=TRUE),
                              left==FALSE, high_income==FALSE, high_edu==FALSE, gender=="male") |>
      survey_bars(gtitle="Old/right/low.inc/low.edu/male"),
    mmtalent_long |> filter( age >= spatstat.geom::weighted.median(age,wgt, na.rm=TRUE),
                              left==FALSE, high_income==FALSE, high_edu==TRUE, gender=="female") |>
      survey_bars(gtitle="Old/right/low.inc/high.edu/female"),
    mmtalent_long |> filter( age >= spatstat.geom::weighted.median(age,wgt, na.rm=TRUE),
                              left==FALSE, high_income==FALSE, high_edu==TRUE, gender=="male") |>
      survey_bars(gtitle="Old/right/low.inc/high.edu/male"),

    mmtalent_long |> filter( age >= spatstat.geom::weighted.median(age,wgt, na.rm=TRUE),
                              left==FALSE, high_income==TRUE, high_edu==FALSE, gender=="female") |>
      survey_bars(gtitle="Old/right/high.inc/low.edu/female"),
    mmtalent_long |> filter( age >= spatstat.geom::weighted.median(age,wgt, na.rm=TRUE),
                              left==FALSE, high_income==TRUE, high_edu==FALSE, gender=="male") |>
      survey_bars(gtitle="Old/right/high.inc/low.edu/male"),
    mmtalent_long |> filter( age >= spatstat.geom::weighted.median(age,wgt, na.rm=TRUE),
                              left==FALSE, high_income==TRUE, high_edu==TRUE, gender=="female") |>
      survey_bars(gtitle="Old/right/high.inc/high.edu/female"),
    mmtalent_long |> filter( age >= spatstat.geom::weighted.median(age,wgt, na.rm=TRUE),
                              left==FALSE, high_income==TRUE, high_edu==TRUE, gender=="male") |>
      survey_bars(gtitle="Old/right/high.inc/high.edu/male")

  )



}


subjectives <- function(mmtalent_long) {
  mmtalent_long |>
  pivot_wider(id_cols=c(id,fo, treatment, gender, age, age_high, left,
                        high_income, income_category,
                        high_edu, edu_category, wgt),
              values_from = value,
              names_from=outcometype) |>
    mutate(Luck = as.numeric(fo=="Luck"),
           Effort = as.numeric(fo=="Effort"),
           Talent = as.numeric(fo=="Talent"))
}


survey_regressions <- function(subjective_data) {
  AB1 <- estimatr::lm_robust(`Unfair inequality` ~ Talent + Effort, data=subjective_data, weights=wgt, clusters=id)
  AB2 <- estimatr::lm_robust(`Unfair inequality` ~ Talent + Effort + `Belief about individual control`,
                             data=subjective_data, weights=wgt, clusters=id)
  AB3 <- estimatr::lm_robust( `Unfair inequality`  ~ Talent + Effort + Talent*`Belief about individual control` +
                                Effort*`Belief about individual control` + `Belief about individual control`,
                              data=subjective_data, weights=wgt, clusters=id)
  AB4 <- estimatr::lm_robust( `Unfair inequality`  ~ Talent + Effort + Talent*`Belief about individual control` +
                                Effort*`Belief about individual control` + `Belief about individual control` +
                                age_high + left + high_income + high_edu + as.factor(gender), data=subjective_data, weights=wgt, clusters=id)
  AB5 <- estimatr::lm_robust( `Unfair inequality`  ~ Talent + Effort + `Belief about individual control` +
                                age_high + left + high_income + high_edu + as.factor(gender), data=subjective_data, weights=wgt, clusters=id)

  AB1t <- estimatr::lm_robust(`Unfair inequality` ~ Talent + Effort + treatment, data=subjective_data, weights=wgt, clusters=id)
  AB2t <- estimatr::lm_robust(`Unfair inequality` ~ Talent + Effort + `Belief about individual control` + treatment,
                             data=subjective_data, weights=wgt, clusters=id)
  AB3t <- estimatr::lm_robust( `Unfair inequality`  ~ Talent + Effort + Talent*`Belief about individual control` +
                                Effort*`Belief about individual control` + `Belief about individual control` + treatment,
                              data=subjective_data, weights=wgt, clusters=id)
  AB4t <- estimatr::lm_robust( `Unfair inequality`  ~ Talent + Effort + Talent*`Belief about individual control` +
                                Effort*`Belief about individual control` + `Belief about individual control` +
                                age_high + left + high_income + high_edu + as.factor(gender) + treatment, data=subjective_data, weights=wgt, clusters=id)
  AB5t <- estimatr::lm_robust( `Unfair inequality`  ~ Talent + Effort + `Belief about individual control` +
                                age_high + left + high_income + high_edu + as.factor(gender) + treatment, data=subjective_data, weights=wgt, clusters=id)

  list(without_treatment = list(AB1,AB2,AB3,AB4,AB5),
       with_treatment = list(AB1t,AB2t,AB3t,AB4t,AB5t))
}
