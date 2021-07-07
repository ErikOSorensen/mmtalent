long_attitudes <- function(mmtalent) {
  mmtalent %>%
    mutate(AL=luck_fair,
           AT=talent_fair,
           AE=effort_fair,
           CL=luck_control,
           CT=talent_control,
           CE=effort_control,
           id = row_number()) %>%
    dplyr::select(AL, AT, AE, CL, CT, CE, treatment, gender, age, left, high_income, income_category,
                  high_edu, edu_category, id, wgt) %>%
    gather(key = "outcome", value="value", AL, AT, AE, CL, CT, CE) %>%
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

survey_bars <- function(mmtalent_long, gtitle = NULL) {
  mmtalent_long %>%
  group_by(outcometype, fo, outcome) %>%
    summarize(m = weighted.mean(value, wgt),
              se = weighted.se(value, wgt)) %>%
    ggplot(aes(x=fo, y=m)) +
    geom_col() + facet_wrap(.~ outcometype) +
    geom_errorbar(aes(ymin=m-se, ymax=m+se), width=0.2) +
    labs(x = element_blank(),
         y = "Mean outcome \u00B1 s.e.m.",
         title = gtitle) +
    theme_minimal() +
    coord_cartesian(ylim = c(0, 8)) +
    theme(plot.title.position = "plot")
}

survey_bars_by_subgroup <- function(mmtalent_long) {
  list(
    mmtalent_long %>% filter( age < spatstat.geom::weighted.median(age,wgt, na.rm=TRUE)) %>%
      survey_bars(gtitle="Young"),
    mmtalent_long %>% filter( age >= spatstat.geom::weighted.median(age,wgt, na.rm=TRUE)) %>%
      survey_bars(gtitle="Old"),
    mmtalent_long %>% filter(left==TRUE) %>% survey_bars(gtitle="Left"),
    mmtalent_long %>% filter(left==FALSE) %>% survey_bars(gtitle="Right"),
    mmtalent_long %>% filter(high_income==FALSE) %>% survey_bars(gtitle="Low income"),
    mmtalent_long %>% filter(high_income==TRUE) %>% survey_bars(gtitle="High income"),
    mmtalent_long %>% filter(high_edu==FALSE) %>% survey_bars(gtitle="Low education"),
    mmtalent_long %>% filter(high_edu==TRUE) %>% survey_bars(gtitle="High education"),
    mmtalent_long %>% filter(gender=="female") %>% survey_bars(gtitle="Female"),
    mmtalent_long %>% filter(gender=="male") %>% survey_bars(gtitle="Male")
  )
}




subjectives <- function(mmtalent_long) {
  mmtalent_long %>%
  pivot_wider(id_cols=c(id,fo, treatment, gender, age, left,
                        high_income, income_category,
                        high_edu, edu_category, wgt),
              values_from = value,
              names_from=outcometype) %>%
    mutate(Luck = as.numeric(fo=="Luck"),
           Effort = as.numeric(fo=="Effort"),
           Talent = as.numeric(fo=="Talent"))
}


survey_regressions <- function(subjective_data) {
  AB1 <- estimatr::lm_robust(`Unfair inequality` ~ Talent + Effort, data=subjective_data, weights=wgt)
  AB2 <- estimatr::lm_robust(`Unfair inequality` ~ Talent + Effort + `Belief about individual control`,
                             data=subjective_data, weights=wgt)
  AB3 <- estimatr::lm_robust( `Unfair inequality`  ~ Talent + Effort + Talent*`Belief about individual control` +
                                Effort*`Belief about individual control` + `Belief about individual control`,
                              data=subjective_data, weights=wgt)
  AB4 <- estimatr::lm_robust( `Unfair inequality`  ~ Talent + Effort + Talent*`Belief about individual control` +
                                Effort*`Belief about individual control` + `Belief about individual control` +
                                age + left + high_income + high_edu + as.factor(gender), data=subjective_data, weights=wgt)
  AB5 <- estimatr::lm_robust( `Unfair inequality`  ~ Talent + Effort + `Belief about individual control` +
                                age + left + high_income + high_edu + as.factor(gender), data=subjective_data, weights=wgt)

  AB1t <- estimatr::lm_robust(`Unfair inequality` ~ Talent + Effort + treatment, data=subjective_data, weights=wgt)
  AB2t <- estimatr::lm_robust(`Unfair inequality` ~ Talent + Effort + `Belief about individual control` + treatment,
                             data=subjective_data, weights=wgt)
  AB3t <- estimatr::lm_robust( `Unfair inequality`  ~ Talent + Effort + Talent*`Belief about individual control` +
                                Effort*`Belief about individual control` + `Belief about individual control` + treatment,
                              data=subjective_data, weights=wgt)
  AB4t <- estimatr::lm_robust( `Unfair inequality`  ~ Talent + Effort + Talent*`Belief about individual control` +
                                Effort*`Belief about individual control` + `Belief about individual control` +
                                age + left + high_income + high_edu + as.factor(gender) + treatment, data=subjective_data, weights=wgt)
  AB5t <- estimatr::lm_robust( `Unfair inequality`  ~ Talent + Effort + `Belief about individual control` +
                                age + left + high_income + high_edu + as.factor(gender) + treatment, data=subjective_data, weights=wgt)

  list(without_treatment = list("(1)"=AB1,"(2)"=AB2,"(3)"=AB3, "(4)"=AB4, "(5)"=AB5),
       with_treatment = list("(1)"=AB1t,"(2)"=AB2t,"(3)"=AB3t, "(4)"=AB4t, "(5)"=AB5t))
}
