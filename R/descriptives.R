# One master function calls several smaller ones, each of these produce a number
# of lines with 5 elements: 1) Name (chr), 2) Mean (raw), 3) SD (raw), 4) Mean (weighted), SD(weighted)
# Will determine later if I can make standard errors for these


descriptive_table_rows <- function(df) {
  gender_rows <- summarize_gender(df)
  age_rows <- summarize_age(df)
  region_rows <- summarize_region(df)
  education_rows <- summarize_education(df)
  income_rows <- summarize_income(df)
  political_rows <- summarize_political(df)
  all_rows <- bind_rows(gender_rows, age_rows, region_rows,
                        education_rows, income_rows, political_rows)
  all_rows
}

summarize_age <- function(df) {
  df %>%
    summarize(mean_raw = mean(age, na.rm=TRUE),
              SD_raw = sd(age, na.rm=TRUE),
              mean_w = weighted.mean(age, w=wgt, na.rm=TRUE),
              SD_w = weighted.sd(age, w=wgt, na.rm=TRUE)) %>%
    mutate(name_col = "Age (years)") %>%
    dplyr::select(name_col, mean_raw, SD_raw, mean_w, SD_w)
  }

summarize_region <- function(df) {
  Northeast <- df %>% summarize(mean_raw = mean(region=="Northeast"), mean_w = weighted.mean(region=="Northeast", wgt)) %>%
    mutate(name_col = "Northeast")
  Midwest <- df %>% summarize(mean_raw = mean(region=="Midwest"), mean_w = weighted.mean(region=="Midwest", wgt)) %>%
    mutate(name_col = "Midwest")
  South <- df %>% summarize(mean_raw = mean(region=="South"), mean_w = weighted.mean(region=="South", wgt)) %>%
    mutate(name_col = "South")
  West <- df %>% summarize(mean_raw = mean(region=="West"), mean_w = weighted.mean(region=="West", wgt)) %>%
    mutate(name_col = "West")
  header <- tibble(name_col = "Census region:", mean_raw=NA, SD_raw=NA, mean_w=NA, SD_w=NA)
  header %>% bind_rows(Northeast, Midwest, South, West)
}

summarize_education <- function(df) {
  header <- tibble(name_col = "Education category:", mean_raw=NA, SD_raw=NA, mean_w=NA, SD_w=NA)
  dft <- df %>% mutate(
    no_high_school = edu_category=="No High School",
    high_school = edu_category=="High School/GED",
    some_college = edu_category %in% c("Associate's Degree", "Some college"),
    bachelor4 = edu_category =="Bachelor",
    graduate = edu_category %in% c("Professional (JD/MD)", "PhD", "Masters")
  )
  no_hs <- dft %>% summarize(mean_raw = mean(no_high_school), mean_w = weighted.mean(no_high_school, wgt)) %>%
    mutate(name_col = "No high school")
  hs <- dft %>% summarize(mean_raw = mean(high_school), mean_w = weighted.mean(high_school, wgt)) %>%
    mutate(name_col = "High School/GED")
  sc <- dft %>% summarize(mean_raw = mean(some_college), mean_w = weighted.mean(some_college, wgt)) %>%
    mutate(name_col = "Some college / ass. degree.")
  b4 <- dft %>% summarize(mean_raw = mean(bachelor4), mean_w = weighted.mean(bachelor4, wgt)) %>%
    mutate(name_col = "Bachelor (4 years)")
  gd <- dft %>% summarize(mean_raw = mean(graduate), mean_w = weighted.mean(graduate, wgt)) %>%
    mutate(name_col = "Graduate degree")
  header %>% bind_rows(no_hs, hs, sc, b4, gd)

}



summarize_gender <- function(df) {
  df %>%
    mutate( female = as.numeric(gender=="female")) %>%
    summarize(mean_raw = mean(female, na.rm=TRUE),
              SD_raw = NA,
              mean_w = weighted.mean(female, w=wgt, na.rm=TRUE),
              SD_w = NA) %>%
    mutate(name_col = "Female (d)") %>%
    dplyr::select(name_col, mean_raw, SD_raw, mean_w, SD_w)
}

summarize_income <- function(df) {
  dft <- df %>% mutate( below_30 = income_category == "Less than 29 999",
                 between_30_60 = income_category =="30k- 59 999",
                 between_60_100 = income_category=="60k - 99 999",
                 between_100_150 = income_category=="100k - 149 999",
                 above_150 = income_category=="150k +")
  with(dft, tibble(name_col = c("Income (Y) category:",
                      "Y<30", "30 leq Y < 60", "60 leq Y 100", "100 leq Y < 150", "Y geq 150"),
         mean_raw = c(NA,
           mean(below_30), mean(between_30_60), mean(between_60_100), mean(between_100_150), mean(above_150)),
         mean_w = c(NA,
                    weighted.mean(below_30, wgt, wgt),
                    weighted.mean(between_30_60, wgt),
                    weighted.mean(between_60_100, wgt),
                    weighted.mean(between_100_150, wgt),
                    weighted.mean(above_150, wgt))))
}

summarize_political <- function(df) {
  df %>%
    summarize(mean_raw = mean(polpref, na.rm=TRUE),
              SD_raw = sd(polpref, na.rm=TRUE),
              mean_w = weighted.mean(polpref, w = wgt, na.rm=TRUE),
              SD_w = weighted.sd(polpref, w=wgt, na.rm=TRUE)) %>%

    mutate(name_col = "Conservative (1-5)") %>%
    dplyr::select(name_col, mean_raw, SD_raw, mean_w, SD_w)
}


format_descriptive_table <- function(rows) {
  rows %>% gt() %>%
    fmt_number(columns = c("mean_raw", "SD_raw","mean_w", "SD_w"), n_sigfig=3) %>%
    fmt_missing(columns = c("mean_raw", "SD_raw","mean_w", "SD_w"), rows = everything(), missing_text = "") %>%
    tab_spanner(
      label = "Raw (quota sampled)",
      columns = c("mean_raw","SD_raw")
    ) %>%
    tab_spanner(
      label = "Weighted",
      columns = c("mean_w", "SD_w")
    ) %>%
    cols_label( mean_raw = "Mean",
                SD_raw = "SD",
                mean_w = "Mean",
                SD_w = "SD",
                name_col = "Outcome")



}


background_balance_rows <- function(df) {
  bt <- df %>% group_by(treatment) %>%
    summarize(Age = weighted.mean(age, wgt, na.rm=TRUE),
           Female = weighted.mean(gender=="female", wgt, na.rm=TRUE),
           Left = weighted.mean(left, wgt, na.rm=TRUE),
           `High education`= weighted.mean(high_edu, wgt, na.rm=TRUE),
           `High income` = weighted.mean(high_income, wgt, na.rm=TRUE),
           `Number of observations` = n())
  bt.T <- as.data.frame(as.matrix(t(bt[,-1])))
  colnames(bt.T) <- as.character(bt$treatment)
  rwn <- rownames(bt.T)
  bt.T <- bt.T %>% mutate(Outcome = rwn) %>%
    dplyr::select(Outcome, `Ex Ante Impersonal`, `Ex Ante Personal`, `Ex Post Impersonal`, `Ex Post Personal`)
  bt.T
}

format_background_balance_table <- function(rows) {
  rows %>% gt() %>%
    fmt_number(columns = c("Ex Ante Impersonal", "Ex Ante Personal",
                           "Ex Post Impersonal", "Ex Post Personal"),
               n_sigfig=3) %>%
    tab_spanner(
      label = "Ex Ante",
      columns = c("Ex Ante Impersonal", "Ex Ante Personal")) %>%
    tab_spanner(
      label = "Ex Post",
      columns = c("Ex Post Impersonal", "Ex Post Personal")
    ) %>%
    cols_label( "Ex Ante Impersonal" = "Impersonal",
                "Ex Ante Personal" = "Personal",
                "Ex Post Impersonal" = "Impersonal",
                "Ex Post Personal" = "Personal")
}


survey_balance_rows <- function(df) {
  at <- df %>% group_by(treatment) %>%
    summarize( "Luck (unfair)" = weighted.mean(luck_fair, wgt, na.rm=TRUE),
               "Talent (unfair)" = weighted.mean(talent_fair, wgt, na.rm=TRUE),
               "Effort (unfair)" = weighted.mean(effort_fair, wgt, na.rm=TRUE),
               "Luck (control)" = weighted.mean(luck_control, wgt, na.rm=TRUE),
               "Talent (control)" = weighted.mean(talent_control, wgt, na.rm=TRUE),
               "Effort (control)" = weighted.mean(effort_control, wgt, na.rm=TRUE),
               "Should society aim to equalize incomes" = weighted.mean(redist_pref, wgt, na.rm=TRUE))
  at.T <- as.data.frame(as.matrix(t(at[,-1])))
  colnames(at.T) <- as.character(at$treatment)
  rwn <- rownames(at.T)
  at.T <- at.T %>% mutate(Outcome = rwn) %>%
    dplyr::select(Outcome, `Ex Ante Impersonal`, `Ex Ante Personal`, `Ex Post Impersonal`, `Ex Post Personal`)

  pvals <- tibble(Outcome = c("Luck (unfair)",
                               "Talent (unfair)",
                               "Effort (unfair)",
                               "Luck (control)",
                               "Talent (control)",
                               "Effort (control)",
                               "Should society aim to equalize incomes"),
                   "P-value (F)" = c(summary(aov(df$luck_fair~df$treatment, weights=df$wgt))[[1]][["Pr(>F)"]][1],
                                     summary(aov(df$talent_fair~df$treatment, weights=df$wgt))[[1]][["Pr(>F)"]][1],
                                     summary(aov(df$effort_fair~df$treatment, weights=df$wgt))[[1]][["Pr(>F)"]][1],
                                     summary(aov(df$luck_control~df$treatment, weights=df$wgt))[[1]][["Pr(>F)"]][1],
                                     summary(aov(df$talent_control~df$treatment, weights=df$wgt))[[1]][["Pr(>F)"]][1],
                                     summary(aov(df$effort_control~df$treatment, weights=df$wgt))[[1]][["Pr(>F)"]][1],
                                     summary(aov(df$redist_pref~df$treatment, weights=df$wgt))[[1]][["Pr(>F)"]][1])
                                     )
  at.T %>% left_join(pvals, by="Outcome")
}


format_survey_balance_table <- function(rows) {
  rows %>% gt() %>%
    fmt_number(columns = c("Ex Ante Impersonal", "Ex Ante Personal",
                           "Ex Post Impersonal", "Ex Post Personal"),
               n_sigfig = 3) %>%
    fmt_number(columns = c("P-value (F)"), decimals = 3) %>%
    tab_spanner(
      label = "Ex Ante",
      columns = c("Ex Ante Impersonal", "Ex Ante Personal")) %>%
    tab_spanner(
      label = "Ex Post",
      columns = c("Ex Post Impersonal", "Ex Post Personal")
    ) %>%
    cols_label( "Ex Ante Impersonal" = "Impersonal",
                "Ex Ante Personal" = "Personal",
                "Ex Post Impersonal" = "Impersonal",
                "Ex Post Personal" = "Personal")
}
