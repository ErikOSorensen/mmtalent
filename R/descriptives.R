# One master function calls several smaller ones, each of these produce a number
# of lines with 5 elements: 1) Name (chr), 2) Mean (raw), 3) SD (raw), 4) Mean (weighted), SD(weighted)
# Will determine later if I can make standard errors for these


descriptive_table <- function(df, fname_inc, fname_edu, fname_censusfile) {
  rows <- descriptive_table_rows(df, fname_inc, fname_edu, fname_censusfile)
  format_descriptive_table(rows)
}

format_descriptive_table <- function(rows) {
  rows |> gt() |>
    sub_missing(columns = c("means_census"), rows = everything(), missing_text = "") |>
    fmt_number(columns = c(2:6), rows=c(1,3:16), decimals=3) |>
    fmt_number(columns = c(2:6), rows=2, decimals=1) |>
    fmt_number(columns = c(2:6), rows=17, decimals=0) |>
    cols_label ( name_col = "",
                 mean_available = "Available",
                 mean_attrition = "Attrition",
                 mean_sample = "Sample (unweighted)",
                 mean_weighted = "Sample (weighted)",
                 means_census = "Census") |>
    tab_spanner (
      label = "Allocated to treatment",
      columns = c("mean_attrition","mean_sample","mean_weighted")
    ) |>
    tab_header("Descriptive statistics (averages) compared to census reference") %>%
    tab_row_group(label="Panel B: Not used in quota sampling or weight construction",
                  rows = 7:16) %>%
    tab_row_group(label="Panel A: Variables used in quota sampling and for weights",
                  rows = 1:6)
}

descriptive_table_rows <- function(df, fname_inc, fname_edu, fname_censusfile) {
  gender_rows <- summarize_gender(df, fname_censusfile)
  age_rows <- summarize_age(df, fname_censusfile)
  region_rows <- summarize_region(df, fname_censusfile)
  education_rows <- summarize_education(df, fname_edu)
  income_rows <- summarize_income(df, fname_inc)
  observation_rows <- summarize_observations(df)
  bind_rows(gender_rows, age_rows, region_rows,
            education_rows, income_rows, observation_rows) |>
    dplyr::select(c(name_col, mean_available, mean_attrition, mean_sample, mean_weighted,means_census))
}

summarize_observations <- function(df) {
  o1 <- df |> filter(completion_state>2) |> nrow()
  o2 <- df |> filter(completion_state==4) |> nrow()
  o3 <- df |> filter(completion_state==5) |> nrow()
  o4 <- df |> filter(completion_state==5) |> nrow()
  tribble(~name_col, ~mean_available,~mean_attrition, ~mean_sample,~mean_weighted,
          "Number of observations", o1, o2, o3, o4)
}

summarize_age <- function(df, fname) {
  a1 <- df |>
    filter(completion_state>2) |>
    summarize(mean_available = mean(age)) |>
    mutate(name_col = "Age (years)")
  a2 <- df |>
    filter(completion_state==4) |>
    summarize(mean_attrition = mean(age)) |>
    mutate(name_col = "Age (years)")
  a3 <- gdf |> filter(completion_state==5) |>
    summarize(mean_sample = mean(age))  |>
    mutate(name_col = "Age (years)")
  a4 <- gdf |> filter(completion_state==5) |>
    summarize(mean_weighted = weighted.mean(age, wgt)) |>
    mutate(name_col = "Age (years)")
  a5 <- read_mean_age2017(censusfile)
  a1 |> inner_join(a2) |> inner_join(a3) |> inner_join(a4) |> inner_join(a5)
}

summarize_region <- function(df, fname) {
  regions_df <- tribble(~region, ~sortorder,
                        "Northeast",1,
                        "Midwest",2,
                        "South",3,
                        "West",4)
  regionresults <- list()
  for (r in regions_df$region) {
    r1 <- df |> filter(completion_state>2) |>
      summarize(mean_available = mean( region==r)) |>
      mutate(name_col = r)
    r2 <- df |> filter(completion_state==4) |>
      summarize(mean_attrition = mean( region==r)) |>
      mutate(name_col = r)
    r3 <- df |> filter(completion_state==5) |>
      summarize(mean_sample = mean( region==r )) |>
      mutate(name_col= r)
    r4 <- df |> filter(completion_state==5) |>
      summarize(mean_weighted = weighted.mean(region==r, wgt)) |>
      mutate(name_col= r)
    regionresults[[r]] <- r1 |> inner_join(r2) |> inner_join(r3) |> inner_join(r4)
  }
  rdf <- Reduce(bind_rows,regionresults)
  cdf <- read_mean_region2017(fname)
  rdf |> inner_join(cdf) |>
    left_join(regions_df, by=c("name_col"="region")) |>
    arrange(sortorder) |>
    dplyr::select(-sortorder)
}

summarize_education <- function(df, fname) {
  outcomes <- tribble(~s_edu_category, ~sortorder,
                      "No high school", 1,
                      "High School/GED",2,
                      "Some college/ass. degree",3,
                      "Bachelor (4 years)",4,
                      "Graduate degree",5)
  dft <- df |>  mutate(edu_category = sjlabelled::as_label(education),
                       s_edu_category = case_when(
                          edu_category == "No High School" ~ "No high school",
                          edu_category == "High School/GED" ~ "High School/GED",
                          edu_category == "Associate's Degree" ~ "Some college/ass. degree",
                          edu_category == "Some college" ~ "Some college/ass. degree",
                          edu_category == "Bachelor" ~ "Bachelor (4 years)",
                          edu_category == "Professional (JD/MD)" ~ "Graduate degree",
                          edu_category == "PhD" ~ "Graduate degree",
                          edu_category == "Masters" ~ "Graduate degree"))
  eduresults <- list()
  for (r in outcomes$s_edu_category) {
    e1 <- dft |> filter(completion_state>2) |>
      summarize(mean_available = mean( s_edu_category==r)) |>
      mutate(name_col = r)
    e2 <- dft |> filter(completion_state==4) |>
      summarize(mean_attrition = mean( s_edu_category==r)) |>
      mutate(name_col = r)
    e3 <- dft |> filter(completion_state==5) |>
      summarize(mean_sample = mean( s_edu_category==r )) |>
      mutate(name_col= r)
    e4 <- dft |> filter(completion_state==5) |>
      summarize(mean_weighted = weighted.mean(s_edu_category==r, wgt)) |>
      mutate(name_col= r)
    eduresults[[r]] <- e1 |> inner_join(e2) |> inner_join(e3) |> inner_join(e4)
  }
  rdf <- Reduce(bind_rows,eduresults)
  edf <- read_educationdistribution2017(fname)
  rdf |> inner_join(edf) |>
    left_join(outcomes, by=c("name_col"="s_edu_category")) |>
    arrange(sortorder) |>
    dplyr::select(-sortorder)
}



summarize_gender <- function(df, censusfile) {
  gdf <- df |>
    mutate( gender = sjlabelled::as_label(gender),
            female = as.numeric(gender=="female"))
  g1 <- gdf |>
    filter(completion_state>2) |>
    summarize(mean_available = mean(female)) |>
    mutate(name_col = "Female (d)")
  g2 <- gdf |>
    filter(completion_state==4) |>
    summarize(mean_attrition = mean(female)) |>
    mutate(name_col = "Female (d)")
  g3 <- gdf |> filter(completion_state==5) |>
    summarize(mean_sample = mean(female))  |>
    mutate(name_col = "Female (d)")
  g4 <- gdf |> filter(completion_state==5) |>
    summarize(mean_weighted = weighted.mean(female, wgt)) |>
    mutate(name_col = "Female (d)")
  g5 <- read_mean_female2017(censusfile)
  g1 |> inner_join(g2) |> inner_join(g3) |> inner_join(g4) |> inner_join(g5)
}

summarize_income <- function(df, fname) {
  outcomes <- tribble(~income_category, ~sortorder,
                      "Y<30", 1,
                      "30 leq Y < 60",2,
                      "60 leq Y 100",3,
                      "100 leq Y < 150",4,
                      "Y geq 150",5)
  dft <- df |>  mutate(income_category = sjlabelled::as_label(income)) |>
    mutate(income_category = case_when(
      income_category=="Less than 29 999" ~ "Y<30",
      income_category=="30k- 59 999" ~ "30 leq Y < 60",
      income_category=="60k - 99 999" ~ "60 leq Y 100",
      income_category=="100k - 149 999" ~ "100 leq Y < 150",
      income_category=="150k +" ~ "Y geq 150"
    ))
  incomeresults = list()
  for (r in outcomes$income_category) {
    i1 <- dft |> filter(completion_state>2) |>
      summarize(mean_available = mean( income_category==r)) |>
      mutate(name_col = r)
    i2 <- dft |> filter(completion_state==4) |>
      summarize(mean_attrition = mean( income_category==r)) |>
      mutate(name_col = r)
    i3 <- dft |> filter(completion_state==5) |>
      summarize(mean_sample = mean( income_category==r )) |>
      mutate(name_col= r)
    i4 <- dft |> filter(completion_state==5) |>
      summarize(mean_weighted = weighted.mean(income_category==r, wgt)) |>
      mutate(name_col= r)
    incomeresults[[r]] <- i1 |> inner_join(i2) |> inner_join(i3) |> inner_join(i4)
  }
  idf <- Reduce(bind_rows, incomeresults)
  censusdata <- read_incomedistribution2017(fname)
  idf |> left_join(censusdata) |>
    left_join(outcomes,by=c("name_col"="income_category") ) |>
    arrange(sortorder) |>
    dplyr::select(-sortorder)
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
    dplyr::select(Outcome, ExAnteImpersonal, ExAntePersonal, ExPostImpersonal, ExPostPersonal)
  bt.T
}

format_background_balance_table <- function(rows) {
  rows %>% gt() %>%
    fmt_number(columns = c("ExAnteImpersonal", "ExAntePersonal",
                           "ExPostImpersonal", "ExPostPersonal"),
               n_sigfig=3) %>%
    tab_spanner(
      label = "ExAnte",
      columns = c("ExAnteImpersonal", "ExAntePersonal")) %>%
    tab_spanner(
      label = "ExPost",
      columns = c("ExPostImpersonal", "ExPostPersonal")
    ) %>%
    cols_label( "ExAnteImpersonal" = "Impersonal",
                "ExAntePersonal" = "Personal",
                "ExPostImpersonal" = "Impersonal",
                "ExPostPersonal" = "Personal")
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
    dplyr::select(Outcome, ExAnteImpersonal, ExAntePersonal, ExPostImpersonal, ExPostPersonal)

  pvals <- tibble(Outcome = c("Luck (unfair)",
                               "Talent (unfair)",
                               "Effort (unfair)",
                               "Luck (control)",
                               "Talent (control)",
                               "Effort (control)",
                               "Should society aim to equalize incomes"),
                   "P-value (F)" = c(p.adjust(c(summary(aov(df$luck_fair~df$treatment, weights=df$wgt))[[1]][["Pr(>F)"]][1],
                                     summary(aov(df$talent_fair~df$treatment, weights=df$wgt))[[1]][["Pr(>F)"]][1],
                                     summary(aov(df$effort_fair~df$treatment, weights=df$wgt))[[1]][["Pr(>F)"]][1]), method="holm"),
                                     p.adjust(c(summary(aov(df$luck_control~df$treatment, weights=df$wgt))[[1]][["Pr(>F)"]][1],
                                     summary(aov(df$talent_control~df$treatment, weights=df$wgt))[[1]][["Pr(>F)"]][1],
                                     summary(aov(df$effort_control~df$treatment, weights=df$wgt))[[1]][["Pr(>F)"]][1]), method="holm"),
                                     summary(aov(df$redist_pref~df$treatment, weights=df$wgt))[[1]][["Pr(>F)"]][1])
                                     )
  at.T %>% left_join(pvals, by="Outcome")
}

survey_balance_rows_timing <- function(df) {
  at <- df %>% group_by(timing) %>%
    summarize( "Luck (unfair)" = weighted.mean(luck_fair, wgt, na.rm=TRUE),
               "Talent (unfair)" = weighted.mean(talent_fair, wgt, na.rm=TRUE),
               "Effort (unfair)" = weighted.mean(effort_fair, wgt, na.rm=TRUE),
               "Luck (control)" = weighted.mean(luck_control, wgt, na.rm=TRUE),
               "Talent (control)" = weighted.mean(talent_control, wgt, na.rm=TRUE),
               "Effort (control)" = weighted.mean(effort_control, wgt, na.rm=TRUE),
               "Should society aim to equalize incomes" = weighted.mean(redist_pref, wgt, na.rm=TRUE))
  at.T <- as.data.frame(as.matrix(t(at[,-1])))
  colnames(at.T) <- as.character(at$timing)
  rwn <- rownames(at.T)
  at.T <- at.T %>% mutate(Outcome = rwn) %>%
    dplyr::select(Outcome, ExAnte, ExPost)

  pvals <- tibble(Outcome = c("Luck (unfair)",
                              "Talent (unfair)",
                              "Effort (unfair)",
                              "Luck (control)",
                              "Talent (control)",
                              "Effort (control)",
                              "Should society aim to equalize incomes"),
                  "P-value (F)" = c(p.adjust(c(summary(aov(df$luck_fair~df$timing, weights=df$wgt))[[1]][["Pr(>F)"]][1],
                                               summary(aov(df$talent_fair~df$timing, weights=df$wgt))[[1]][["Pr(>F)"]][1],
                                               summary(aov(df$effort_fair~df$timing, weights=df$wgt))[[1]][["Pr(>F)"]][1]), method="holm"),
                                    p.adjust(c(summary(aov(df$luck_control~df$timing, weights=df$wgt))[[1]][["Pr(>F)"]][1],
                                               summary(aov(df$talent_control~df$timing, weights=df$wgt))[[1]][["Pr(>F)"]][1],
                                               summary(aov(df$effort_control~df$timing, weights=df$wgt))[[1]][["Pr(>F)"]][1]), method="holm"),
                                    summary(aov(df$redist_pref~df$timing, weights=df$wgt))[[1]][["Pr(>F)"]][1])
  )
  at.T %>% left_join(pvals, by="Outcome")
}

format_survey_balance_table <- function(rows) {
  rows %>% gt() %>%
    fmt_number(columns = c("ExAnteImpersonal", "ExAntePersonal",
                           "ExPostImpersonal", "ExPostPersonal"),
               n_sigfig = 3) %>%
    fmt_number(columns = c("P-value (F)"), decimals = 3) %>%
    tab_spanner(
      label = "ExAnte",
      columns = c("ExAnteImpersonal", "ExAntePersonal")) %>%
    tab_spanner(
      label = "ExPost",
      columns = c("ExPostImpersonal", "ExPostPersonal")
    ) %>%
    cols_label( "ExAnteImpersonal" = "Impersonal",
                "ExAntePersonal" = "Personal",
                "ExPostImpersonal" = "Impersonal",
                "ExPostPersonal" = "Personal")
}

format_survey_balance_table_timing <- function(rows) {
  rows %>% gt() %>%
    fmt_number(columns = c("ExAnte", "ExPost"),
               n_sigfig = 3) %>%
    fmt_number(columns = c("P-value (F)"), decimals = 3)
}

read_incomedistribution2017 <- function(fname) {
  original <- readxl::read_excel(fname)
  r_df <- original[10:53,1:2]
  colnames(r_df) <- c("incgroup","nmb")
  r_df$nmb <- as.integer(r_df$nmb)
  r_df$name_col <- c(rep("Y<30",12), rep("30 leq Y < 60", 12), rep("60 leq Y 100", 16), rep("100 leq Y < 150",1), rep("Y geq 150",3))
  r_df %>% group_by(name_col) %>%
    summarize( n = sum(nmb)) %>%
    mutate( means_census = n/sum(n)) %>%
    dplyr::select(name_col, means_census)
}

read_educationdistribution2017 <- function(fname) {
  original <- readxl::read_excel(fname)
  e_df <- original[c(5,7),3:17]
  colnames(e_df) <- e_df[1,]
  e_df[2,] %>% pivot_longer(cols = everything(), names_to = "edugroup", values_to = "nmb" ) %>%
    mutate(nmb = as.integer(nmb)) %>%
    mutate(name_col = case_when(
    edugroup %in% c("None","1st - 4th grade","5th - 6th grade","7th - 8th grade","9th grade","10th grade","11th grade2") ~ "No high school",
    edugroup == "High school graduate" ~ "High School/GED",
    edugroup %in% c("Some college, no degree", "Associate's degree, occupational", "Associate's degree, academic") ~ "Some college/ass. degree",
    edugroup == "Bachelor's degree" ~ "Bachelor (4 years)",
    edugroup %in% c("Master's degree","Professional degree","Doctoral degree") ~ "Graduate degree")) %>%
    group_by(name_col) %>%
    summarize(n = sum(nmb)) %>%
    mutate(means_census = n/sum(n)) %>%
    dplyr::select(name_col, means_census)
}

read_mean_age2017 <- function(fname) {
  original <- read_csv(fname,quote="")
  original %>%
  filter(SEX %in% c(1,2)) %>%
  filter(ORIGIN==0) %>%
  filter(AGE>=18) %>%
    group_by(AGE) %>%
    summarize(n=sum(POPESTIMATE2017)) %>%
    summarize(means_census = weighted.mean(AGE, n)) %>%
    mutate(name_col = "Age (years)") %>%
    dplyr::select(name_col, means_census)
}

read_mean_female2017 <- function(fname) {
  original <- read_csv(fname,quote="")
  original |>
    filter(SEX %in% c(1,2)) |>
    filter(ORIGIN==0) |>
    filter(AGE>=18) |>
    group_by(SEX) |>
    summarize(n=sum(POPESTIMATE2017)) |>
    pivot_wider(values_from="n", names_from="SEX", names_prefix = "n") |>
    mutate( means_census = n2/(n1+n2),
            name_col = "Female (d)") |>
    select(c(name_col, means_census))
}

read_mean_region2017 <- function(fname) {
  regions <- tribble(~REGION, ~region,
                     1,"Northeast",
                     2,"Midwest",
                     3,"South",
                     4,"West")
  original <- read_csv(fname,quote="", show_col_types = FALSE) |> left_join(regions, by="REGION")
  original |>
    filter(SEX %in% c(1,2)) |>
    filter(ORIGIN==0) |>
    filter(AGE>=18) |>
    group_by(region) |>
    summarize(n = sum(POPESTIMATE2017)) |>
    mutate(means_census = n/sum(n)) |>
    rename(name_col = region) |>
    select(-n)
}
