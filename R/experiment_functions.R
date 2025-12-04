implemented_inequality <- function(dt) {
  dt <- dt |> mutate(male = gender=="male")
  R1 <- lm(gini ~ treatment, data = dt, weights=wgt)
  R2 <- lm(gini ~ treatment + age_high + left + high_edu + high_income + male,
           data = dt, weights=wgt)
  R3 <- lm(nothingtw ~ treatment, data = dt, weights=wgt)
  R4 <- lm(nothingtw ~ treatment + age_high + left + high_edu + high_income + male,
           data = dt, weights=wgt)

  R5 <- lm(noredistribution ~ treatment, data = dt, weights=wgt)
  R6 <- lm(noredistribution ~ treatment + age_high + left + high_edu + high_income + male,
           data = dt, weights=wgt)


  list(R1,R2,R3,R4,R5,R6)
}


inequality_heterogeneity_graph <- function(mmtalent) {
  dt <- mmtalent |> mutate(exante = as.numeric(timing =="ExAnte"),
                           person = as.numeric(personal=="Personal"))
  subsets <- tibble(
    group = c("Age", "Age", "Politics", "Politics", "Income", "Income", "Education", "Education", "Gender", "Gender"),
    subset_name = c("Low age", "High age", "Left", "Right", "Low income", "High income", "Low education", "High education", "Male", "Female"),
    filter_condition = list(
      dt |> filter(age_high==0),
      dt |> filter(age_high==1),
      dt |> filter(left==TRUE),
      dt |> filter(left==FALSE),
      dt |> filter(high_income==FALSE),
      dt |> filter(high_income==TRUE),
      dt |> filter(high_edu==FALSE),
      dt |> filter(high_edu==TRUE),
      dt |> filter(gender=="male"),
      dt |> filter(gender=="female")
    )
  )
  results1 <- subsets |>
    mutate(
      model_results = map(filter_condition, ~ {
        lm(gini ~ exante, data=.x, weights=wgt) |>
          tidy() |>
          filter(term == "exante")
      })
    ) |>
    unnest(model_results) |>
    dplyr::select(group, subset_name, estimate, std.error)
  results1 <- results1 |>
    mutate(subset_name = factor(subset_name, levels = unique(subset_name)))
  results2 <- subsets |>
    mutate(
      model_results = map(filter_condition, ~ {
        lm(nothingtw ~ exante, data=.x, weights=wgt) |>
          tidy() |>
          filter(term == "exante")
    })
    ) |>
    unnest(model_results) |>
    dplyr::select(group, subset_name, estimate, std.error)
  results2 <- results2 |>
    mutate(subset_name = factor(subset_name, levels = unique(subset_name)))
  results3 <- subsets |>
    mutate(
      model_results = map(filter_condition, ~ {
        lm(gini ~ person, data=.x, weights=wgt) |>
          tidy() |>
          filter(term == "person")
      })
    ) |>
    unnest(model_results) |>
    dplyr::select(group, subset_name, estimate, std.error)
  results3 <- results3 |>
    mutate(subset_name = factor(subset_name, levels = unique(subset_name)))
  results4 <- subsets |>
    mutate(
      model_results = map(filter_condition, ~ {
        lm(nothingtw ~ person, data=.x, weights=wgt) |>
          tidy() |>
          filter(term == "person")
      })
    ) |>
    unnest(model_results) |>
    dplyr::select(group, subset_name, estimate, std.error)
  results4 <- results4 |>
    mutate(subset_name = factor(subset_name, levels = unique(subset_name)))


  graph1 <- results1 |> ggplot(aes(x = estimate, y = subset_name)) +
    geom_vline(xintercept = 0, color = "black", size = 0.5, linetype = "solid") +
    geom_point() +
    geom_errorbarh(aes(xmin = estimate - 1.96 * std.error, xmax = estimate + 1.96 * std.error), height = 0.2) +
    facet_wrap(~ group, scales = "free_y", ncol = 1, switch = "y") +
    scale_x_continuous(limits = c(-0.03, 0.25), expand = expansion(mult = c(0.1, 0.1)),
                       breaks = c(0,0.05,0.10,0.15,0.20)) +
    labs(
      x = "Estimated effect of ex ante vs. ex post \u00B1 95% CI",
      y = element_blank(),
      title = "Effects on Gini"
    ) +
    theme_minimal() +
    theme(
      panel.spacing = unit(0, "lines"),
      strip.text = element_text(size = 12),
      strip.placement = "outside",
      strip.text.y.left = element_text(angle = 0),
      plot.title.position = "plot"
    )

  graph2 <- results2 |> ggplot(aes(x = estimate, y = subset_name)) +
    geom_vline(xintercept = 0, color = "black", size = 0.5, linetype = "solid") +
    geom_point() +
    geom_errorbarh(aes(xmin = estimate - 1.96 * std.error, xmax = estimate + 1.96 * std.error), height = 0.2) +
    facet_wrap(~ group, scales = "free_y", ncol = 1, switch = "y") +
    scale_x_continuous(limits = c(-0.03, 0.25), expand = expansion(mult = c(0.1, 0.1)),
                       breaks = c(0,0.05,0.10,0.15,0.20)) +
    labs(
      x = "Estimated effect of ex ante vs. ex post \u00B1 95% CI",
      y = element_blank(),
      title = "Effects on share that implements maximum inequality"
    ) +
    theme_minimal() +
    theme(
      panel.spacing = unit(0, "lines"),
      strip.text = element_text(size = 12),
      strip.placement = "outside",
      strip.text.y.left = element_text(angle = 0),
      plot.title.position = "plot")

  graph3 <- results3 |> ggplot(aes(x = estimate, y = subset_name)) +
    geom_vline(xintercept = 0, color = "black", size = 0.5, linetype = "solid") +
    geom_point() +
    geom_errorbarh(aes(xmin = estimate - 1.96 * std.error, xmax = estimate + 1.96 * std.error), height = 0.2) +
    facet_wrap(~ group, scales = "free_y", ncol = 1, switch = "y") +
    scale_x_continuous(limits = c(-0.2, 0.08), expand = expansion(mult = c(0.1, 0.1)),
                       breaks = c(-0.2, -0.15, -0.1, -0.05, 0, 0.05)) +
    labs(
      x = "Estimated effect of personal vs. impersonal \u00B1 95% CI",
      y = element_blank(),
      title = "Effects on Gini"
    ) +
    theme_minimal() +
    theme(
      panel.spacing = unit(0, "lines"),
      strip.text = element_text(size = 12),
      strip.placement = "outside",
      strip.text.y.left = element_text(angle = 0),
      plot.title.position = "plot"
    )

  graph4 <- results4 |> ggplot(aes(x = estimate, y = subset_name)) +
    geom_vline(xintercept = 0, color = "black", size = 0.5, linetype = "solid") +
    geom_point() +
    geom_errorbarh(aes(xmin = estimate - 1.96 * std.error, xmax = estimate + 1.96 * std.error), height = 0.2) +
    facet_wrap(~ group, scales = "free_y", ncol = 1, switch = "y") +
    scale_x_continuous(limits = c(-0.2, 0.08), expand = expansion(mult = c(0.1, 0.1)),
                       breaks = c(-0.2, -0.15, -0.1, -0.05, 0, 0.05)) +
    labs(
      x = "Estimated effect of personal vs. impersonal \u00B1 95% CI",
      y = element_blank(),
      title = "Effects on share that implements maximum inequality"
    ) +
    theme_minimal() +
    theme(
      panel.spacing = unit(0, "lines"),
      strip.text = element_text(size = 12),
      strip.placement = "outside",
      strip.text.y.left = element_text(angle = 0),
      plot.title.position = "plot")




  list("results1"=results1,  "results2"=results2, "results3"=results3,  "results4"=results4,
       "graph1"=graph1, "graph2"=graph2, "graph3"=graph3, "graph4"=graph4)

}


implemented_inequality_heterogeneity <- function(dt) {
  dt <- dt |> mutate(male = gender=="male",
                     person = personal=="Personal")
  A1 <- lm(gini ~ timing + timing*age_high + age_high + left + high_edu + high_income + male ,
           data = dt, weights=wgt)
  A2 <- lm(gini ~ timing + timing*left + age_high + left + high_edu + high_income + male,
           data = dt, weights=wgt)
  A3 <- lm(gini ~ timing + timing*high_edu + age_high + left + high_edu + high_income + male,
           data = dt, weights=wgt)
  A4 <- lm(gini ~ timing + timing*high_income + age_high + left + high_edu + high_income + male,
           data = dt, weights=wgt)
  A5 <- lm(gini ~ timing + timing*male + left + age_high + high_edu + high_income + male,
           data = dt, weights=wgt)
  Alist <- list(A1,A2,A3,A4,A5)

  B1 <- lm(nothingtw ~ timing + timing*age_high + age_high + left + high_edu + high_income + male ,
           data = dt, weights=wgt)
  B2 <- lm(nothingtw ~ timing + timing*left + age_high + left + high_edu + high_income + male,
           data = dt, weights=wgt)
  B3 <- lm(nothingtw ~ timing + timing*high_edu + age_high + left + high_edu + high_income + male,
           data = dt, weights=wgt)
  B4 <- lm(nothingtw ~ timing + timing*high_income + age_high + left + high_edu + high_income + male,
           data = dt, weights=wgt)
  B5 <- lm(nothingtw ~ timing + timing*male + left + age_high + high_edu + high_income + male,
           data = dt, weights=wgt)
  Blist <- list(B1,B2,B3,B4,B5)

  All <- c(Alist,Blist)


  C1 <- lm(gini ~ person + person*age_high + age_high + left + high_edu + high_income + male ,
           data = dt, weights=wgt)
  C2 <- lm(gini ~ person + person*left + age_high + left + high_edu + high_income + male,
           data = dt, weights=wgt)
  C3 <- lm(gini ~ person + person*high_edu + age_high + left + high_edu + high_income + male,
           data = dt, weights=wgt)
  C4 <- lm(gini ~ person + person*high_income + age_high + left + high_edu + high_income + male,
           data = dt, weights=wgt)
  C5 <- lm(gini ~ person + person*male + left + age_high + high_edu + high_income + male,
           data = dt, weights=wgt)
  Clist <- list(C1,C2,C3,C4,C5)

  D1 <- lm(nothingtw ~ person + person*age_high + age_high + left + high_edu + high_income + male ,
           data = dt, weights=wgt)
  D2 <- lm(nothingtw ~ person + person*left + age_high + left + high_edu + high_income + male,
           data = dt, weights=wgt)
  D3 <- lm(nothingtw ~ person + person*high_edu + age_high + left + high_edu + high_income + male,
           data = dt, weights=wgt)
  D4 <- lm(nothingtw ~ person + person*high_income + age_high + left + high_edu + high_income + male,
           data = dt, weights=wgt)
  D5 <- lm(nothingtw ~ person + person*male + left + age_high + high_edu + high_income + male,
           data = dt, weights=wgt)
  Dlist <- list(D1,D2,D3,D4,D5)

  DAll <- c(Clist,Dlist)


  list("A"=Alist, "B"=Blist, "All"=All, "DAll" = DAll)

}

histogram_distributions <- function(dt) {
  dt |>
    mutate(treatment = factor(treatment, levels = c("ExAnteImpersonal",
                                                    "ExAntePersonal",
                                                    "ExPostImpersonal",
                                                    "ExPostPersonal"))) |>
        mutate(bin = factor(payment_low_worker - 2, levels = 0:6)) |>
    group_by(treatment, bin) |>
    summarise(w = sum(wgt), .groups = "drop") |>
    group_by(treatment) |>
    mutate(p = w / sum(w)) |>
    ggplot(aes(x = factor(bin), y = p)) +
    geom_col() +
    facet_wrap(~ treatment) +
    theme_minimal() +
    labs(
      x = "Transfer to loser",
      y = "Fraction"
    )
}


average_distributions <- function(dt) {
  df <-  dt |> group_by(treatment) |> summarize(se_gini = weighted.se(gini, wgt),
                                           mean_gini = weighted.mean(gini, wgt))
  gr <- df |>
  ggplot(aes(x=treatment, y=mean_gini)) +
  geom_point() +
  geom_errorbar(aes(ymin=mean_gini - se_gini, ymax=mean_gini + se_gini), width=0.2) +
  labs(y="Mean inequality (Gini) \u00B1 s.e.m.",
       x=element_blank()) +
  theme_minimal()
  list("numbers"=df, "graph"=gr)
}

extreme_shares <- function(dt) {
  shares <- dt |> mutate(no_redistribution = redistribute==0,
                          full_redistribution = redistribute==3) |>
    group_by(treatment) |>
    summarize( no_redistribution_mean = weighted.mean(no_redistribution, wgt),
               no_redistribution_se = weighted.se(no_redistribution, wgt),
               full_redistribution_mean = weighted.mean(full_redistribution, wgt),
               full_redistribution_se = weighted.se(full_redistribution, wgt))

  a <- shares |> ggplot(aes(x=treatment, y=no_redistribution_mean,
                             ymin = no_redistribution_mean - no_redistribution_se,
                             ymax = no_redistribution_mean + no_redistribution_se)) +
    geom_point() +
    geom_errorbar(width=0.2) +
    theme_minimal() +
    scale_x_discrete(guide = guide_axis(n.dodge=2)) +
    labs(x = element_blank(), y = "Share \u00B1 s.e.", title="No redistribution")
  b <- shares |> ggplot(aes(x=treatment, y=full_redistribution_mean,
                             ymin = full_redistribution_mean - full_redistribution_se,
                             ymax = full_redistribution_mean + full_redistribution_se)) +
    geom_point() +
    geom_errorbar(width=0.2) +
    theme_minimal() +
    scale_x_discrete(guide = guide_axis(n.dodge=2)) +
    labs(x = element_blank(), y = "Share \u00B1 s.e.", title="Full redistribution")
  list('a'=a,'b'=b)
}


decision_amount_table <- function(dt) {
  dt <- dt |> mutate(male = gender=="male")
  R2 <- lm(payment_low_worker==2 ~ treatment + age_high + left + high_edu + high_income + male,
           data = dt, weights=wgt)
  R3 <- lm(payment_low_worker==3 ~ treatment + age_high + left + high_edu + high_income + male,
           data = dt, weights=wgt)
  R4 <- lm(payment_low_worker==4 ~ treatment + age_high + left + high_edu + high_income + male,
           data = dt, weights=wgt)
  R5 <- lm(payment_low_worker==5 ~ treatment + age_high + left + high_edu + high_income + male,
           data = dt, weights=wgt)
  R6 <- lm(payment_low_worker==6 ~ treatment + age_high + left + high_edu + high_income + male,
           data = dt, weights=wgt)
  R7 <- lm(payment_low_worker==7 ~ treatment + age_high + left + high_edu + high_income + male,
           data = dt, weights=wgt)
  R8 <- lm(payment_low_worker==8 ~ treatment + age_high + left + high_edu + high_income + male,
                                             data = dt, weights=wgt)

  list(R2,R3,R4,R5,R6,R7,R8)
}

cuddly_data_prep <- function(cuddly_data) {
  # We are only interested in the (US, Luck) decisions
  df <- cuddly_data |>
    filter(us==1) |>
    filter(luck==1) |>
    mutate(transfer = worker_b,
           study = "Alm\u00E5s et al (2020)") |>
    dplyr::select(c(study,transfer))
  df
}

cuddly_comparisons_displays <- function(mmtalent, cuddly_data) {
  cuddly <- cuddly_data_prep(cuddly_data)
  all_transfers <- mmtalent |>
    filter(treatment=="ExPostImpersonal") |>
    mutate(study="Current study",
           transfer = payment_low_worker - 2) |>
    dplyr::select(c(study,transfer)) |>
    bind_rows(cuddly) |>
    mutate(study = factor(study, levels=c("Current study","Alm\u00E5s et al (2020)")))
  g1 <- all_transfers |>
    ggplot(aes(x=transfer, y = after_stat(prop))) +
    geom_bar() +
    theme_minimal() +
    facet_wrap(.~study) + labs(x = "Transfer from winner to loser",
                               y = "Proportion")

  prop_table <- all_transfers |>
    count(study, transfer) |>
    group_by(study) |>
    mutate(
      N = sum(n),
      p = n / N,
      se = sqrt(p * (1 - p) / N),
      cell = sprintf("%.3f<br>(%.3f)", p, se)
    ) |>
    ungroup() |>
    mutate(transfer = factor(transfer, levels = 0:7)) |>
    dplyr::select(transfer, study, cell) |>
    pivot_wider(
      names_from = study,
      values_from = cell
    )

  t1 <- prop_table |>
    gt(rowname_col = NULL) |>
    tab_header(
      title = "Proportion of transfer values by study"
    ) |>
    cols_align("center") |>
    fmt_markdown(columns = everything())

  list("graph"=g1, "table"=t1)
}

