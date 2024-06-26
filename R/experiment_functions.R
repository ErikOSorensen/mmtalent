implemented_inequality <- function(dt) {
  dt <- dt |> mutate(male = gender=="male")
  R1 <- lm(gini ~ treatment, data = dt, weights=wgt)
  R2 <- lm(gini ~ treatment + age_high + left + high_edu + high_income + male,
           data = dt, weights=wgt)
  R3 <- lm(nothingtw ~ treatment, data = dt, weights=wgt)
  R4 <- lm(nothingtw ~ treatment + age_high + left + high_edu + high_income + male,
           data = dt, weights=wgt)

  list(R1,R2,R3,R4)
}


implemented_inequality_heterogeneity <- function(dt) {
  dt <- dt |> mutate(male = gender=="male")
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

  list("A"=Alist, "B"=Blist, "All"=All)
}


histogram_distributions <- function(dt) {
  dt |> ggplot(aes(x=payment_low_worker-2,y = (..count..)/tapply(..count..,..PANEL..,sum)[..PANEL..]) ) +
    geom_bar() +
    theme_minimal() +
    facet_wrap(. ~ treatment) +
    scale_x_discrete(limits=c(0,1,2,3,4,5,6)) +
    labs(x = "Transfer to loser",
         y = "Fraction")
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
