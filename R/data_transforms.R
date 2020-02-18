#' Do basic data transforms
#'
#' @param df Original dataset
#' @param df_popweights Dataset of population weights per age_category X region X gender
#'
#' @return Transformed dataset
#' @export
#'
#' @examples
prepare_data <- function(df, df_popweights) {
  wgts <- find_population_weights(df, df_popweights)
  transformed_data <- df %>% mutate(
    left = (polpref < 3),
    nothingtw = 1*(payment_low_worker %in% c(2, 8)),
    gini = abs(payment_low_worker - payment_high_worker)/10) %>%
    left_join(wgts, by=c("age_category", "region", "gender"))
  transformed_data
}

#' Calculate population weights
#'
#' @param df_experiment Original dataset
#' @param df_popweights Dataset of population weights
#'
#' @return A dataset of weights for each age_category X region X gender?
#' @export
#'
#' @examples
find_population_weights <- function(df_experiment, df_popweights) {
  total_n_exp <- nrow(df_experiment)
  ncensus <- sum(df_popweights$n2016)
  weights <- df_experiment %>%
    group_by(age_category, region, gender) %>%
    summarize(n_exp = n()) %>%
    left_join(df_popweights, by=c("age_category", "region", "gender")) %>%
    mutate(share_in_sample = n_exp / total_n_exp,
           share_in_pop = n2016 / ncensus,
           wgt = share_in_pop / share_in_sample) %>%
    select(age_category, region, gender, wgt)
  weights
}
