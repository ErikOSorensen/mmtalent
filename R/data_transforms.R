#' Do basic data transforms
#'
#' @param df Original dataset
#' @param df_popweights Dataset of population weights per age_category X region X gender
#'
#' @return Transformed dataset
#' @export
#'
#' @examples
prepare_data <- function(df, wgts) {
  transformed_data <- df %>% mutate(
    left = (polpref < 3),
    nothingtw = 1*(payment_low_worker %in% c(2, 8)),
    gini = abs(payment_low_worker - payment_high_worker)/10) %>%
    left_join(wgts, by=c("age_category", "region", "gender")) %>%
    mutate(high_income = income_category %in% c("60k - 99 999", "100k - 149 999","150k +"),
           high_edu = edu_category %in% c("PhD", "Professional (JD/MD)", "Masters", "Bachelor"))
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
    dplyr::select(age_category, region, gender, wgt)
  weights
}

weighted.scale <- function(x, w = NULL, na.rm = FALSE) {
  m <- weighted.mean(x, w, na.rm=na.rm)
  sd <- weighted.sd(x, w , na.rm = na.rm)
  as.numeric((x - m)/sd)
}

#' Standard error of weighted means
#'
#' @details
#' When means are estimated from surveys with panel weights,
#' we would like standard errors to also take the weights into account.
#' The formula used here is according to Cochran (1977) (which is not
#' very specific), but it was examined in Donald F. Gatz, Luther Smith,
#' The standard error of a weighted mean concentration—I. Bootstrapping vs other methods,
#' Atmospheric Environment, Volume 29, Issue 11, 1995, Pages 1185-1193,
#' https://doi.org/10.1016/1352-2310(94)00210-C
#'
#' Code taken from https://stats.stackexchange.com/questions/25895/computing-standard-error-in-weighted-mean-estimation
#'
#'
#' @family weighted statistics
#' @param x numeric vector of observations
#' @param w Vector of weights
#' @param na.rm if \code{TRUE}, missing values in both \code{w} and \code{x}
#'   will be removed prior computation. Otherwise if there are missing values
#'   the result will always be missing.
#' @export
weighted.se <- function(x, w, na.rm=FALSE)
  #  Computes the variance of a weighted mean following Cochran 1977 definition
{
  if (na.rm) { w <- w[i <- !is.na(x)]; x <- x[i] }
  n = length(w)
  xWbar = weighted.mean(x,w,na.rm=na.rm)
  wbar = mean(w)
  out = n/((n-1)*sum(w)^2)*(sum((w*x-wbar*xWbar)^2)-2*xWbar*sum((w-wbar)*(w*x-wbar*xWbar))+xWbar^2*sum((w-wbar)^2))
  sqrt(out)
}



