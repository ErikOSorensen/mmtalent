prepare_data <- function(df) {
  transformed_data <- df |>
    filter(completion_state == 5) |>
    mutate(
      treatment = sjlabelled::as_label(treatment),
      timing = factor(
        case_when(
          treatment %in% c("ExAnteImpersonal", "ExAntePersonal") ~ "ExAnte",
          treatment %in% c("ExPostImpersonal", "ExPostPersonal") ~ "ExPost"
        )
      ),
      personal = factor(
        case_when(
          treatment %in% c("ExAntePersonal","ExPostPersonal") ~ "Personal",
          treatment %in% c("ExAnteImpersonal","ExPostImpersonal") ~ "Impersonal",
        )
      ),
      treatment = relevel(treatment, ref="ExPostImpersonal"),
      timing = relevel(timing, ref="ExPost"),
      personal = relevel(personal, ref="Impersonal"),
      gender = sjlabelled::as_label(gender),
      age_high = as.numeric( age> median(age) ),
      left = (polpref < 3),
      nothingtw = 1*(payment_low_worker %in% c(2, 8)),
      noredistribution = as.numeric(payment_low_worker==2),
      gini = abs(payment_low_worker - payment_high_worker)/10,
      income_category = sjlabelled::as_label(income),
      edu_category = sjlabelled::as_label(education),
      redistribute = payment_low_worker - 2) |>
    mutate(high_income = income_category %in% c("60k - 99 999", "100k - 149 999","150k +"),
           high_edu = edu_category %in% c("PhD", "Professional (JD/MD)", "Masters", "Bachelor"))
  transformed_data
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



