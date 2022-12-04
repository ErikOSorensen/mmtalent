#' Compute a weighted variance or standard deviation of a vector.
#'
#' @details
#' Note that unlike the base R \code{\link{var}} function, these functions only
#' work with individual vectors not matrices or data frames.  Borrowed from hadley/bigvis
#'
#' @family weighted statistics
#' @seealso \code{\link[stats]{weighted.mean}}
#' @param x numeric vector of observations
#' @param w integer vector of weights, representing the number of
#'  time each \code{x} was observed
#' @param na.rm if \code{TRUE}, missing values in both \code{w} and \code{x}
#'   will be removed prior computation. Otherwise if there are missing values
#'   the result will always be missing.
#' @export
weighted.var <- function(x, w = NULL, na.rm = FALSE) {
  if (na.rm) {
    na <- is.na(x) | is.na(w)
    x <- x[!na]
    w <- w[!na]
  }

  sum(w * (x - weighted.mean(x, w)) ^ 2) / (sum(w) - 1)
}

#' @export
#' @rdname weighted.var
weighted.sd <- function(x, w, na.rm = TRUE) sqrt(weighted.var(x, w, na.rm = TRUE))

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

weighted.bars <- function(y, weight, values=NULL, name=NULL, na.rm=FALSE) {
  df <- dplyr::tibble(x=y, w=weight)
  df <- df |> fastDummies::dummy_columns(select_columns="x", ignore_na=TRUE,
                                          remove_selected_columns = TRUE)
  means <- df |> summarize_at(vars(starts_with("x_")),
                               ~weighted.mean(., w, na.rm=TRUE))
  out <- means |> pivot_longer(starts_with("x_"), values_to ="proportion", names_to = "v",
                                names_prefix="x_") |>
    mutate(v=as.numeric(v))
  if (!is.null(values)) {
    dfv <- tibble(v=values, proportion=0)
    out <- out |> bind_rows(dfv)
    out <- out |> group_by(v) |>
      summarize(proportion=sum(proportion))
  }
  if (!is.null(name)) {
    out <- out |> rename(v=name)
  }
  out
}

