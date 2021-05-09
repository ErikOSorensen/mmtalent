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
