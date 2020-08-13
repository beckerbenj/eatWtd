#' Weighted variance.
#'
#' Compute a weighted variance.
#'
#' This function provides similar functionality as the \code{\link[Hmisc]{wtd.var}} function from \code{Hmisc}.
#'
#'@param x numeric vector
#'@param weights a numeric vector of non-negative weights
#'@param na.rm set to \code{FALSE} to suppress checking for NAs. If \code{TRUE}, NAs are removed
#'from \code{x} as well as from \code{weights} prior to variance estimation.
#'
#'@return a scalar
#'
#'@examples
#'x <- c(50, 1, 25)
#'w <- c(1, 4, 1)
#'
#'# compute weighted variance
#'wtdVar(x, w)
#'
#'@export
wtdVar <- function (x , weights , na.rm = FALSE) {
    if (na.rm) {
      na <- is.na(x) | is.na(weights)
      x <- x[!na]
      weights <- weights[!na]
    }
    out <- sum(weights * (x - stats::weighted.mean(x, weights)) ^ 2) / (sum(weights) - 1)
return(out)}
