#' Compute Polynomial fits of variables
#'
#' Compute Polynomial fits of variables for use in linear models.
#'
#' @param x Variable. The variable you would like to fit a polynomial to.
#' @param degree Integer. The degree of the polynomial to fit.
#' @param raw Logical. Defaults is \code{TRUE}, meaning the polynomial coefficient outputs can be used in direct calculations/predictions. Setting this value to \code{FALSE} results in \emph{orthogonal} polynomials whose coefficients can't be used to compute predictions by hand.
#'
#' @examples
#' # Use a quadratic polynomial to predict weight based on height.
#' lm(weight ~ poly(height, 2), data = cdc)
#'
#' @export

poly <- function(x, ..., degree = 1, raw = TRUE) {
  stats::poly(x = x, ..., degree = degree, raw = raw)
}

