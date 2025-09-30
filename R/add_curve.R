#' Add a fitted linear model to a plot
#'
#' Include a line or curve created from a linear model with a single response
#' and a single explanatory variable. The \code{mod} function should be created
#' by using the \code{lm} function. To plot a polynomial curve, use the
#' \code{poly} function with the explanatory variable. See the examples below
#' for additional usage details.
#'
#' @param mod Model. A linear model with a single response and a single explanatory
#'   variable.
#' @param col Character. The name of a color to use when plotting the line/curve.
#'
#' @seealso \code{\link{add_line}}, \code{\link{lm}}, \code{\link{poly}}
#'
#' @examples
#' # Create a linear model and use the model plot the line on a xyplot
#' m_line <- lm(weight ~ height, data = cdc)
#' xyplot(weight ~ height, data = cdc)
#' add_curve(m_line)
#'
#' # Create a linear model with a quadratic polynomial and include the curve on
#' # the xyplot.
#' m_curve <- lm(weight ~ poly(height, 2), data = cdc)
#' xyplot(weight ~ height, data = cdc)
#' add_curve(m_curve)
#'
#' @importFrom lattice panel.curve
#' @export


add_curve <- function(mod, col = "black", plot_base = trellis.last.object()) {
  # Determine the degree of the model (How many terms are included)
  deg = length(coef(mod)) - 1

  # Add the curve to an xyplot
  deg <- length(coef(mod)) - 1
  plot_base +
    latticeExtra::layer(panel.curve(rowSums(t(coef(mod) * t(outer(x, 0:deg, "^")))),
                                    col = col),
                        data = list(deg = deg,
                                    x = plot_base$panel.args[[1]]$x,
                                    mod = mod,
                                    col = col))
}
