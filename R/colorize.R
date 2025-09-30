#' Add colors by category
#'
#' A simple function to add a variable of colors to a data frame based on a
#' separate categorical variable. Designed to be used within the \code{dplyr}
#' \code{mutate} funciton.
#'
#' @param x Categorical. Used to base the coloring of the observations on. For
#'   now must have 12 or fewer categories to work.
#'
#' @return When used with \code{mutate}, the function returns a variable where
#'   each observation is assigned a color based on the categories of a
#'   categorical variable.
#'
#' @examples
#' mt_colors <- mutate(mtcars, colors = colorize(cyl))
#' head(mt_colors$colors)
#' xyplot(hp ~ disp, data = mt_colors, groups = colors)
#'
#' @importFrom RColorBrewer brewer.pal
#' @export

colorize <- function(x) {

  # Make sure variable is a factor, otherwise coerce it.
  if (!is.factor(x)) {
    x <- as.factor(x)
  }

  # Get levels for factor and make sure there's fewer than 12 levels.
  lvls <- levels(x)
  if (length(lvls) > 12) {
    stop("Variable must have 12 or fewer categories")
  }

  # Create the colors and then return them as a variable.
  cols <- brewer.pal(n = length(lvls), name = "Paired")
  c_mat <- outer(x, lvls, "==")
  cols[apply(c_mat, 1, which)]
}
