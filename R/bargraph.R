#' Bargraph Plots
#'
#' Create a bargraph of a categorical variable.
#'
#' @param x Formula. An expression of the form \code{~ x | z} where
#'   \code{x} is the variable we want to make bargraphs for and \code{z} is a
#'   second, optional, variable we would like to use for faceting.
#' @param data Data frame. The data where the variables can be found.
#' @param groups Factor. An additional variable to use for side-by-side
#'   bargraphs.
#' @param horizontal Logical. Use \code{horizontal = TRUE} to make a horizontal
#'   bargraph.
#' @param type Character. Choose the type of y-axis. Options include \code{"count"}, \code{"frequency"}, \code{"proportion"} and \code{"percent"}.
#'
#'
#' @section Note: For additional options for creating bargraphs, look at the
#'   help file found in the \code{mosaic} package or run \code{help(bargraph,
#'   package="mosaic")} in the console.
#' @examples
#' data(cdc)
#' bargraph(~depressed, data = cdc)
#' bargraph(~depressed, data = cdc, groups = gender)
#' bargraph(~depressed | grade, data = cdc, groups = gender)
#'
#' @export

bargraph <- function (x, data, horizontal = FALSE, type = "frequency", col = NULL, ...) {
# Set the default plot setting
par_setting <- simpleTheme()

# If user supplies a vector of colors, add these to the plot settings
if(!is.null(col)) {
par_setting <- simpleTheme(col = col)
}

# Include the plot settings in the sub-function calls.
if (!horizontal) {
mosaic::bargraph(x = x, data = data, scales = list(x = list(rot = 30)),
auto.key = TRUE, type = type, par.settings = par_setting, ...)
} else {
mosaic::bargraph(x = x, data = data, horizontal = horizontal,
auto.key = TRUE, type = type, par.settings = par_setting,...)
}
}
