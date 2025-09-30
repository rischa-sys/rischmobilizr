#' Scatter Plots
#'
#' Create a scatter plot.
#'
#' @param x \code{formula}. An expression of the form \code{y ~ x | z} where \code{x} is the variable we want on the x-axis, \code{y} is the variable we want on the y-axis and \code{z} is an optional variable we would like to use for faceting.
#' @param data data frame. The data where the variables can be found.
#' @param groups factor. An additional variable to use for coloring the individual points in the plot.
#' @section Note:
#' For additional options for creating bargraphs, look at the help file found in the \code{lattice} package or run \code{help(xyplot, package="lattice")} in the console.
#' @examples
#' data(cdc)
#' xyplot(weight~height, data = cdc)
#' xyplot(weight ~ height | gender, data = cdc)
#' xyplot(weight ~ height, data = cdc, groups = gender)
#'
#' @export

xyplot <- function(x, data, ...) {


  try({
    condition <- lattice::latticeParseFormula(x, data = data)$condition
    total_levels <- 1
    if (length(condition) > 0 ){
      for (cc in 1: length(condition)){
        total_levels <- total_levels * length(attributes(condition[cc][[1]])$levels)
      }
    }
    if (isTRUE( total_levels > 180 )){
      message("This command is too complex, Total Levels: ", total_levels)
      if (length(condition) > 0 ){
        for (cc in 1: length(condition)){
          message( toString(names(condition[cc])), " has level: ", length(attributes(condition[cc][[1]])$levels) )
        }
      }
      message("Please revise this command and try again. ")
      return (NULL)
    }
    # print(total_levels)
  }, silent = TRUE)

  
  lattice::xyplot(x = x, data = data, auto.key = TRUE, ...)
}
