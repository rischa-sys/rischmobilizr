#' Box and Whisker Plot
#'
#' Create a box and whisker plot for a numerical variable.
#'
#' @param x Formula. An expression of the form \code{~ x | z} where \code{x} is
#'   the variable we want to make boxplots for and \code{z} is a second,
#'   optional, variable we would like to use for faceting. Alternatively, one
#'   could also use the form \code{z ~ x} or \code{x ~ z} to change how the
#'   faceted boxplot is displayed.
#' @param data Data frame. The data where the variables can be found.
#'
#' @examples
#' data(cdc)
#' bwplot(~height, data = cdc)
#' bwplot(~height | gender, data = cdc)
#' bwplot(gender ~ height, data = cdc)
#' bwplot(height ~ gender, data = cdc)
#'
#' @export

bwplot <- function(x, data, color = "blue", ...) {


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


  # Change the midpoint symbol from a dot to a line
  lattice::bwplot(x = x, data = data, col = color, ...,
                  par.settings = list(box.rectangle = list(col = color),
                                      box.dot = list(col = color),
                                      box.umbrella = list(col = color),
                                      plot.symbol = list(col = color)),
                  panel = function(x, ...) {
    lattice::panel.bwplot(x, pch = "|", ...)
  })
}
