#' Print first six observations.
#'
#' \code{head} will print the first six values of an object.
#'
#' @param x Formula or other object to print.
#' @param data Data frame. The data where the variables can be found.
#' @examples
#' head(~hp, data = mtcars)
#' tail(~hp, data = mtcars)
#'
#' @importFrom mosaic aggregatingFunction1
#' @export

# Allow user to use formula syntax with head function.
head <- aggregatingFunction1(utils::head)
