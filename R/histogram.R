#' Histogram Plots
#'
#' Create a histogram of single variables.
#'
#' @param x \code{formula}. An expression of the form \code{~ x | z} where \code{x} is the variable we want to make histograms for and \code{z} is a second, optional, variable we would like to use for faceting.
#' @param data data frame. The data where the variables can be found.
#' @param type character. A character string that describes what units the user would like on the y-axis. Options include \code{"count"} for frequencies, \code{"percent"} for percentages and \code{"density"} which can be used when adding fitted lines.
#' @param nint number. The number of bins to use in the histogram
#' @param fit character. A character string that describes a probablility density curve to overlay on the histogram of the data. Distributions that can be used are \code{"normal"}, \code{"beta"}, \code{"cauchy"}, \code{"chisq"} (similarly \code{"chi-squared"}), \code{"exponential"}, \code{"gamma"}, \code{"geometric"}, \code{"lognormal"}, \code{"poisson"}, \code{"t"} and \code{"weibull"}
#' @param breaks Usually a numeric vector of length (number of bins + 1) defining the breakpoints of the bins. Note that when breakpoints are not equally spaced, the only value of type that makes sense is density.
#' @section Note:
#' For additional options for creating histogram, look at the help file found in the \code{lattice} package or run \code{help(histogram, package="lattice")} in the console.
#' @examples
#' data(cdc)
#' histogram(~height, data = cdc)
#' histogram(~height, data = cdc, fit = "normal")
#' histogram(~height | gender, data = cdc)
#'
#' @export



histogram <- function(x, data, type = 'count', fit, ...) {

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

  # Filter out observations with NAs in its dependent variable
  if(!missing(data)){
    containing_na<- all.vars(x)[1]
    data<- data %>% filter(is.na(!!rlang::sym(containing_na)) == F)
  }

  # Change default y-axis to "count"
  # If including a fitted density curve, change y-axis back to "density"
  if(!missing(fit)) {
    suppressWarnings(expr =
    lattice::histogram(x = x, data = data, type = 'density', fit = fit, ...))
  } else {
    suppressWarnings(expr =
    lattice::histogram(x = x, data = data, type = type, ...))
  }



}

#' @export

old_histogram <- function (x, data, type = "count", nint = 8, fit, breaks, ...) {

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


  # Check for subsetting, if so, subset data manually otherwise use function defaults
  dots_args <- eval(substitute(alist(...)))
  if (!is.null(dots_args$subset)) {
    data <- subset(data, eval(dots_args$subset))
  }

  # Parse formula to check for conditioning/faceting
  form <- latticeParseFormula(model = x, data = data)
  if (missing(breaks) & !is.null(form$condition)) {
    breaks <- seq(min(form$right), max(form$right), length.out = nint + 1)
  }

  # Run the rest of the function as before.
  if (!missing(fit)) {
    lattice::histogram(x = x, data = data, type = "density", nint = nint,
                       fit = fit, breaks = breaks, ...)
  }
  else {
    lattice::histogram(x = x, data = data, nint = nint, type = type,
                       breaks = breaks, ...)
  }
}


#' @export
old_histogram19 <- function (x, data = NULL, type = "count", nint = 8, fit, breaks, ...) {

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

  dots_args <- eval(substitute(alist(...)))

  if (is.numeric(x) & !missing(fit)) {
    return(lattice::histogram(x = ~x, data = NULL, type = "density", nint = nint, fit = fit, breaks = breaks, ...))
  }

  if (is.numeric(x)) {
    return(lattice::histogram(x = ~x, data = NULL, type = type, nint = nint, breaks = breaks, ...))
  }

  if (!is.null(dots_args$subset)) {
    data <- subset(data, eval(dots_args$subset))
  }


  form <- latticeParseFormula(model = x, data = data)


  if (missing(breaks) & !is.null(form$condition)) {
    breaks <- seq(min(form$right), max(form$right), length.out = nint + 1)
  }

  if (!missing(fit)) {
    lattice::histogram(x = x, data = data, type = "density", nint = nint,
                       fit = fit, breaks = breaks, ...)
  }
  else {
    lattice::histogram(x = x, data = data, nint = nint, type = type,
                       breaks = breaks, ...)
  }
}

