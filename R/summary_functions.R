#' Numerical Summary Functions
#'
#' A set of functions to calculate numerical summaries. The functions set \code{na.rm = TRUE} by default and will print a warning when data contains missing values, (\code{NA}s).
#' @param x A vector of values or a formula. Formulas should be of the form \code{~ x | z} where \code{x} is a numeric variable and \code{z} is a categorical variable. If a formula is supplied, the \code{data} argument must also be supplied.
#' @param data A data set where variables supplied to the formula, \code{~ x | z}, can be found.
#' @examples
#' data(cdc)
#' mean(1:10)
#' mean(~height, data = cdc)
#' mean(~height | gender, data = cdc)
#' MAD(1:10)
#' MAD(~height, data = cdc)
#' MAD(~height | gender, data = cdc)
#' quantile(~height, data = cdc)
#' quantile(~height, data = cdc, probs = 0.6)
#'
#' @import mosaic
#' @importFrom plyr is.formula
#' @export

iqr <- function(x, ...) {
  if (any(is.na(mosaic::mean(x = x, na.rm = FALSE, ...)))) {
    warning(paste("The data contains missing values"), call. = FALSE)
  }
  output <- mosaic::iqr(x = x, ..., na.rm = TRUE)
  return(output)
}

#' @rdname iqr
#' @export
IQR <- function(x, ...) {
  if (any(is.na(mosaic::mean(x = x, na.rm = FALSE, ...)))) {
    warning(paste("The data contains missing values"), call. = FALSE)
  }
  output <- mosaic::iqr(x = x, ..., na.rm = TRUE)
  return(output)
}

SAD.simple <- function(x, ...) {
  mean.x <- mosaic::mean(x, ...)
  abs.deviation <- base::abs(x - mean.x)
  sad <- mosaic::sum(abs.deviation, ...)
  return(sad)
}

SAD.formula <- aggregatingFunction1(SAD.simple)

#' @rdname iqr
#' @export
SAD <- function(x, ...) {
  if (any(is.na(mosaic::mean(x = x, na.rm = FALSE, ...)))) {
    warning(paste("The data contains missing values"), call. = FALSE)
  }
  output <- SAD.formula(x = x, ..., na.rm = TRUE)
  return(output)
}

#' @rdname iqr
#' @export
MAD <- function(x, ..., na.rm = TRUE) {
  tal <- tally(x, format = "count", margin = TRUE, useNA = "no", ...)
  if (is.na(ncol(tal))) {
    n <- unname(tal["Total"])
  } else {
    n <- tal["Total", ]
  }
  output <- SAD(x = x, ...) / n
  return(output)
}

#' @rdname iqr
#' @export
max <- function(x, ...) {
  if (is.formula(x)) {
    if (any(is.na(mosaic::mean(x = x, na.rm = FALSE, ...)))) {
      warning(paste("The data contains missing values"), call. = FALSE)
    }
    output <- mosaic::max(x = x, ..., na.rm = TRUE)
  } else {
    output <- base::max(x, ...)
  }
  return(output)
}

#' @rdname iqr
#' @export
mean <- function(x, ...) {
  if (any(is.na(mosaic::mean(x = x, na.rm = FALSE, ...)))) {
    warning(paste("The data contains missing values"), call. = FALSE)
  }
  output <- mosaic::mean(x = x, na.rm = TRUE, ...)
  return(output)
}

#' @rdname iqr
#' @export
median <- function(x, ...) {
  if (any(is.na(mosaic::mean(x = x, na.rm = FALSE, ...)))) {
    warning(paste("The data contains missing values"), call. = FALSE)
  }
  output <- mosaic::median(x = x, ..., na.rm = TRUE)
  return(output)
}

#' @rdname iqr
#' @export
min <- function(x, ...) {
  if (is.formula(x)) {
    if (any(is.na(mosaic::mean(x = x, na.rm = FALSE, ...)))) {
      warning(paste("The data contains missing values"), call. = FALSE)
    }
    output <- mosaic::min(x = x, ..., na.rm = TRUE)
  } else {
    output <- base::min(x, ...)
  }
  return(output)
}

#' @rdname iqr
#' @export
prod <- function(x, ...) {
  if (any(is.na(mosaic::mean(x = x, na.rm = FALSE, ...)))) {
    warning(paste("The data contains missing values"), call. = FALSE)
  }
  output <- mosaic::prod(x = x, ..., na.rm = TRUE)
  return(output)
}

quantile.formula <- aggregatingFunction1(stats::quantile)

#' @rdname iqr
#' @export
quantile <- function(x, ...) {
  if (any(is.na(mosaic::mean(x = x, na.rm = FALSE, ...)))) {
    warning(paste("The data contains missing values"), call. = FALSE)
  }
  output <- quantile.formula(x = x, ..., na.rm = TRUE)
  return(output)
}

#' @rdname iqr
#' @export
range <- function(x, ...) {
  if (any(is.na(mosaic::mean(x = x, na.rm = FALSE, ...)))) {
    warning(paste("The data contains missing values"), call. = FALSE)
  }
  output <- mosaic::range(x = x, ..., na.rm = TRUE)
  return(output)
}

#' @rdname iqr
#' @export
sd <- function(x, ...) {
  if (is.formula(x)) {
    if (any(is.na(mosaic::mean(x = x, na.rm = FALSE, ...)))) {
      warning(paste("The data contains missing values."), call. = FALSE)
    }
    output <- mosaic::sd(x = x, ..., na.rm = TRUE)
  } else {
    output <- stats::sd(x, na.rm = TRUE, ...)
  }
  return(output)
}

#' @rdname iqr
#' @export
sum <- function(x, ...) {
  if (any(is.na(mosaic::sum(x = x, ...)))) {
    warning(paste("The data contains missing values"), call. = FALSE)
  }
  output <- mosaic::sum(x = x, ..., na.rm = TRUE)
  return(output)
}

#' @rdname iqr
#' @export
var <- function(x, ...) {
  if (any(is.na(mosaic::mean(x = x, ...)))) {
    warning(paste("The data contains missing values"), call. = FALSE)
  }
  output <- mosaic::sd(x = x, ..., na.rm = TRUE)^2
  return(output)
}

#' @rdname iqr
#' @export
cor <- function(x, y = NULL, ..., data = NULL) {
  if (lazyeval::is_formula(x)) {
    if (lazyeval::is_formula(y)) {
      x <- lazyeval::f_eval(x, data)
      y <- lazyeval::f_eval(y, data)
    }
    else {
      formula <- mosaicCore::mosaic_formula_q(x, max.slots = 3)
      x <- lazyeval::f_eval_rhs(formula, data)
      y <- lazyeval::f_eval_lhs(formula, data)
    }
    if (any(is.na(x) | any(is.na(y)))) {
      warning("The variables selected feature NA values which have been removed.", call. = FALSE)
      not_na <- complete.cases(cbind(x, y))
      x <- x[not_na]
      y <- y[not_na]
    }
    stats::cor(x, y, ...)
  }
  if (any(is.na(x) | any(is.na(y)))) {
    warning("The variables selected feature NA values which have been removed.", call. = FALSE)
    not_na <- complete.cases(cbind(x, y))
    x <- x[not_na]
    y <- y[not_na]
  }
  stats::cor(x, y, ...)
}

## This is deprecated after mosaic updates -------------------------
#' #' @rdname favstats
#' #' @export
#' fav_stats <- function(x, ..., na.rm = TRUE) {
#'   if (!is.null(dim(x)) && min(dim(x)) != 1)
#'     warning("Not respecting matrix dimensions.  Hope that's OK.")
#'   x <- as.vector(x)
#'   qq <- mosaic::quantile(x, na.rm = na.rm)
#'   val <- data.frame(qq[1], qq[2], qq[3], qq[4], qq[5],
#'                     base::mean(x,na.rm = na.rm),
#'                     stats::sd(x, na.rm = na.rm),
#'                     base::sum(!is.na(x)),
#'                     base::sum(is.na(x)))
#'   rownames(val) <- ""
#'   names(val) <- c("min", "Q1", "median", "Q3", "max", "mean",
#'                   "sd", "n", "missing")
#'   return(val)
#' }
