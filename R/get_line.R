#' Calculate Slope-Intercept Form of a Line
#'
#' Given two points, calculates the slope-intercept form of a line.
#'
#' @param p1 The coordinates of the first point, specified as a numeric vector of length 2.
#' @param p2 The coordinates of the second point, specified as a numeric vector of length 2.
#' @return A named numeric vector containing the intercept and slope of the line.
#' @export
#' @examples
#' p1 <- c(0, 0)
#' p2 <- c(1, 1)
#' get_line(p1, p2)
#' # Output: intercept = 0, slope = 1
get_line <- function(p1, p2) {
  # Check if p1 and p2 are numeric vectors of length 2
  if (!is.numeric(p1) || length(p1) != 2 || !is.numeric(p2) || length(p2) != 2) {
    stop("Both p1 and p2 must be numeric vectors of length 2.")
  }
  
  # Check if the two points are distinct
  if (identical(p1, p2)) {
    stop("The two points must be distinct.")
  }
  
  # Handle vertical line case
  if (p1[1] == p2[1]) {
    stop("Vertical line: slope is undefined (infinite).")
  }
  
  # Calculate slope and intercept
  m <- (p2[2] - p1[2]) / (p2[1] - p1[1])
  b <- p1[2] - m * p1[1]
  
  return(c("intercept" = b, "slope" = m))
}
