#' Make a map of points
#'
#' A function that creates a plot of points on a background map pulled from
#' Google Maps' API.
#'
#' Creates a map using a Google Maps basemap, with points of data on top. Points
#' can be colored differently and sized according to another variable
#' @param x Function of the form latitude~longitude
#' @param data Data frame containing latitude and longitude variables
#' @param scaleby A variable for bubbles to be scaled by in a bubble plot.
#' @param size Size of the points. Default is 1, larger numbers will make points larger.
#' @param col A specification for the plotting color. Defaults to "blue". You
#'   can provide any color name here, like "red", "purple", etc., in quotes. Or,
#'   you could use hex values like "FF00FF", again in quotes. See section 'Color
#'   Specification' in the \code{\link{par}} documentation for more.
#' @param dots graphical parameters to be passed to \code{\link{points}}. For
#'   example, you could change the size of the dots on the map by passing
#'   \code{cex=2}.
#' @return A map of points with a google map background
#' @examples
#' lat <- runif(10, 32, 42)
#' lon <- runif(10, -124, -114)
#' geodata <- data.frame(lat, lon)
#' scale <- sample(1:10, 10)
#' map(lat~lon, data=geodata)
#' map(lat~lon, data=geodata, scaleby = scale)
#' map(lat~lon, data=geodata, scaleby = scale, size = 5)
#' map(lat~lon, data=geodata, col = type)
#' map(lat~lon, data=geodata, scaleby = scale, col = type)

map <- function(x, data=parent.frame(), e, scaleby, size = 1, col='blue', zoom=10, color, ...){


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

  
  latitude <- data[, deparse(lhs(x))]
  longitude <- data[, deparse(rhs(x))]
  projected <- data.frame(longitude, latitude)

  if(!missing(color)) {
    stop("Remember to use the argument 'col' and not 'color'.")
  }

  latrange <- extendrange(latitude, f=0.04)
  lonrange <- extendrange(longitude, f=0.04)
  x <- c(lonrange[1], latrange[1], lonrange[2], latrange[2])
  f1 <- (latrange[2] - latrange[1])/(lonrange[2] - lonrange[1])
  if (f1 < 1/4) {
    latrange <- extendrange(latitude, f = 1.5 - f1)
    x <- c(lonrange[1], latrange[1], lonrange[2], latrange[2])
  }
  if (f1 > 5/4) {
    lonrange <- extendrange(longitude, f = f1 - 1)
    x <- ec(lonrange[1], latrange[1], lonrange[2], latrange[2])
  }

  r <- ggmap::get_map(x, source="stamen")
  p <- ggmap::ggmap(r)
  if(!missing(scaleby)){
    projected$scaleby <- scaleby
    print(p + geom_point(data=projected, aes(x=longitude, y=latitude, size=scaleby), colour=col))
  }else{
    print(p + geom_point(data=projected, aes(x=longitude, y=latitude), size=size, colour=col))
  }
}
