#' Reads comma-seperated value data files into R.
#'
#' Reads comma-seperated value data files into R. This function is specially made to work with Mobilize participatory sensing data. After loading the data, certain variable names are cleaned and others are automatically dropped. To read csv data in using the default method, run \code{utils::read.csv()}.
#' @inheritParams utils::read.csv
#' @examples
#' \dontrun{
#' food <- read.csv("Food_Habits_Data.csv")
#' }
#' @export

# Because of changes in RStudio that occurred between LAUSD adopting
# IDS and other districts, we create two functions read.csv() and
# read_csv() so that the different versions of RStudio act similarly.

read.csv <- function(file, ...) {

  # Names of participatory sensing variables to look for.
  ps_names <- c("user.id", "context.timestamp", "context.location.latitude",
                "context.location.longitude")

  # Read the data in using the standard read.csv() function
  df <- utils::read.csv(file = file, ...)

  # If the participatory sensing campaigns are included in the data:
  # 1) Remove the ".key" variables
  if (all(ps_names %in% names(df))) {
    if(length(grep(".key", names(df))) > 0) {
      df <- df %>% dplyr::select(-ends_with(".key"))
    }
    # 2) Remove the ".label", "context." and "location." characters from the
    # Variable names
    names(df) <- gsub(names(df), pattern = ".label", replace = "")
    names(df) <- gsub(names(df), pattern = "context.", replace = "")
    names(df) <- gsub(names(df), pattern = "location.", replace = "")
  }

  # Return the data
  return(df)
}

#' @rdname read.csv
#' @export

read_csv <- function(file, ...) {

  # Names of participatory sensing variables to look for.
  ps_names <- c("user.id", "context.timestamp", "context.location.latitude",
                "context.location.longitude")

  # Read the data in using the standard read.csv() function
  df <- utils::read.csv(file = file, ...)

  # If the participatory sensing campaigns are included in the data:
  # 1) Remove the ".key" variables
  if (all(ps_names %in% names(df))) {
    if(length(grep(".key", names(df))) > 0) {
      df <- dplyr::select(df, -ends_with(".key"))
    }
  # 2) Remove the ".label", "context." and "location." characters from the
  # Variable names
    names(df) <- gsub(names(df), pattern = ".label", replace = "")
    names(df) <- gsub(names(df), pattern = "context.", replace = "")
    names(df) <- gsub(names(df), pattern = "location.", replace = "")
  }

  # Return the data
  return(df)
}
