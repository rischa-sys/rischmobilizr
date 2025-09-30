#' Load built-in datasets.
#'
#' Load built-in data sets. After loading the data, the data frame will popup in
#' an RStudio window to view.
#'
#' @param ... The name of the dataset to load.
#'
#' @return Loads the dataset and opens it up to view.
#'
#' @examples
#' data(cdc)

data <- function(...) {

  # In order to avoid some recurring errors, we check to see whether the call
  # should or should not open the View() window.
  args <- list(...)
  exist <- length(args)
  if (exist != 0) {
    if (is.null(args[['package']])) {
      View(...)
    }
  }
  utils::data(...)
}

#' Load external datasets.
#'
#' Load external data sets. After loading the data, the data frame will popup in
#' an RStudio window to view. For more detail, check curriculum.idsucla.org/data
#'
#' @param data The name of the dataset to load.
#'
#' @return Loads the dataset and opens it up to view.
#'
#' @examples
#' extra_data('cdc')
#' @export

extra_data <- function(data) {
  var_name=load(url(paste("http://curriculum.idsucla.org/data/",data,".rda",sep="")), envir = .GlobalEnv)
  # if (exists(var_name, envir = .GlobalEnv)) {
  #   View(get(var_name, envir = .GlobalEnv))
  # }
}

