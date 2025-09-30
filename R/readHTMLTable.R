#' Read data from one or more HTML tables
#'
#' Read data from one or more HTML tables
#' 
#' @param url 
#' @param which An integer to choose which table to return. If not specified, return a list of tables.
#' @param ... see \code{rvest::html_table()}
#'
#'
#' @examples
#' # Get a list of Tables
#' url <- "https://www.inflationdata.com/Inflation/Consumer_Price_Index/HistoricalCPI.aspx"
#' readHTMLTable(url)
#'
#' # Read the 1st Table
#' url <- "https://www.inflationdata.com/Inflation/Consumer_Price_Index/HistoricalCPI.aspx"
#' readHTMLTable(url, which = 1)
#'
#' @export

readHTMLTable <- function(url, which, ...) {
  the_html <- read_html(httr::GET(url, config = httr::config(ssl_verifypeer = FALSE)))
  
  if (!missing(which)) {
    table <- rvest::html_table(the_html, ...)[[which]]
  } else {
    table <- rvest::html_table(the_html, ...)
  }

  
  return(table)
}