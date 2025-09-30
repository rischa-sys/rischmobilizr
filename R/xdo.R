#' A wrapper for the mosaic package's \code{do} function.
#'
#' Number of iterations is capped at 500 to prevent individual users from
#' hogging server computation time. Additional details about the \code{do}
#' function can be found in the \code{\link[mosaic]{do}} package documentation
#' @inheritParams mosaic::do
#' @return A data frame of the values for the repeated function.
#' @section Note: Find more examples, help and details at the help page for
#'   \code{\link[mosaic]{do}} in the \code{mosaic} package.
#' @examples
#' do(10) * sample(1:10, size=2)
#'
#' @importFrom mosaic do
#' @export

do <- function(N) {
  # If number of requested loops exceed 500, break and return a message.
  if (N > 2000) stop("Number of iterations must not exceed 2000. Choose a smaller number of iterations and try again.")

  # If number of requested loops is 500 or fewer, perform the loops.
  df <- mosaic::do(N, mode = 'data.frame')
  return(df)
}

