#' Time Perception Stopwatch
#'
#' A stopwatch to use for the time perception experiment. To use, simply run the
#' function \code{stopwatch()} and then hit enter at the desired time to stop
#' the timer.
#'
#' @examples
#' \dontrun{
#' stopwatch()
#' }
#' @export
stopwatch <- function () {
  tic <- Sys.time()
  cat("The stopwatch has started. \n Press [enter] to stop it.")
  line <- readline()
  toc <- Sys.time() - tic
  #print(toc)
  elapsed_seconds <- as.numeric(toc, units = "secs")
  cat("Time difference of ", elapsed_seconds, " secs\n")
}
