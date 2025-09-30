#' Biased Dice Experiment
#'
#' Roll dice to determine which, if any, is biased.
#'
#' @param die \code{"A"} or \code{"B"}. The die you wish to roll.
#' @param times Integer. The number of times you'd like to roll the die.
#'
#' @examples
#' # Roll a die once
#' roll_die("A")
#'
#' # Roll a die many times then create a histogram.
#' rolls <- roll_die("A", times = 100)
#' histogram(rolls, nint = 6)
#'
#' @export

roll_die <- function(die, times = 1) {

  # Make sure one of the appropriate dice is selected
  if (!die %in% c("A", "B")) {
    stop("Please set the die argument to be either 'A' or 'B'")
  }

  if (times > 2000) {
    stop("Number of rolls should be less than or equal to 2000")
  }

  # For "A" roll an unbiased die.
  if (die == "A") {
    rolls <- sample(1:6, size = times, replace = TRUE)
  }

  # For "B" roll a biased die.
  if (die == "B") {
    s_unfair <- 0.22
    s_fair <- (1 - s_unfair)/5
    probs <- rep(s_fair, 6)
    probs[3] <- s_unfair
    rolls <- sample(1:6, size = times, prob = probs, replace = TRUE)
  }

  # Print the rolls
  cat("Your roll(s):\n")
  rolls
}
