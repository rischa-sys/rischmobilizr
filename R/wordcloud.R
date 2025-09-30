#' Word Clouds
#'
#' Create a word cloud.
#'
#' @param x \code{formula}. An expression of the form \code{y ~ x} where \code{x} is the words we are selecting and \code{y} is the frequency with which the word occurs. Note that \code{x} and \code{y} are the variable names where these are located.
#' @param data data frame. The data where the variables can be found.
#' @param max.words argument. An argument to designate a different max number of words to plot in the word cloud (default is 100).
#' @param color argument. An argument to designate the color of the word cloud.
#' @section Note:
#' For additional options for creating word clouds, look at the help file found in the \code{wordcloud} package or run \code{help(wordcloud, package="wordcloud")} in the console. ALSO: note in the examples that data must be formatted a particular way in order to use the wordcloud function.
#' @examples
#' data(pride)
#' pride <- tally(~word, data = pride, format = "data.frame")
#' wordcloud(Freq ~ word, data = pride)
#'
#' @export

wordcloud <- function(formula, data, max.words = 100, color = "black") {
    color <- eval(substitute(color), data, environment(formula))
    if(length(color) == 1) {
        wordcloud::wordcloud(words = data[, all.vars(formula)[2]], freq = data[, all.vars(formula)[1]], max.words = max.words, colors = color)
    }
    else {
        wordcloud::wordcloud(words = data[, all.vars(formula)[2]], freq = data[, all.vars(formula)[1]], max.words = max.words, colors = color, ordered.colors = TRUE)
    }
}