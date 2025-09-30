#' Update stopword lists with new words.
#'
#' An easy way to update stopword lists with new words. Either update preloaded stopwords lists, i.e. "en" or "SMART" or update your own custom stopwords list.
#' @param words A vector of words, formatted as strings, to add to the 'stopword.set'.
#' @param stopword.set choose a preloaded stopwords list, i.e. "en" (default) or "SMART" or a vector of custom stopwords.
#' @return a new list of stopwords
#' @examples
#' data("crude")   # Load text data
#' WordFreqTable(crude, top = 10)    # Notice the top 10 words
#' crude <- ProcessText(crude, removestopwords = TRUE)   # Remove the "en" stopwords
#' WordFreqTable(crude, top = 10)    # Notice the new top 10 words
#' WordsToInclude <- c("said", "mln", "bpd", "dlrs")    # Words we want to include in our stoplist
#' UpdatedStopwords <- UpdateStopwords(words = WordsToInclude)   # Add words to "en" stoplist
#' crude <- ProcessText(crude, stopwords.list = UpdatedStopwords, removestopwords = TRUE)
#' WordFreqTable(crude, top = 10)    # Notice the new top 10 words
#' ## Example using the "SMART" stopword list
#' data("crude")   # Load text data
#' WordFreqTable(crude, top = 10)    # Notice the top 10 words
#' crude <- ProcessText(crude, stopwords.list = stopwords("SMART"), removestopwords = TRUE)   # Remove the "SMART" stopwords
#' WordFreqTable(crude, top = 10)    # Notice the new top 10 words
#' WordsToInclude <- c("mln", "bpd", "dlrs")    # Words we want to include in our stoplist
#' UpdatedStopwords <- UpdateStopwords(words = WordsToInclude, stopword.set = "SMART")   # Add words to "SMART" stoplist
#' crude <- ProcessText(crude, stopwords.list = UpdatedStopwords, removestopwords = TRUE)
#' WordFreqTable(crude, top = 10)    # Notice the new top 10 words
#'



update_stopwords <- function(words, stopword.set = "en") {
  if(class(words) != "character") {
    stop("Words you'd like to add as stopwords should be formatted as strings, i.e.
         c('alpha', 'beta', 'gamma)'")
  }
  if (class(stopword.set) != "character") {
    stop("'stopword.set' should be set to one of the default stopword lists,
         e.g. 'en' or 'SMART', or should be formatted as a string, i.e.
         c('alpha', 'beta', 'gamma)'")
  }
  if ((stopword.set == "en") || (stopword.set == "SMART")) {
    new.stopwords <- append(stopwords(kind = stopword.set), words)
  }
  else {new.stopwords <- append(stopword.set, words)
  }
  return(new.stopwords)
  }
