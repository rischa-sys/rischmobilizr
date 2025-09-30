#' A set of logging functions that Steve made.
#' These are and aren't used by the user.
#' @import log4r


log_config <- log4r::create.logger()

.onLoad <- function(libname, pkgname) {
  # set default na.action level, that is used by user.
  options(na.action = "na.exclude")
  log_path <- Sys.getenv('MOBILIZR_LOGFILE', '~/.mobilizr.log')
  logger_set(file.path(log_path), "INFO")
}

logger_set <- function(logfile = NULL, level = NULL){
  log4r::logfile(log_config) <<- logfile
  log4r::level(log_config) <<- level
}

log_debug <- function(msg){
  log4r::debug(log_config, msg)
}

log_info <- function(msg){
  log4r::info(log_config, msg)
}

log_warn <- function(msg){
  log4r::warn(log_config, msg)
}

log_error <- function(msg){
  log4r::error(log_config, msg)
}

log_fatal <- function(msg){
  log4r::fatal(log_config, msg)
}
