#' Cleans and formats data from the Time Use campaign.
#'
#' Cleans and formats data from the Time Use campaign. Each observation will be
#' the average time spent participating in various activities over the data
#' collection period for each user.
#' @param data Data frame. The unaltered Time Use campaign data that is
#'   downloaded, uploaded and loaded from the Mobilize landing page.
#' @return The average time spent participating in various activities for each
#'   participant.
#' @section How it works: The code cleans and aggregates the data in a 3-step
#'   process. \enumerate{ \item The first step is to do some preliminary
#'   cleaning of the data. This includes a step to turn "NOT_DISPLAYED" values
#'   into 0's. This is done because when a user does not select a category to
#'   note the amount of time they've participated, it's assumed that the reason
#'   is because they've  participated in the activity for 0 minutes. \item The
#'   next step is to sum the amount of time spent doing the various activities
#'   for each day. This then represents the total amount of time spent
#'   performing the various activities over the course of each day. \item
#'   Finally, the third step is to take the summed times for each day from step
#'   2. and average them over the total number of days they participated in the
#'   campaign.}
#' @examples
#' \dontrun{
#' timeuse_raw <- read.csv("My class time use data.csv")
#' timeuse <- timeuse_format(timeuse_raw)
#' }
#'
#' @importFrom dplyr group_by
#' @importFrom dplyr summarise
#' @importFrom magrittr %>%
#' @export

timeuse_format <-
  function (data)
  {
    names(data) <- gsub(names(data), pattern = ":", replacement = ".")
    time_use_vars <- c("chores", "communicate", "day", "extra", "grooming",
                       "homework", "meals", "purchases", "read", "school", "sleep",
                       "social_media","socialize", "spiritual", "sports", "television",
                       "travel", "videogames", "work")
    clean_data <- sapply(data, function(x) gsub(x, pattern = "NOT_DISPLAYED",
                                                replacement = 0))
    clean_data <- as.data.frame(clean_data, stringsAsFactors = FALSE)
    clean_data[time_use_vars] <- sapply(clean_data[time_use_vars],
                                        as.numeric)
    sum_timeuse <- clean_data %>% group_by(user.id, day) %>%
      summarise(submissions = n(), chores = sum(chores),
                communicate = sum(communicate), extra = sum(extra),
                grooming = sum(grooming), homework = sum(homework),
                meals = sum(meals), purchases = sum(purchases), read = sum(read),
                school = sum(school), sleep = sum(sleep), social_media = sum(social_media),
                socialize = sum(socialize), spiritual = sum(spiritual), sports = sum(sports),
                television = sum(television), travel = sum(travel),
                videogames = sum(videogames), work = sum(work))
    mean_timeuse <- sum_timeuse %>% group_by(user.id) %>% summarise(submissions = sum(submissions), chores = mean(chores),
                                                                    communicate = mean(communicate), extra = mean(extra),
                                                                    grooming = mean(grooming), homework = mean(homework),
                                                                    meals = mean(meals), purchases = mean(purchases), read = mean(read),
                                                                    school = mean(school), sleep = mean(sleep), social_media = mean(social_media),
                                                                    socialize = mean(socialize), spiritual = mean(spiritual), sports = mean(sports),
                                                                    television = mean(television), travel = mean(travel),
                                                                    videogames = mean(videogames), work = mean(work))
    return(mean_timeuse)
  }
