#' Quick Import From Ohmage
#'
#' @examples
#' quick_import() # Interactively 
#' quick_import(server = "https://ids.mobilizingcs.org/", username = "guestx", campaign = "urn:class:PD:2017:fall:guestx:ids:p1:foodhabits") # NOT RUN 
#' quick_import(server = "https://ids.mobilizingcs.org/", username = "guestx", token = token, campaign = "urn:class:PD:2017:fall:guestx:ids:p1:foodhabits") # IF YOU HAVE TOKEN # NOT RUN 
#'
#' @export

quick_import <- function(server = NULL, username = NULL, token = NULL, campaign = NULL) {

  message("Loading Dataset from Servers...")
  if(is.null(server)){
    server_list <- c("https://tools.idsucla.org",
                     "https://mobilize.lausd.net",
                     "https://ids.lbschools.net",
                     "https://ids.centinela.k12.ca.us",
                     "https://sandbox.mobilizingcs.org",
                     "others")

    server_no <- menu(server_list, title = "Enter the number next to the server you would like to load")

    if(server_no==0) return (NULL)

    server <- toString(server_list[server_no])

  }


  while(nchar(server)<8 || server=="others" ){
    server <- readline(prompt="Enter server: ")
  }

  message(server)


  if(is.null(username)){

    username_list <- c("others")
    username_no <- 1

    if(nchar(toString(Sys.getenv("LOGNAME")))>2){
      username_list <- c(toString(Sys.getenv("LOGNAME")), "others")
      username_no <- menu(username_list, title = "Enter the number next to the user you would like to load")
    }

    if(username_no==0) return (NULL)

    username <- toString(username_list[username_no])

  }

  while(nchar(username)<2 || username=="others" ){
    username <- readline(prompt="Enter username: ")
  }

  message(username)

  if(is.null(token)){
    password <- rstudioapi::askForPassword(paste("Please enter your password for", username))

    POST1 <- httr::POST( paste0(server,'/app/user/auth_token'), body = list(user=username, password=password, client='curl'))

    if(POST1$status_code!=200){
      message(paste("Connection Invalid: Check and try again?"))
      return (NULL)
    }

    POST1C <- httr::content(POST1)

    if(POST1C$result!="success"){
      message(paste("Auth Invalid: Check and try again?"))
      return (NULL)
    }

    token <- POST1C$token
    .GlobalEnv$token <- token
  }

  POST2 <- httr::POST( paste0(server,'/app/campaign/read'), body = list(user=username, auth_token=token, output_format='short', client='curl'))

  if(POST2$status_code!=200){
    message(paste("Connection Invalid: Check and try again?"))
    return (NULL)
  }

  POST2C <- httr::content(POST2)

  if(POST2C$metadata$number_of_results==0){
    message(paste("This user has no campaign. "))
    return (NULL)
  }

  campaign_list <- sort(unlist(POST2C$metadata$items))

  if(!isTRUE(campaign %in% campaign_list )){

    campaign_no <- menu(campaign_list, title = "Enter the number next to the campaign you would like to load")

    if(campaign_no==0) return (NULL)

    campaign <- toString(campaign_list[campaign_no])

  }

  message(campaign)

  POST3 <- httr::POST( paste0(server,'/app/survey_response/read'), body = list(user=username, auth_token=token, campaign_urn=campaign, column_list="urn:ohmage:special:all", user_list="urn:ohmage:special:all", prompt_id_list="urn:ohmage:special:all", output_format = 'csv', suppress_metadata = "true", client='curl'))

  if(POST3$status_code!=200){
    message(paste("Connection Invalid: Check and try again?"))
    return (NULL)
  }

  csv_path0 <- paste0(gsub("[[:punct:]]", "_", campaign),"")
  csv_path_raw <- paste0(gsub("[[:punct:]]", "_", campaign),".raw.csv")
  csv_path <- paste0(gsub("[[:punct:]]", "_", campaign),".csv")

  writeLines(rawToChar(POST3$content), csv_path_raw)

  message(paste("File Saved:", csv_path_raw))

  library(readr)
  MyData <- read_csv(csv_path_raw)

  try(
    MyData <- MyData[MyData$survey.privacy_state == 'shared',]
    ,silent=TRUE)

  delist <- c("survey.id", "survey.title", "survey.description", "client", "utc_timestamp", "epoch_millis", "date", "timezone", "provider", "timestamp.1", "timezone.1", "accuracy", "survey.privacy_state", "launch_long", "launch_short", "survey_response.id", "status")

  for (cc in 1:length(delist)){
    try(
      MyData <- MyData[,!( names(MyData) %in% c(delist[cc]) )]
      ,silent=TRUE)
  }


  write.csv(MyData, file = csv_path, row.names=FALSE)
  
  MyData <- read_csv(csv_path)

  message(paste("File Saved:", csv_path))

  .GlobalEnv[[csv_path0]] <-  MyData

  message(paste("csv Imported:", csv_path0))

  eval(parse(text=paste0("View(", csv_path0, ")")), envir=.GlobalEnv)
}


