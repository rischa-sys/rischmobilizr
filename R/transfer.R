#' transfer_upload
#'
#' Share files - Upload via transfer.sh
#'
#' @param file path to an existing file on disk
#'
#' @examples
#'
#' transfer_upload("my.csv") # NOT RUN
#'
#' @export
transfer_upload <- function(file = NULL){
  if(is.null(file)||!file.exists(file)){
    message(paste("File Invalid:", toString(file)))
    return (NULL)
  }
  file <- normalizePath(file)
  base_name <- basename(file)
  message(paste("File Ready, Processing Upload:", file))
  curl_status <- curl::curl_upload(file, 'https://transfer.sh/', verbose = FALSE)
  if(curl_status$status_code!=200){
    message(paste("Upload Invalid: Check and try again?"))
    return (NULL)
  }
  addr <- rawToChar(curl_status$content)
  passcode <- basename(dirname(addr))
  message("==Upload Success==")
  message(paste0("Sample #1: transfer_download(\'", base_name,"\', passcode=\'", passcode, "\')" ))
  message(paste0("Sample #2 (alternatively): transfer_download(\'", passcode, "/", base_name,"\')" ))
  message(paste0("Sample #3 (with rename): transfer_download(\'", base_name,"\', passcode=\'", passcode, "\', destfile = \'", paste0(base_name, ".copy") ,"\')" ))
  message(paste0("Sample #4 (alternatively): transfer_download(\'", passcode, "/", base_name,"\', destfile = \'", paste0(base_name, ".copy") ,"\')" ))
  message(paste0("Sample #5 (open in browser): ", addr ))
  message("Link expires in 14 days.")
  return (addr)
}

#' transfer_download
#'
#' Share files - Download via transfer.sh
#'
#' @param url A character string naming the URL of a resource to be downloaded
#' @param passcode A character string provided by transfer_upload()
#' @param destfile	A character string with the name where the downloaded file is saved. Tilde-expansion is performed.
#'
#' @examples
#'
#' transfer_download('do.R', passcode='ayU4v') # NOT RUN
#' transfer_download('ayU4v/do.R') # NOT RUN
#' transfer_download('do.R', passcode='ayU4v', destfile = 'do.R.copy') # NOT RUN
#' transfer_download('ayU4v/do.R', destfile = 'do.R.copy') # NOT RUN
#'
#' @export
transfer_download <- function(url = NULL, passcode = NULL, destfile = NULL){
  if(is.null(url)){
    message("Invalid url")
    return (NULL)
  }
  if(is.null(passcode)){
    passcode <- basename(dirname(url))
    if(passcode=='.'){
      message("Invalid passcode")
      return (NULL)
    }
  }
  base_name <- basename(url)
  extended_url <- paste0("https://transfer.sh/", passcode, "/", base_name)
  if(is.null(destfile)){
    destfile <- base_name
  }
  tryCatch({
    download.file(extended_url, destfile = destfile)
    message(paste0("File downloaded to: \'", normalizePath(destfile), "\'"))
    return (normalizePath(destfile))
  }, error=function(err) {
    tryCatch({
      download.file(extended_url, destfile = destfile, method = 'curl')
      message(paste0("File downloaded to: \'", normalizePath(destfile), "\'"))
      return (normalizePath(destfile))
    }, error=function(err1) {
      tryCatch({
        download.file(extended_url, destfile = destfile)
        message(paste0("File downloaded to: \'", normalizePath(destfile), "\'"))
        return (normalizePath(destfile))
      }, error=function(err2) {
        message(err2)
        return (NULL)
      })
    })
  })
}

