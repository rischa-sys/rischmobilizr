#' Load lab slides into RStudio's Viewer Pane
#'
#' Load lab slides into RStudio's Viewer Pane. The function can either be called
#' without an argument \code{load_lab()}, in which case a menu will appear in
#' the console for the user to make their selection, or with a number indicating
#' the desired lab as an argument.
#'
#' @param lab Integer (optional). Including an integer will load the desired lab
#'   automatically. Leaving the argument blank will load a menu for users to
#'   make a selection from.
#' @examples
#' load_lab() # Loads a menu to choose from
#' load_lab(1) # Automatically loads the 1st lab from the menu.
#' load_labs() # Similar to load_lab()
#' load_labs(1) # Similar to load_lab(1)
#'
#' @importFrom curl curl
#' @importFrom rstudioapi viewer
#' @importFrom stringr str_extract
#' @export

load_lab <- function(lab) {
  # Create the names of the labs for the menu
  unit_1_titles <- c("Lab 1A", "Lab 1B", "Lab 1C", "Lab 1D", "Lab 1E", "Lab 1F",
                     "Lab 1G", "Lab 1H")
  unit_2_titles <- c("Lab 2A", "Lab 2B", "Lab 2C", "Lab 2D", "Lab 2E", "Lab 2F",
                     "Lab 2G", "Lab 2H", "Lab 2I")
  unit_3_titles <- c("Lab 3A", "Lab 3B", "Lab 3C", "Lab 3D", "Lab 3E", "Lab 3F")
  unit_4_titles <- c("Lab 4A", "Lab 4B", "Lab 4C", "Lab 4D", "Lab 4E", "Lab 4F",
                     "Lab 4G", "Lab 4H")

  # These were added because the labs used during Summer PDs used different
  # campaigns than the actual labs. At some point it'd be nice to remove
  # these labs.
  pd_lab_titles <- c("PD Lab 1D",
                     "PD Lab 1E")

  # Put lab titles together.
  lab_titles <- c(unit_1_titles, unit_2_titles, unit_3_titles, unit_4_titles,
                  pd_lab_titles)

  # If the user specifies a lab file in load_lab(), grab the lab URL.
  if (!missing(lab)) {
    url <- .lab_selector(lab=lab, lab_titles = lab_titles)
  }

  # Otherwise, open a menu for the user to select from and grab the lab URL.
  if (missing(lab)) {
    url <- .lab_selector(lab=NULL, lab_titles = lab_titles)
  }

  # Using the URL for the chosen lab, the following reads in the lab's HTML
  # and displays it in the Viewer pane.

  # Get HTML
  con <- curl(url, "r")
  page <- paste(readLines(con), collapse = '\n')
  close(con)

  # Create a temp HTML file
  tf <- tempfile(fileext = ".html")
  writeLines(page, tf)

  # Display HTML file in the viewer pane.
  viewer(tf)

  # auto update
  # mobilizr::update_mobilizr();
}

#' @rdname load_lab
#' @export
load_labs <- function(lab) {
  # Alias to avoid problems with load_lab vs. load_labs in the written
  # curriculum
  load_lab(lab)
}

.format_lab_title <- function (x) {
  # Helper function that formats the lab title, from the menu, to the lab
  # file format for the html.
  x_lower <- tolower(x)
  x_nospace <- gsub(x = x_lower, pattern = ' ', replacement = "")
  clean_title <- gsub(x = x_nospace, pattern = "-", replacement = "")

  unit_num <- str_extract(clean_title, "[1-4]")
  lab_location <- paste0("unit_", unit_num, "/", clean_title, "/", clean_title)
  return(lab_location)
}

# Helper function that displays the actual menu to choose labs from.
.lab_selector <- function(lab, lab_titles) {

  # Since LAUSD and CSVD might not update as often as the IDS server,
  # we can keep separate branches of the labs so we can pull the labs
  # based on their versions of Rstudio

  # if (rstudioapi::getVersion() == "0.99.902") {
  #   # LAUSD: Pull labs from their branch
  #   # NOTE: Make sure to update the Rstudio version on the line above
  #   lab_urls <- paste0("https://raw.githubusercontent.com/IDSUCLA/ids_labs/lausd-labs/",
  #                      .format_lab_title(lab_titles), '.html')
  # } else if (rstudioapi::getVersion() == "0.99.902") {
  #   # CVSD: Pull labs from their branch
  #   # NOTE: Make sure to update the Rstudio version on the line above
  #   lab_urls <- paste0("https://raw.githubusercontent.com/IDSUCLA/ids_labs/cvsd-labs/",
  #                      .format_lab_title(lab_titles), '.html')
  # } else {
  #   # IDS Server: Pull labs from the master branch
  #   lab_urls <- paste0('http://gh.IDSUCLA.org/ids_labs/',
  #                      .format_lab_title(lab_titles), '.html')
  # }

  lab_urls <- paste0('https://raw.githubusercontent.com/mobilizingcs/ids_labs/master/',
                     .format_lab_title(lab_titles), '.html')

  if (is.null(lab)) {
    # If user doesn't specify a lab to open in load_lab(), prompt them.
    selection <- menu(lab_titles, title = "Enter the number next to the lab you would like to load:")
    .log_loaded_lab(selection)
    url <- lab_urls[selection]
  }

  if (!is.null(lab)) {
	if(is.character(lab) && isTRUE(nchar(lab) == 2) ){
		LAB <- paste0("Lab ", toupper(lab))
		if (isTRUE(match(LAB,lab_titles) > 0)){
			lab <- match(LAB,lab_titles)
		}else{
			stop("Input should be either left blank or a single integer..")
		}
	}else if(is.character(lab) && isTRUE(nchar(lab) > 2) ){
		LAB <- paste0(toupper(lab))
		if (isTRUE(match(LAB,toupper(lab_titles)) > 0)){
			lab <- match(LAB,toupper(lab_titles))
		}else{
			stop("Input should be either left blank or a single integer...")
		}
	}else if (!is.numeric(lab) | length(lab) != 1) {
      # If user puts something that's not a number associated with a lab,
      # give the user an error message.
      stop("Input should be either left blank or a single integer.")
    }
    .log_loaded_lab(lab)
    url <- lab_urls[lab]
  }
  return(url)
}

.log_loaded_lab <- function(lab) {
  # logs the load_lab command correctly regardless of how the user selected a lab
  log_info(paste('load_lab(',lab,')', sep = ""))
}


#' Load new lab slides into RStudio's Viewer Pane
#'
#' Load new lab slides into RStudio's Viewer Pane. The function can either be called
#' without an argument \code{load_new_lab()}, in which case a menu will appear in
#' the console for the user to make their selection, or with a number indicating
#' the desired lab as an argument.
#'
#' @param lab Integer (optional). Including an integer will load the desired lab
#'   automatically. Leaving the argument blank will load a menu for users to
#'   make a selection from.
#' @examples
#' load_new_lab() # Loads a menu to choose from
#' load_new_lab(1) # Automatically loads the 1st lab from the menu.
#' load_new_labs() # Similar to load_new_lab()
#' load_new_labs(1) # Similar to load_new_lab(1)
#'
#' @importFrom curl curl
#' @importFrom rstudioapi viewer
#' @importFrom stringr str_extract
#' @export

load_new_lab <- function(lab) {
  # Create the names of the labs for the menu
  unit_1_titles <- c("Lab 1A", "Lab 1B", "Lab 1C", "Lab 1D", "Lab 1E", "Lab 1F",
                     "Lab 1G", "Lab 1H")
  unit_2_titles <- c("Lab 2A", "Lab 2B", "Lab 2C", "Lab 2D", "Lab 2E", "Lab 2F",
                     "Lab 2G", "Lab 2H", "Lab 2I")
  unit_3_titles <- c("Lab 3A", "Lab 3B", "Lab 3C", "Lab 3D", "Lab 3E", "Lab 3F")
  unit_4_titles <- c("Lab 4A", "Lab 4B", "Lab 4C", "Lab 4D", "Lab 4E", "Lab 4F",
                     "Lab 4G", "Lab 4H")

  # These were added because the labs used during Summer PDs used different
  # campaigns than the actual labs. At some point it'd be nice to remove
  # these labs.
  #pd_lab_titles <- c("PD Lab 1D",
  #                   "PD Lab 1E")

  pd_lab_titles <- c()

  # Put lab titles together.
  lab_titles <- c(unit_1_titles, unit_2_titles, unit_3_titles, unit_4_titles,
                  pd_lab_titles)

  # If the user specifies a lab file in load_new_lab(), grab the lab URL.
  if (!missing(lab)) {
    url <- .new_lab_selector(lab=lab, lab_titles = lab_titles)
  }

  # Otherwise, open a menu for the user to select from and grab the lab URL.
  if (missing(lab)) {
    url <- .new_lab_selector(lab=NULL, lab_titles = lab_titles)
  }

  # Using the URL for the chosen lab, the following reads in the lab's HTML
  # and displays it in the Viewer pane.

  # Get HTML
  con <- curl(url, "r")
  page <- paste(readLines(con), collapse = '\n')
  close(con)

  # Create a temp HTML file
  tf <- tempfile(fileext = ".html")
  writeLines(page, tf)

  # Display HTML file in the viewer pane.
  viewer(tf)
}

#' @rdname load_new_lab
#' @export
load_new_labs <- function(lab) {
  # Alias to avoid problems with load_new_lab vs. load_new_labs in the written
  # curriculum
  load_new_lab(lab)
}

# Helper function that displays the actual menu to choose labs from.
.new_lab_selector <- function(lab, lab_titles) {

  lab_urls <- paste0('https://raw.githubusercontent.com/mobilizingcs/ids_labs/master/',
                     .format_lab_title(lab_titles), 'Rev.html')

  if (is.null(lab)) {
    # If user doesn't specify a lab to open in load_lab(), prompt them.
    selection <- menu(lab_titles, title = "Enter the number next to the lab you would like to load:")
    .log_loaded_lab(selection)
    url <- lab_urls[selection]
  }

  if (!is.null(lab)) {
	if(is.character(lab) && isTRUE(nchar(lab) == 2) ){
		LAB <- paste0("Lab ", toupper(lab))
		if (isTRUE(match(LAB,lab_titles) > 0)){
			lab <- match(LAB,lab_titles)
		}else{
			stop("Input should be either left blank or a single integer..")
		}
	}else if(is.character(lab) && isTRUE(nchar(lab) > 2) ){
		LAB <- paste0(toupper(lab))
		if (isTRUE(match(LAB,toupper(lab_titles)) > 0)){
			lab <- match(LAB,toupper(lab_titles))
		}else{
			stop("Input should be either left blank or a single integer...")
		}
	}else if (!is.numeric(lab) | length(lab) != 1) {
      # If user puts something that's not a number associated with a lab,
      # give the user an error message.
      stop("Input should be either left blank or a single integer.")
    }
    .log_loaded_lab(lab)
    url <- lab_urls[lab]
  }
  return(url)
}


#' @rdname load_pd
#' @export
load_pds <- function(lab) {
  load_pd(lab)
}

#' Load new pd lab slides into RStudio's Viewer Pane
#'
#' Load new pd lab slides into RStudio's Viewer Pane. The function can either be called
#' without an argument \code{load_pd()}, in which case a menu will appear in
#' the console for the user to make their selection, or with a number indicating
#' the desired lab as an argument.
#'
#' @param lab Integer (optional). Including an integer will load the desired pd lab
#'   automatically. Leaving the argument blank will load a menu for users to
#'   make a selection from.
#' @examples
#' load_pd() # Loads a menu to choose from
#' load_pd(1) # Automatically loads the 1st lab from the menu.
#' load_pds() # Similar to load_pd()
#' load_pds(1) # Similar to load_pd(1)
#'
#' @importFrom curl curl
#' @importFrom rstudioapi viewer
#' @importFrom stringr str_extract
#' @export

load_pd <- function(lab) {

  lab_titles_A <- c("PD lab 1d", "PD lab 1e")
  lab_urls_A <- paste0('https://raw.githubusercontent.com/mobilizingcs/ids_labs/master/',
                       "/unit_1/", gsub(x = tolower(lab_titles_A), pattern = ' ', replacement = ""), "/" , gsub(x = tolower(lab_titles_A), pattern = ' ', replacement = ""), 'Rev.html')

  lab_titles_B <- c("Advanced PD 1a", "Advanced PD 1b", "Advanced PD 2a", "Advanced PD 2b")
  lab_titles_BB <- c("AdvPD1a", "AdvPD1b", "AdvPD2a", "AdvPD2b")
  lab_urls_B <- paste0('https://raw.githubusercontent.com/mobilizingcs/ids_labs/master/',
                     "/AdvPD/", lab_titles_BB, "/lab" , lab_titles_BB, 'Rev.html')

  lab_titles <- c(lab_titles_A, lab_titles_B)
  lab_urls <- c(lab_urls_A, lab_urls_B)

  lab_titles <- c(lab_titles_A, lab_titles_B)

  if (missing(lab)) {
    lab <- menu(lab_titles, title = "Enter the number next to the lab you would like to load:")
  }

  .log_loaded_pd(lab)
  if (assertthat::is.string(lab)) {
	url <- paste0('https://raw.githubusercontent.com/mobilizingcs/ids_labs/master/',
                     "/AdvPD/" , toString(lab), '.html')
  }else if (lab>6) {
	url <- paste0('https://raw.githubusercontent.com/mobilizingcs/ids_labs/master/',
                     "/AdvPD/labAdvPD" , toString(lab), 'Rev.html')
  }else{
	url <- lab_urls[lab]
  }

  con <- curl(url, "r")
  page <- paste(readLines(con), collapse = '\n')
  close(con)

  tf <- tempfile(fileext = ".html")
  writeLines(page, tf)

  viewer(tf)


}


#' @rdname extra_lab
#' @export
extra_labs <- function(lab) {
  extra_lab(lab)
}

#' Load new extra lab slides into RStudio's Viewer Pane
#'
#' Load new extra lab slides into RStudio's Viewer Pane. The function can either be called
#' without an argument \code{extra_lab()}, in which case a menu will appear in
#' the console for the user to make their selection, or with a number indicating
#' the desired lab as an argument.
#'
#' @param lab Integer/String (optional). Including an integer will load the desired extra lab
#'   automatically. Leaving the argument blank will load a menu for users to
#'   make a selection from.
#' @examples
#' extra_lab() # Loads a menu to choose from
#' extra_lab(999) # Automatically loads the 999th lab from the menu.
#' extra_lab('veryinterestinglab') # Automatically loads the veryinterestinglab from the menu.
#' extra_labs() # Similar to extra_lab()
#' extra_labs(999) # Similar to extra_lab(999)
#' extra_labs('veryinterestinglab') # Similar to extra_lab('veryinterestinglab')
#'
#' @importFrom curl curl
#' @importFrom rstudioapi viewer
#' @importFrom stringr str_extract
#' @export

extra_lab <- function(lab) {

  if (missing(lab)) {
    lab <- menu(1:10, title = "Enter the number next to the lab you would like to load:")
  }

  .log_loaded_pd(lab)

	url <- paste0('https://raw.githubusercontent.com/mobilizingcs/ids_labs/master/',
                     "/extra_labs/" , toString(lab), '.html')

  con <- curl(url, "r")
  page <- paste(readLines(con), collapse = '\n')
  close(con)

  tf <- tempfile(fileext = ".html")
  writeLines(page, tf)

  viewer(tf)


}

.log_loaded_pd <- function(lab) {
  # logs the load_lab command correctly regardless of how the user selected a lab
  log_info(paste('load_pd(',lab,')', sep = ""))
}


#' Update Mobilizr
#' @export
update_mobilizr <- function() {

}

