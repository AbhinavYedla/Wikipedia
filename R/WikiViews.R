#############################################################################################
#                                     Wikipedia Page Views                                  #
#############################################################################################
#' @title
#' Wikipedia Page Views per day
#'
#' @description
#' Get views per page for a given day
#'
#'
#' @param page.title
#' Title of the page to get views
#'
#' @param date
#' get views of page on given date
#' 
#' @param language
#' Language of wikipedia page
#' 
#' @return
#' An list with date and views on that date
#'
#' @author
#' Abhinav Yedla \email{abhinavyedla@gmail.com}
#'
#' @examples
#' wiki_views_day("India",20151231)
#' 
#' @seealso
#' \code{\link{}}
#'
#' @keywords
#' Wikipedia Page view Stats
#'
#' @import jsonlite
#'
#' @export

wiki_views_day <- function(page.title, date, language = "en") {
  #check for length of date and overall date ex: 20161332 wrong date
  
  #Checking parameters
  if (missing(page.title)) {
    stop("Please provide the wikipedia Page Title")
  }
  
  url.source <- "http://stats.grok.se/json/"
  
  
  if (!language %in% wikiLangCodes$ShortCode) {
    stop(
      "Please check the language code. Refer to wikiLangCodes data frame for language short code"
    )
  }
  
  tryCatch({
    formatted.date <-
      as.character(as.Date(as.character(date), "%Y%m%d"))
  }, error = function(ex) {
    stop("Please check the date entered. Example of correct date format: 20151231")
  },
  finally = {
    closeAllConnections()
  })
  
  url <-
    paste(url.source,
          language,
          "/",
          substr(date, 1, 6),
          "/",
          page.title ,
          sep = "")
  
  #Fecth data from url in JSON Format
  data <- fromJSON(url)
  
  return(data[['daily_views']][formatted.date])
}

#############################################################################################
#                                     Wikipedia Page Views                                  #
#############################################################################################
#' @title
#' Wikipedia Page Views per month
#'
#' @description
#' Get views for wikipedia based on given month
#'
#'
#' @param page.title
#' Title of the page to get views
#'
#' @param date
#' get views of page on given date
#' 
#' @param language
#' Language of wikipedia page
#' 
#' @return
#' An list with date and views on that date
#'
#' @author
#' Abhinav Yedla \email{abhinavyedla@gmail.com}
#'
#' @examples
#' wiki_views_month("United_States",200712,"en")
#' 
#' @seealso
#' \code{\link{}}
#'
#' @keywords
#' Wikipedia Page view Stats
#'
#' @import jsonlite
#'
#' @export

wiki_views_month <- function(page.title, date, language = "en") {
  if (!language %in% wikiLangCodes$ShortCode) {
    stop(
      "Please check the language code. Refer to wikiLangCodes data frame for language short code"
    )
  }
  
  date <- as.character(date)
  
  if (nchar(date) != 6) {
    stop("Wrong date. Ex: 201512")
  }
  
  url.source <- "http://stats.grok.se/json/"
  
  url <-
    paste(url.source, language, "/", date, "/", page.title, sep = "")
  
  data <- fromJSON(url)
  
  return(data[['daily_views']])
  
  
}

#############################################################################################
#                                     Wikipedia Page Views                                  #
#############################################################################################
#' @title
#' Wikipedia Page Views per year
#'
#' @description
#' Get views for wikipedia page based on given year.
#'
#' @param page.title
#' Title of the page to get views
#'
#' @param year
#' get views of page for given year
#'
#' @param language
#' Language of wikipedia page
#'
#' @return
#' An list with date and views on that date
#'
#' @author
#' Abhinav Yedla \email{abhinavyedla@gmail.com}
#'
#' @examples
#' wiki_views_year("China",2012,"zh")
#' 
#' @seealso
#' \code{\link{}}
#'
#' @keywords
#' Wikipedia Page view Stats
#'
#' @import jsonlite
#'
#' @export

wiki_views_year <- function(page.title, year, language = "en") {
 
  year <- as.character(year)
  
  if (nchar(year) != 4) {
    stop("Wrong date. Ex: 2015")
  }
  
  
  if (!language %in% wikiLangCodes$ShortCode) {
    stop(
      "Please check the language code. Refer to wikiLangCodes data frame for language short code"
    )
  }
  
  #Initialise variable
  data <- list()
  
  url.source <- "http://stats.grok.se/json/"
  
  present.year <- format(Sys.Date(), "%Y")
  
  start.month <- 1
  end.month <- 12
  
  if (year == 2007) {
    start.month <- 12
  } else if (present.year == year) {
    end.month <- format(Sys.Date(), "%m")
  } else if (year > present.year) {
    stop("Wrong date")
  }
  
  #Iterate from Jan to Dec
  for (month in start.month:end.month) {
    interm.data <-
      wiki_views_month(page.title, paste(year, formatC(
        month, width = 2, flag = "0"
      ), sep =
        ""), language)
    
    data <- append(data, interm.data)
    
  }
  return(data)
}

#############################################################################################
#                                     Wikipedia Page Views                                  #
#############################################################################################
#' @title
#' Wikipedia Page Views
#'
#' @description
#' Get views per page for wikipedia page from its creation date.
#'
#' @param page.title
#' Title of the page to get views
#'
#' @param language
#' Language of wikipedia page
#'
#' @return
#' An list with date and views on that date
#'
#' @author
#' Abhinav Yedla \email{abhinavyedla@gmail.com}
#'
#' @examples
#' wiki_views("England","hi")
#' 
#' @seealso
#' \code{\link{}}
#'
#' @keywords
#' Wikipedia Page view Stats
#'
#' @import jsonlite
#' 
#' @note 
#' For faster data collection use wiki_views_new. With one restriction that 
#' data is only avalaible from 2015-07-01 with the new api API. 
#'
#' @export

wiki_views <- function(page.title, language = "en") {
  if (!language %in% wikiLangCodes$ShortCode) {
    stop(
      "Please check the language code. Refer to wikiLangCodes data frame for language short code"
    )
  }
  
  data <- list()
  url.source <- "http://stats.grok.se/json/"
  
  startYear <- 2007
  endYear <- as.numeric(format(Sys.Date(), "%Y"))
  
  for (year in startYear:endYear) {
    interm.data <- wiki_views_year(page.title, year, language)
    
    data <- append(data, interm.data)
  }
  return(data)
  
}

#############################################################################################
#                                     Wikipedia Page Views                                  #
#############################################################################################
#' @title
#' Wikipedia Page Views new ApI
#'
#' @description
#' Get views per page from 20150107
#'
#' @param page.title
#' Title of the page to get views
#' 
#' @param language
#' Language of wikipedia page
#'
#' @return
#' An list with date and views on that date
#'
#' @author
#' Abhinav Yedla \email{abhinavyedla@gmail.com}
#'
#' @examples
#' wiki_views_new("Hello_World","en")
#' 
#' @seealso
#' \code{\link{}}
#'
#' @keywords
#' Wikipedia Page view Stats
#'
#' @import jsonlite
#'
#' @note 
#' This function is used to get data from 2015-07-01 to present in single request
#' 
#' @export

wiki_views_new <- function(page.title, language = "en") {
  if (!language %in% wikiLangCodes$ShortCode) {
    stop(
      "Please check the language code. Refer to wikiLangCodes data frame for language short code"
    )
  }
  
  data <- list()
  url.source <-
    "https://wikimedia.org/api/rest_v1/metrics/pageviews/per-article/"
  project <-
    paste(language, ".wikipedia/all-access/all-agents/", sep = "")
  
  start <- "20150701"
  end <- format(Sys.Date() - 1, format = "%Y%m%d")
  url <-
    paste(url.source,
          tolower(project),
          page.title,
          "/daily/",
          start,
          "/",
          end,
          sep = "")
  interm.data <- fromJSON(url)
  data <- interm.data[[1]][c(1, 4, 7)]
  return(data)
}
