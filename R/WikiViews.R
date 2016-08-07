#############################################################################################
#                                     Wikipedia Page Views                                  #
#############################################################################################
#' @title 
#' Wikipedia Page Views
#' 
#' @description
#' Get views per page. 5 function with day, month, year and from 2007 date intervals
#'
#' @param language
#' Language of wikipedia page
#'
#' @param page.title
#' Title of the page to get views
#'
#' @param date
#' get views of page on given date
#'
#' @param year
#' get views of page for given year
#'
#' @return
#' An list with date and views on that date
#'
#' @author
#' Abhinav Yedla \email{abhinavyedla@gmail.com}
#'
#' @examples
#' GetPageViewsDay("India",20151231)
#' GetPageViewsMonth(200712,"United_States","en")
#' GetPageViewsYear(2012,"China","zh")
#' GetPageViews("England","hi")
#' GetPageViewsNew("Hello_World","en")
#' GetPageViewsNew: gives the data from Jul 2015 - Present in a single request
#' @seealso
#' \code{\link{}}
#'
#' @keywords
#' Wikipedia Page view Stats
#' 
#' @import jsonlite
#'
#' @export

#Get Views per Day
GetPageViewsDay <- function(page.title, date, language = "en") {
  #check for length of date and overall date ex: 20161332 wrong date

  url.source <- "http://stats.grok.se/json/"

  if (length(date) < 8){
    stop("Wrong date")
  }
  
  data(languageCodes)
  
  if(!language %in% languageCodes$ShortCode){
    stop("Please check the language code. Use  languageCodes data frame language short code")
  }
  
  tryCatch({
    formatted.date <-
      as.character(as.Date(as.character(date), "%Y%m%d"))
  }, finally = {
    stop("Please check the date entered. Ex: 20161231")
    
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

#Get wikipedia page views per month
GetPageViewsMonth <- function(page.title, date, language = "en") {
  
  
  if (length(date) != 6){
    stop("Wrong date. Ex: 201512")
  }
  
  data(languageCodes)
  
  if(!language %in% languageCodes$ShortCode){
    stop("Please check the language code. Use  languageCodes data frame language short code")
  }
  

  url.source <- "http://stats.grok.se/json/"

  url <-
    paste(url.source, language, "/", date, "/", page.title, sep = "")

  data <- fromJSON(url)

  return( data[['daily_views']])


}

#Get page views per year
GetPageViewsYear <- function(page.title, year, language = "en") {
  
  
  if (length(year) != 4){
    stop("Wrong date. Ex: 2015")
  }
  
  data(languageCodes)
  
  if(!language %in% languageCodes$ShortCode){
    stop("Please check the language code. Use  languageCodes data frame language short code")
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
  } else if (year > present.year){
    stop("Wrong date")
  }

  #Iterate from Jan to Dec
  for (month in start.month : end.month) {

    interm.data <-
      GetPageViewsMonth(page.title, paste(year, formatC(
        month, width = 2, flag = "0"
      ), sep =
        ""), language)

    data <- append(data, interm.data)

  }
  return(data)
}


#Get page views from 12/2007 to present
GetPageViews <- function(page.title, language = "en") {
  
  data(languageCodes)
  
  if(!language %in% languageCodes$ShortCode){
    stop("Please check the language code. Use  languageCodes data frame language short code")
  }

  data <- list()
  url.source <- "http://stats.grok.se/json/"

  startYear <- 2007
  endYear <- as.numeric(format(Sys.Date(), "%Y"))

  for (year in startYear:endYear) {
    interm.data <- GetPageViewsYear(page.title, year, language)

    data <- append(data, interm.data)
  }
  return(data)

}

#This function is used to get data from 07/01/2015 to present in single request
GetPageViewsNew <- function(page.title, language = "en") {
  
  data(languageCodes)
  
  if(!language %in% languageCodes$ShortCode){
    stop("Please check the language code. Use  languageCodes data frame language short code")
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
