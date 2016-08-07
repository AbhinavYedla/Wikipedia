###################################################################
#                     Wikipedia Top 1000 Pages                    #
###################################################################
#' @title 
#' Wikipedia top 100 pages
#' 
#' @description
#' Get Top 1000 wikipedia pages for Day / Month(all Days of a month)
#'
#' @param date
#' Date in format: YYYYMMDD
#'
#' @param date.month.year
#' Date in format: YYYYMM
#'
#' @return
#' An list with Json data
#'
#' @author
#' Abhinav Yedla \email{abhinavyedla@gmail.com}
#'
#' @examples
#' WikiTop1000PerDay(20151231)
#' WikiTop1000PerMonth(200712)
#'
#' @seealso
#' \code{\link{}}
#'
#' @keywords
#' Top 1000 wikipedia pages
#' 
#' @import 
#' jsonlite
#' 
#' @export


WikiTop1000PerDay <- function(date) {


  #Checking parameters
  if (nchar(date) == 8) {

    year <- substr(date, 1, 4)
    month <-  substr(date, 5, 6)
    day <-  substr(date, 7, 8)

    source <-
      "http://wikimedia.org/api/rest_v1/metrics/pageviews/top/en.wikipedia/all-access/"

    url <-
      paste(source,
            year,
            "/",
            month , "/", day,
            sep = "")

    #Fetch data from url in JSON format
    data <- fromJSON(url)

    return(data)
  }
}

WikiTop1000PerMonth <- function(date.month.year) {
  
  if (nchar(date.month.year) == 6) {
    year <- substr(date.month.year, 1, 4)
    month <-  substr(date.month.year, 5, 6)

    source <-
      "http://wikimedia.org/api/rest_v1/metrics/pageviews/top/en.wikipedia/all-access/"
    url <-
      paste(source, year, "/", month, "/all-days", sep = "")

    #Fetch data from url in JSON format
    data <- fromJSON(url)
    return(data)
  }
}
