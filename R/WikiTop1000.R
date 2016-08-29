###################################################################
#                     Wikipedia Top 1000 Pages                    #
###################################################################
#' @title 
#' Wikipedia top 1000 pages per day
#' 
#' @description
#' Get Top 1000 wikipedia pages for each Day
#'
#' @param date
#' Date in format: YYYYMMDD
#'
#' @return
#' An list with Json data
#'
#' @author
#' Abhinav Yedla \email{abhinavyedla@gmail.com}
#'
#' @examples
#' wiki_top1000_day(20151231)
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


wiki_top1000_day <- function(date) {


  #Checking parameters
  if (nchar(date) != 8) {
   stop("Please check the date entered. Format of the date: YYYYMMDD") 
  }

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

###################################################################
#                     Wikipedia Top 1000 Pages                    #
###################################################################
#' @title 
#' Wikipedia top 1000 pages per month
#' 
#' @description
#' Get Top 1000 wikipedia pages for Month(all Days of a month)
#'
#' @param month.year
#' Date in format: YYYYMM
#'
#' @return
#' An list with Json data
#'
#' @author
#' Abhinav Yedla \email{abhinavyedla@gmail.com}
#'
#' @examples
#' wiki_top1000_month(201512)
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


wiki_top1000_month <- function(month.year) {
  
  if (nchar(month.year) != 6) {
    stop("Please check the date entered. Format of the date: YYYYMM") 
  }
    year <- substr(month.year, 1, 4)
    month <-  substr(month.year, 5, 6)
    
    
    if(year < "2015"){
      stop("Please make sure date atleast 201507")
    }else if(year == "2015"){
      if(month < "07"){
        stop("Please make sure date atleast 201507")
      }
    }
    
    
    source <-
      "http://wikimedia.org/api/rest_v1/metrics/pageviews/top/en.wikipedia/all-access/"
    url <-
      paste(source, year, "/", month, "/all-days", sep = "")

    #Fetch data from url in JSON format
    data <- fromJSON(url)
    return(data)
  
}
