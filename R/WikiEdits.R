######################################################################################################
#                                             Wikipedia Page Edits                                   #
######################################################################################################
#' @title 
#' Wikipedia Page Edits
#' 
#' @description
#' Get Number of edits of a wikipedia page. List of Ip Edits, Minor Edits and total Edits are returned
#'
#' @param page.title
#' Title of the page to get views
#'
#' @param language
#' Language of wikipedia page
#'
#' @return
#' An list with number of edits of a page per month from creation of page
#'
#' @details
#' \code{language} The codes represent the language codes defined by ISO 639-1 and ISO 639-3, and
#' the decision of which language code to use is usually determined by the IETF language tag policy.
#' For more details please check List_of_Wikipedias page[1]
#'
#' @examples
#' wikiEdits("United_States","en")
#'
#' @author
#' Abhinav Yedla \email{abhinavyedla@gmail.com}
#'
#' @references
#' [1] https://en.wikipedia.org/wiki/List_of_Wikipedias
#'
#' @keywords
#' Wikipedia Page Edits
#'
#' @seealso
#' \code{\link{}}
#' 
#' @note languageCodes is a dataframe with all language short codes
#'
#' @import rvest
#'
#' @export


wikiEdits <- function(page.title, language = "en") {

  #Checking parameters
  if (missing(page.title)){
    stop("Page Title is not provided")
  }
  data(languageCodes)
  
  if(!language %in% languageCodes$ShortCode){
    stop("Please check the language code. Use  languageCodes data frame language short code")
  }
    

  #Initialize varaiables
    df <- data.frame()

    url <-
      paste0("https://tools.wmflabs.org/xtools-articleinfo/?article=", page.title,
             "&project=", language, ".wikipedia.org")

    #Prepare the xpath expressions
    xpath <-
      "//div[@id = 'monthcounts']/table//td[@class = 'date' or @class = 'tdnum']//text()"


    # Read the HTML Document and parse data according to the provided xpath expression
    doc <- read_html(url)
    dataXMLNodeSet <- html_nodes(doc, xpath = xpath)
    data <- html_text(dataXMLNodeSet)


    #Sequence the data and split it with each part consisting 6 rows
    seq.data <- seq_along(data)
    interm.data <- split(data, ceiling(seq.data / 6))

    #Consolidate the data
    df <- do.call(rbind, interm.data)
    df$language <-  language

    
    colnames(df) <-
      c(
        "Date",
        "Number of edits",
        "IPs",
        "IPs Percentage",
        "Minor edits",
        "Minor edits Percentage",
        "language"
      )

    return(df)
  }
