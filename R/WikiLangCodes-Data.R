##############################################################################
#                               Wikipedia Languages                          #
##############################################################################
#' Wikipedia languages and codes.
#'
#' @docType data
#'
#' @usage data(wikiLangCodes)
#'
#' @format An data frame with 3 columns: Language, Language Local and Short Code.
#'
#' @keywords datasets
#'
#' @references 
#' https://en.wikipedia.org/wiki/List_of_Wikipedias
#'
#' @source \href{#' https://en.wikipedia.org/wiki/List_of_Wikipedias}
#'
#' @examples
#' data(wikiLangCodes)
#' code <- wikiLangCodes[wikiLangCodes$Language == 'Japanese',3]
"wikiLangCodes"