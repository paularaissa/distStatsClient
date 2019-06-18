#-------------------------------------- HEADER --------------------------------------------#
#' @title Maximum
#' @description Computes the maximum value among data nodes or for each data node.
#' @details Return the maximum of all the values present in their arguments, as integer if all are logical or integer,
#' as double if all are numeric, and character otherwise.
#'
#' The maximum of a numeric empty set is +Inf which ensures transitivity.
#' For numeric x ds.max(x) == -Inf whenever ds.length(x) == 0 (after removing missing values if requested).
#'
#' By definition the max of a numeric vector containing an NaN is NaN, except that the max of any vector containing
#' an NA is NA even if it also contains an NaN. Note that ds.max(NA, Inf) == NA even though the maximum would be Inf whatever
#' the missing value actually is.
#'
#' Character versions are sorted lexicographically. The max of an empty character vector is defined to be character NA.
#'
#' It is a wrapper for the server side function \code{\link{getMax}}.
#' @param x a character, the name of study variable.
#' @param datasources a list of opal object(s) obtained after login in to opal servers;
#' these objects hold also the data assign to R, as \code{data frame}, from opal datasources.
#' @return If type='split', returns a numeric list.
#' If type='combine', return a numeric value.
#' @author Paula Raissa Costa e Silva
#' @section Dependencies:
#' \code{\link{getMax}}
#' @export
#' @examples {
#' max <- ds.max('D$birth_weight')
#' }

ds.max <- function(x=NULL, datasources=NULL) {

  if(is.null(x)){
    stop("Please provide the name of the input vector", call. = FALSE)
  }

  if(is.null(datasources)){
    datasources <- findLoginObjects()
  }

  xnames <- extract(x)
  varname <- xnames$elements
  obj2lookfor <- xnames$holders

  if(is.na(obj2lookfor)) {
    defined <- isDefined(datasources, varname)
  } else{
    defined <- isDefined(datasources, obj2lookfor)
  }

  result <- list()
  num.sources <- length(datasources)
  for(i in 1:num.sources){
    cally <- call("getMax", x)
    result <- opal::datashield.aggregate(datasources, cally)
  }

  aux <- c()
  for(val in result){
    aux <- rbind(aux, val)
  }
  max <- max(aux)

  return(max)

}
