#-------------------------------------- HEADER --------------------------------------------#
#' @title Sum of Squares
#' @description Computes the sum of squares for a given study variable.
#' @details It is a wrapper for the server side function \code{\link{getSquaredSum}}.
#' @param x a character, the name of a study variable.
#' @param datasources a list of opal object(s) obtained after login in to opal servers;
#' these objects hold also the data assign to R, as \code{data frame}, from opal datasources.
#' @return return a numeric value.
#' @author Paula Raissa Costa e Silva
#' @section Dependencies:
#' \code{\link{getSquaredSum}}
#' @export
#' @examples
#' ds.sumOfSquares(x='D$birth_weight')
#'
ds.sumOfSquares <- function(x=NULL, datasources=NULL) {

  if(is.null(x)){
    stop("Please provide the name of the input vector", call. = FALSE)
  }

  # if no opal login details are provided look for 'opal' objects in the environment
  if(is.null(datasources)){
    datasources <- findLoginObjects()
  }

  xnames <- extract(x)
  varname <- xnames$elements
  obj2lookfor <- xnames$holders
  if(is.na(obj2lookfor)){
    defined <- isDefined(datasources, varname)
  } else {
    defined <- isDefined(datasources, obj2lookfor)
  }
  #number of studies
  num.sources <- length(datasources)

  result <- list()
  for (i in 1:num.sources) {
    cally <- call("getSquaredSum", x)
    result <- opal::datashield.aggregate(datasources, cally)
  }

  total.sum <- 0
  for(value in result) {
    total.sum <- total.sum + value
  }

  return(total.sum)

}
