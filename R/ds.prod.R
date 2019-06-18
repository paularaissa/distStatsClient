#-------------------------------------- HEADER --------------------------------------------#
#' @title Product of study variable elements
#' @description It computes the product of all values in its arguments.
#' @details It is a wrapper for the server side function \code{\link{getProd}}.
#' @param x a character, the name of study variable.
#' @param naRm a boolean, if TRUE (default) remove the missing values during the computation on the server side;
#' if FALSE does not remove the missing values.
#' @param datasources a list of opal object(s) obtained after login in to opal servers;
#' these objects hold also the data assign to R, as \code{data frame}, from opal datasources.
#' @return return a numeric value.
#' @author Paula Raissa Costa e Silva
#' @section Dependencies:
#' \code{\link{getProd}}
#' @export
#' @examples
#' prod <- ds.prod('D$maternal_age')
#'

ds.prod <- function(x=NULL, naRm=FALSE, datasources=NULL) {
  if(is.null(x)){
    stop("Please provide the name of the input vector", call. = FALSE)
  }

  if(is.null(datasources)) {
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
  result <- c()
  for(i in 1:num.sources){
    call <- call("getProd", x, naRm)
    result <- opal::datashield.aggregate(datasources, call)
  }

  total.prod <- 0
  for (value in result) {
    total.prod <- total.prod + value
  }

  return(total.prod)

}
