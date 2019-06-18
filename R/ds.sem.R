#-------------------------------------- HEADER --------------------------------------------#
#' @title Standard Error of the Mean
#' @description Computes the variance for a study variable.
#' @details By default, computes the sample variance.
#' @param x a character, the name of a study variable.
#' @param datasources a list of opal object(s) obtained after login in to opal servers;
#' these objects hold also the data assign to R, as \code{data frame}, from opal datasources.
#' @return return a numeric value.
#' @author Paula Raissa Costa e Silva
#' @section Dependencies:
#' \code{\link{getLength}}, \code{\link{getVariance}}
#' @export
#' @examples {
#' var <- ds.var('D$birth_weight')
#' }
#'

ds.sem <- function(x=NULL, datasources=NULL) {
  #-------------------------------------- BASIC CHECKS ----------------------------------------------#
  # if no opal login details are provided look for 'opal' objects in the environment
  if(is.null(datasources)){
    datasources <- findLoginObjects()
  }

  if(is.null(x)){
    stop("Please provide the name of the input vector", call. = FALSE)
  }

  xnames <- extract(x)
  varname <- xnames$elements
  obj2lookfor <- xnames$holders

  if(is.na(obj2lookfor)){
    defined <- isDefined(datasources, varname)
  } else {
    defined <- isDefined(datasources, obj2lookfor)
  }

  sd <- ds.sd(x, datasources)
  n <- ds.length(x, datasources)

  sem <- sd / sqrt(n)

  return(sem)

}
