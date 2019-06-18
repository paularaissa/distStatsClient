#-------------------------------------- HEADER --------------------------------------------#
#' @title Arithmetic mean.
#' @description Computes the arithmetic mean.
#' @details The \eqn{\sum{x}} and the sample size is computed is computed for each data node.
#' Combines these values by the equation \eqn{{\sum{dn}}/{l}}, where \eqn{\sum{dn}} is the sum of the sums of nodes,
#' and \emph{l} is the global sample size.
#' Requires the functions \code{\link{getSum}} and \code{\link{getLength}} from server side.
#' @param x a character, the name of study variable.
#' @param datasources a list of opal object(s) obtained after login in to opal servers;
#' these objects hold also the data assign to R, as \code{data frame}, from opal datasources.
#' @return return a numeric value.
#' @author Paula Raissa Costa e Silva
#' @section Dependencies:
#' \code{\link{getSum}}, \code{\link{getLength}}
#' @export
#' @examples {
#' ds.arMean('D$birth_weight')
#' }

ds.arMean <- function(x=NULL, datasources=NULL) {

  if(is.null(x)){
    stop("Please provide the name of the input vector", call. = FALSE)
  }

  result.length <- c()
  result.sum <- c()

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
  call.sum <- call("getSum", x)
  result.sum <- opal::datashield.aggregate(datasources, call.sum)
  call.length <- call("getLength", x)
  result.length <- opal::datashield.aggregate(datasources, call.length)

  total.sum <- 0
  total.length <- 0
  for (i in 1:length(result.sum)) {
    total.sum <- total.sum + result.sum[[i]]
    total.length <- total.length + result.length[[i]]
  }
  mean <- total.sum/total.length

  return(mean)

}
