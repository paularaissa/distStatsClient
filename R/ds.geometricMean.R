#-------------------------------------- HEADER --------------------------------------------#
#' @title Geometric Mean
#' @description Computes the geometric mean for a given study variable.
#' @details Formally the geometric mean is the \emph{i[n]} root of the \emph{n} numbers products, or \emph{exp} to the mean log of \emph{x}.
#' Each data node computes the sum of logs, and the central node computes the arithmetic mean and its exponentiation.
#' @param x a character, the name of study variable.
#' @param naRm a boolean, if TRUE (default) remove the missing values during the computation on the server side;
#' if FALSE does not remove the missing values.
#' @param zero.propagate a boolean, if TRUE (default) remove all zeros during the computation on the server side;
#' if FALSE does not remove the zero values.
#' @param datasources a list of opal object(s) obtained after login in to opal servers;
#' these objects hold also the data assign to R, as \code{data frame}, from opal datasources.
#' @return return a numeric value.
#' @author Paula Raissa Costa e Silva
#' @section Dependencies:
#' \code{\link{getGm}}, \code{\link{getLength}}
#' @export
#' @examples {
#' gm <- ds.geometricMean('D$birth_weight')
#' }

ds.geometricMean <- function(x=NULL, naRm=TRUE, zero.propagate=FALSE, datasources=NULL) {

  if(is.null(x)){
    stop("Please provide the name of the input vector", call. = FALSE)
  }

  sumLog.result <- list()
  length.result <- list()

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
  for (i in 1:num.sources) {
    sumLog.cally <- call("getGm", x, naRm, zero.propagate)
    sumLog.result <- opal::datashield.aggregate(datasources, sumLog.cally)
    call.lenght <- call("getLength", x)
    length.result <- opal::datashield.aggregate(datasources, call.lenght)
  }


  sum.log <- 0
  total.length <- 0
  for(sum in sumLog.result) {
    if(is.nan(sum)){
      sum <- 0
    }else{
      sum.log <- sum.log + sum
    }
  }
  for(n in length.result){
    total.length <- total.length + n
  }
  gm <- exp(sum.log/total.length)

  return(gm)

}
