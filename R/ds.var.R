#-------------------------------------- HEADER --------------------------------------------#
#' @title Sample Variance
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
ds.var <- function(x=NULL, datasources=NULL){

  if(is.null(x)){
    stop("Please provide the name of the input vector", call. = FALSE)
  }

  # if no opal login details are provided look for 'opal' objects in the environment
  if(is.null(datasources)){
    datasources <- findLoginObjects()
  }

  result.var <- c()
  result.length <- c()
  mean <- ds.arMean(x=x, datasources=datasources)

  #number of studies
  num.sources <- length(datasources)
  xnames <- extract(x)
  varname <- xnames$elements
  obj2lookfor <- xnames$holders
  if(is.na(obj2lookfor)){
    defined <- isDefined(datasources, varname)
  } else {
    defined <- isDefined(datasources, obj2lookfor)
  }
  idx <- 1
  for(i in 1:length(num.sources)) {
    mean.temp <- paste0(as.character(mean[[idx]]), collapse="x")
    call.var <- call("getVariance", x, mean.temp)
    result.var <- opal::datashield.aggregate(datasources, call.var)
    call.length <- call("getLength", x)
    result.length <- opal::datashield.aggregate(datasources, call.length)
    if(length(mean) > 1) idx <- idx + 1
  }

  total.length <- 0
  total.var <- 0
  for(i in 1:length(result.length)) {
    total.length <- total.length + result.length[[i]]
    total.var <- total.var + result.var[[i]]
  }
  final.var <- total.var/(total.length-1)

  return(final.var)

}
