#-------------------------------------- HEADER --------------------------------------------#
#' @title Skewness
#' @description Computes the skewness.
#' @details Considering x.i for elements of x and mu for their mean, so m.r = sum{i}(xi-mu)^r/n is the sample moments of order r.
#' Which sum{i}(xi-mu)^r is computed at each data node and combined by this function in the central node, also called client-side.
#'
#' It is a wrapper for the server side function \code{\link{getSkewness}}.
#' @param x a character, the name of the study variable.
#' @param datasources a list of opal object(s) obtained after login in to opal servers;
#' these objects hold also the data assign to R, as \code{data frame}, from opal datasources.
#' @return return a numeric value.
#' @author Paula Raissa Costa e Silva
#' @section Dependencies:
#' \code{\link{getSkewness}}
#' @export
#' @examples
#' skew <- ds.skewness('D$birth_weight')
#'

ds.skewness <- function(x=NULL, datasources=NULL) {

  if(is.null(x)){
    stop("Please provide the name of the input vector!", call. = FALSE)
  }
  if(is.null(datasources)){
    datasources <- findLoginObjects()
  }

  mean <- ds.arMean(x, datasources)
  size <- ds.length(x, datasources)
  result <- list()

  xnames <- extract(x)
  varname <- xnames$elements
  obj2lookfor <- xnames$holders

  if(is.na(obj2lookfor)) {
    defined <- isDefined(datasources, varname)
  } else{
    defined <- isDefined(datasources, obj2lookfor)
  }

  #number of studies
  num.sources <- length(datasources)

  idx <- 1
  for (i in 1:num.sources) {
    mean.temp <- paste0(as.character(mean[[idx]]), collapse="x")
    cally <- call("getSkewness", x, mean.temp)
    result <- opal::datashield.aggregate(datasources, cally)
    if(length(mean) > 1) idx <- idx + 1
  }

  m3 <- 0
  m2 <- 0
  for (sk in result) {
    m3 <- m3 + sk$m3
    m2 <- m2 + sk$m2
  }
  m3 <- m3 / size
  m2 <- sqrt(m2/(size-1))^3

  skew.final <- m3/m2

  return(skew.final)

}
