#-------------------------------------- HEADER --------------------------------------------#
#' @title Kurtosis
#' @description Computes the kurtosis of a given vector.
#' @details Considering \eqn{x[i]} for elements of \eqn{x} and \eqn{x*} for their mean, so \eqn{m[r] = \sum[i](x[i]-x*)^r/n}
#' is the sample moments of order r.
#' Which \eqn{\sum[i](x[i]-x*)^r} is computed at each data node and combined by this function in the central node, also called client-side.
#'
#' It is a wrapper for the server side function \code{\link{getKurtosis}}.
#' @param x a character, the name of a numerical vector
#' @param datasources a list of opal object(s) obtained after login in to opal servers;
#' these objects hold also the data assign to R, as \code{data frame}, from opal datasources.
#' @return return a numeric value.
#' @author Paula Raissa Costa e Silva
#' @section Dependencies:
#' \code{\link{getKurtosis}}, \code{\link{ds.arMean}}, \code{\link{ds.length}}
#' @export
#' @examples {
#' kurt <- ds.kurtosis('D$birth_weight')
#' }

ds.kurtosis <- function(x=NULL, datasources=NULL) {

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
    cally <- call("getKurtosis", x, mean.temp)
    result <- opal::datashield.aggregate(datasources, cally)
    if (length(mean) > 1) idx <- idx + 1
  }

  m4 <- 0
  m2 <- 0
  for (kt in result) {
    m4 <- m4 + kt$m4
    m2 <- m2 + kt$m2
  }
  m4 <- m4 / size
  m2 <- (m2/(size-1))^2
  kurtosis.final <- (m4/m2) - 3

  return(kurtosis.final)

}
