#-------------------------------------- HEADER --------------------------------------------#
#' @title Length of an Object
#' @description Computes the sample length of a study variable.
#' @details If a vector is shortened, extra values are discarded and when a vector is lengthened,
#' it is padded out to its new length with NAs (nul for raw vectors).
#'
#' It is a wrapper for the server side function \code{\link{getLength}}.
#' @param x a character, the name of a study variable.
#' @param datasources a list of opal object(s) obtained after login in to opal servers;
#' these objects hold also the data assign to R, as \code{data frame}, from opal datasources.
#' @return return a numeric value.
#' @author Paula Raissa Costa e Silva
#' @section Dependencies:
#' \code{\link{getLength}}
#' @export
#' @examples {
#' size <- ds.length('D$birth_weight')
#' }
#'

ds.length <- function(x=NULL, datasources=NULL) {

  result <- c()

  if (is.null(x)) {
    stop("Please provide the name of the input vector", call. = FALSE)
  }

  # if no opal login details are provided look for 'opal' objects in the environment
  if (is.null(datasources)) {
    datasources <- findLoginObjects()
  }
  #number of studies
  num.sources <- length(datasources)
  xnames <- extract(x)
  varname <- xnames$elements
  obj2lookfor <- xnames$holders
  if (is.na(obj2lookfor)) {
    defined <- isDefined(datasources, varname)
  } else {
    defined <- isDefined(datasources, obj2lookfor)
  }
  typ <- checkClass(datasources, x)

  #the input vectormust be numeric
  if (typ!='integer' & typ!='numeric') {
    message(paste0(x, "is of type ", typ, "!"))
    stop("The input vector must be integer or numeric vector", call. = FALSE)
  }
  for (i in 1:num.sources) {
    cally <- call("getLength", x)
    result <- opal::datashield.aggregate(datasources, cally)
  }

  total.lenght <- 0
  for (value in result) {
    total.lenght <- total.lenght + value
  }

  return(total.lenght)

}
