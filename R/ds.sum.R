#-------------------------------------- HEADER --------------------------------------------#
#' @title Sum of Study Variable Elements
#' @description Computes the sum of values from a given study variable.
#' @details It is a wrapper for the server side function \code{\link{getSum}}.
#' @param x a character, the name of a study variable.
#' @param datasources a list of a list of data connection parameters.
#' If it is a DataSHIELD connection, datasources is a list of opal object(s) obtained after login in to opal servers;
#' these objects hold also the data assign to R, as \code{data frame}, from opal datasources.
#' If the data is csv files, datasources is a list of paths and filenames.
#' If the data is a sql connection, datasources is a list of conection, database name, variables and query.
#' @return return a numeric value.
#' @author Paula Raissa Costa e Silva
#' @section Dependencies:
#' \code{\link{getSum}}
#' @export
#' @examples
#' ds.sum('D$birth_weight')
#'

ds.sum <- function(x=NULL, datasources=NULL) {

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
    cally <- call("getSum", x)
    result <- opal::datashield.aggregate(datasources, cally)
  }

  total.sum <- 0
  for (value in result) {
    total.sum <- total.sum + value
  }

  return(total.sum)

}
