#-------------------------------------- HEADER --------------------------------------------#
#' @title Standard Deviation
#' @description Computes the standard deviation for a study variable.
#' @details Uses denominator n-1.
#'
#' The standard deviation of a zero-length vector (after removal of NAs if na.rm = TRUE) is not defined and gives an error.
#' The standard deviation of a length-one vector is NA.
#' @param x a character, the name of a study variable.
#' @param datasources a list of opal object(s) obtained after login in to opal servers;
#' these objects hold also the data assign to R, as \code{data frame}, from opal datasources.
#' @return return a numeric value.
#' @author Paula Raissa Costa e Silva
#' @section Dependencies:
#' \code{\link[distStatsC]{ds.var}}
#' @export
#' @examples {
#' sd <- ds.sd('D$birth_weight')
#' }
ds.sd <- function(x=NULL, datasources=NULL) {

  if(is.null(x)){
    stop("Please provide the name of the input vector", call. = FALSE)
  }

  # if no opal login details are provided look for 'opal' objects in the environment
  if(is.null(datasources)) {
    datasources <- findLoginObjects()
  }

  var <- ds.var(x, datasources)
  final.sd <- sqrt(var)

  return(final.sd)
}
