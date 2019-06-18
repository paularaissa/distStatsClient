#-------------------------------------- HEADER --------------------------------------------#
#' @title Combined Histogram
#' @description Computes the combined frequecy table for each value given in a list of intervals.
#' @details Return the frequency value for each given interval.
#' For a intervals list [V1, V2, V3, ...], it computes the frequency list [F1, F2, F3, ...], where F1 is the number of values <= V1;
#' F2 is the number of values where V1 > x <= V2, etc.
#' @param x a character, the name of study variable.
#' @param listValues a list of values to compute the frequency table.
#' @param datasources a list of parameters to access files sytems or databases.
#' @return return a numeric value.
#' @author Paula Raissa Costa e Silva
#' @section Dependencies:
#' \code{\link{getHistogram}}
#' @export
#' @examples {
#' fileNames <- list(file=c("file1.csv", "file2.csv", "file3.csv"))
#' ds.histogram(x='idademae', intervalList=c(20,40,50,30,20,30), datasources=fileNames)
#' }

ds.histogram <- function(x=NULL, intervalList=NULL, datasources=NULL) {

  if(is.null(x)) {
    stop("Please provide the name of the input vector", call. = FALSE)
  }

  if(is.null(datasources)){
    datasources <- findLoginObjects()
  }

  # order.interval <- sort(unique(intervalList))
  #
  # result <- list()
  # if (!is.null(datasources$file)) {
  #   for (file in datasources$file) {
  #     result <- append(result, lapply(x, FUN=getHistogram, intervalList=order.interval, datasources=file))
  #   }
  # } else if (!is.null(datasources$mysql)) {
  #   for (db in datasources$mysql) {
  #     result <- append(result, lapply(x, FUN=getHistogram, intervalList=order.interval, datasources=db))
  #   }
  # }
  #
  # hist <- data.frame(value=order.interval, freq=rep(0, length(order.interval)))
  # for (df in result) {
  #   hist$freq <- hist$freq + df$freq
  # }

  cally <- call('getHistogram', x)
  result <- opal::datashield.aggregate(datasources, cally)

  return(result)

}
