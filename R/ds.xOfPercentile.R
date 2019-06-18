#-------------------------------------- HEADER --------------------------------------------#
#' @title X Value for Percentile
#' @description Computes the X value for a given percentile in distributed datasources
#' @details Read each data file (node) and compute the combined X value for a given percentile.
#' @param varName a character, the name of study variable.
#' @param percentile a numeric, the given percentile to calculate the corresponding value.
#' @param datasources a list, parameters to access files sytems or databases.
#' @author Paula Raissa Costa e Silva
#' @section Dependencies:
#' \code{\link{getSamplesCounting}}
#' \code{\link{getPercentile}}
#' @export
#' @examples {
#' ds.xOfPercentile('V2', 0.10, datasources)
#' }
#'

ds.xOfPercentile <- function(varName, percentile, datasources){
  if (percentile < 0 || percentile > 100) {
    print("[ERROR: invalid percentile value given]")
    return(NaN)
  }

  ###############################################
  # 1. Read each data file (node) and compute the
  #    local cumDistFunc for each data node
  ###############################################
  CDFs <- list()
  cont <- 1
  if (!is.null(datasources$file)) {
    for (file in datasources$file) {
      CDFs[[cont]] <- getNodeCDF(varName, file)
      cont <- cont + 1
    }
  }
  cdf <- combineCDFs(CDFs)

  ########################################################################
  # 2. Get the combined Cumulative Distribution Function of all data files
  ########################################################################
  xValue <- getXvalueFromPercentile(percentile, cdf)

  cat("\nX value for Percentile", percentile * 100, " is ", xValue)

  invisible(xValue)
}
