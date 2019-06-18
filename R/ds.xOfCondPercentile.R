#-------------------------------------- HEADER --------------------------------------------#
#' @title X Value for Conditional Percentile
#' @description Computes the X value for a given conditional percentile in distributed datasources
#' @details Read each data file (node) and compute the combined X value for a given percentile, according to a given category variable and name.
#' @param varName a character, the name of study variable. It must be a numerical vector.
#' @param varCategory a character, the name of the categorical variable.
#' @param category a character, the category value.
#' @param percentile a numeric, the percentile value.
#' @param datasources a list of parameters to access files sytems or databases.
#' @author Paula Raissa Costa e Silva
#' @section Dependencies:
#' \code{\link{getSamplesCounting}}
#' \code{\link{getPercentile}}
#' @export
#' @examples {
#' ds.xOfCondPercentile('V3', 'gender', 'male', 0.1, datasources)
#' ds.xOfCondPercentile('V3', 'gender', 'female', 0.65, datasources)
#' }
#'

ds.xOfCondPercentile <- function(varName, varCategory, category, percentile, datasources){
  if (percentile < 0 || percentile > 100) {
    print("[ERROR: invalid percentile value given]")
    return(NaN)
  }

  CCDFs <- list()
  idx <- 1
  if (!is.null(datasources$file)) {
    for (file in datasources$file) {
      CCDFs[[idx]] <- getNodeCCDF(varName, varCategory, file)
      idx <- idx + 1
    }
  }

  ccdf = combineCCDFs(CCDFs)

  xValue <- getXvalueFromPercentile(percentile, ccdf[[category]])

  cat("\nX value for Conditional Percentile", percentile * 100, " is ", xValue)

  invisible(xValue)
}
