#-------------------------------------- HEADER --------------------------------------------#
#' @title Distributed Conditional Percentile
#' @description Computes the distributed conditional percentile for a given study and categorical variables.
#' @details Read each datasource (node) and compute the combined percentile according to categories.
#' @param varName a character, the name of numerical variable.
#' @param catName a character, the name of categorical variable.
#' @param category the given category to calculate the corresponding percentile.
#' @param xValue the given value to calculate the corresponding percentile.
#' @param datasources a list of parameters to access files sytems or databases.
#' @author Rui Camacho, Paula Raissa
#' @section Dependencies:
#' \code{\link{getSamplesCounting}}
#' \code{\link{getPercentile}}
#' @export
#' @examples {
#' fileNames <- c("juul.csv", "juul2.csv")
#' varName <- "age"
#' catName <- "sex"
#' ds.CondPercentile(varName, catName, 'f', 10, fileNames)
#' }
ds.condPercentile <- function(varName, catName, category, xValue, datasources){

  ###############################################
  # 1. Read each datasource (node) and compute the
  #    local condSamplesCounting for each data node
  ###############################################
  cont <- 1
  numSamples <- list()
  if (!is.null(datasources$file)) {
    for (filename in datasources$file) {
      numSamples[[cont]] <- getCondSamplesCounting(varName, catName, category, xValue, filename)
      cont <- cont + 1
    }
  }

  ########################################################################
  # 2. Get the combined conditional percentile of all datasources
  ########################################################################
  percentile <- getPercentile(numSamples)

  cat("\n Conditional Percentile of x=", xValue, " category=", category, " is ", percentile * 100)

  invisible(percentile * 100)
}
