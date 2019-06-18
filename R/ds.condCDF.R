#-------------------------------------- HEADER --------------------------------------------#
#' @title Conditional Cumulative Distributed Function
#' @description Computes the distributed cumulative distribution function.
#' @details Reads each datasource (node) and compute the local cumulative distributed function for each data node.
#' If the datasources are files, these files must be in .csv formate.
#' @param varName a character, the name of study variable. The data type must be a numeric.
#' @param catName a character, the name of categorical variable.
#' @param datasources a list of parameters to access files sytems or databases.
#' @return The combined cumulative distribution function of all datasources.
#' @author Rui Camacho, Paula Raissa
#' @export
#' @examples {
#' ds.cdf(varName, fileNames)
#' }
#'

ds.condCDF <- function(varName, catName, datasources) {

  cat("Computing the distributed Conditional Cumulative Distribution Function using", length(datasources$file), "nodes")

  ###############################################
  # 1. Read each data file (node) and compute the
  #    local conditioned cumDistFunc for each data node
  ###############################################
  CCDFs <- list()
  idx <- 1
  if (!is.null(datasources$file)) {
    for (file in datasources$file) {
      CCDFs[[idx]] <- getNodeCCDF(catName, varName, file)
      idx <- idx + 1
    }
  }

  #####################################################################################
  # 2. Get the combined Conditional Cumulative Distribution Function of all data files
  #####################################################################################
  ccdf <- combineCCDFs(CCDFs)
  cat("\nCombined CFD values: \n")
  print(ccdf)

  invisible(ccdf)
}
