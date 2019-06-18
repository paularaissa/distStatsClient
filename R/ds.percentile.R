#-------------------------------------- HEADER --------------------------------------------#
#' @title Distributed Percentile
#' @description Computes the distributed percentile for a given study variable.
#' @details Read each data file (node) and compute the combined percentile value.
#' @param x a character, the name of study variable.
#' @param xValue the given value to calculate the corresponding percentile.
#' @param datasources a list of opal object(s) obtained after login in to opal servers;
#' these objects hold also the data assign to R, as \code{data frame}, from opal datasources.
#' @author Rui Camacho, Paula Raissa
#' @section Dependencies:
#' \code{\link{getSamplesCounting}}
#' \code{\link{getPercentile}}
#' @export
#' @examples {
#' ds.percentile(varName, 10, fileNames)
#' }

ds.percentile <- function(varName=NULL, formula=NULL, xValue=NULL, cVar=NULL, relation=NULL, threshold=NULL, relation2=NULL, threshold2=NULL){

  # if no opal login details are provided look for 'opal' objects in the environment
  datasources <- findLoginObjects()

  ###############################################
  # 1. Read each data file (node) and compute the
  #    local cumDistFunc for each data node
  ###############################################
  tempFormula <- paste0('x', formula)
  originalFormula <- Reduce(paste, deparse(tempFormula))
  formula2use <- as.formula(paste0(Reduce(paste, deparse(originalFormula)))) # here we need the formula as a 'call' object
  cally <- call('getSamplesCounting', varName, formula2use, xValue, cVar, relation, threshold, relation2, threshold2)
  numSamples <- opal::datashield.aggregate(datasources, cally)

  return(numSamples)

  ###############################################
  # 1. Read each data file (node) and compute the
  #    local cumDistFunc for each data node
  ###############################################
  #num.sources <- length(datasources)

  #teste <- gsub("~", '789', formula)
  #cally <- paste0("getSamplesCounting('",paste("my text", format(frm)),"')")
  # #form.text <- Reduce(paste, deparse(formula))
  # test <- gsub(")","",model.formula[2])
  # cally <- call("getSamplesCounting", test)
  # numSamples <- opal::datashield.aggregate(datasources, cally)

  # for (i in 1:num.sources) {
  #   cally <- call("getSamplesCounting", varName, as.character(formula))
  #   # cally <- call("getSamplesCounting", varName, model.formula)
  #   numSamples <- opal::datashield.aggregate(datasources, cally)
  # }
  # return(numSamples)

  ########################################################################
  # 2. Get the combined Cumulative Distribution Function of all data files
  ########################################################################
  # percentile <- getPercentile(numSamples)
  # cat("\n Percentile of x=", xValue, " is ", percentile * 100)
  #
  # invisible(percentile * 100)
}
