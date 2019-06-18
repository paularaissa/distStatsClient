#-------------------------------------- HEADER --------------------------------------------#
#' @title Conditional Combine Cumulative Distribuition Function
#' @description Combine an entire list of Conditioned Cum Dist Funcs.
#' @details The combined values are computed recursively.
#' @param CCDFlist a list of conditional cumulative distance values
#' @author Rui Camacho, Paula Raissa
#'

combineCCDFs <- function(CCDFlist){
  CCDFs <- list()
  cdfFinal <- list()
  categories <- CCDFlist[[1]]$categories

  if(length(CCDFlist) == 1)
    return(CCDFlist[[1]]$CDFs)

  for (node in 1:length(CCDFlist)) {
    for(cat in categories) {
      CCDFs[[cat]] <- append(CCDFs[[cat]], list(CCDFlist[[node]]$CDFs[[cat]]))
    }
  }

  for (cat in categories) {
    cdfFinal[[cat]] <- combineCDFs(CCDFs[[cat]])
  }

  return(cdfFinal)
}
