#-------------------------------------- HEADER --------------------------------------------#
#' @title Combine 2 Cumulative Distances
#' @description Computes the combined values for 2 cumulative distances.
#' @details The combined values are computed by linear interpolation.
#' @param cdf1 a list of cumulative distance values.
#' @param cdf2 a list of cumulative distance values.
#' @author Rui Camacho, Paula Raissa
#' @export
#'

combine2CDFs <- function(cdf1, cdf2) {
  xValues <- c()
  cumValues <- c()
  idx1 <- 1
  idx2 <- 1
  acum1 <- 0
  acum2 <- 0

  while (idx1 <= length(cdf1$xValues) && idx2 <= length(cdf2$xValues)) {
    if (cdf1$xValues[idx1] == cdf2$xValues[idx2]) {
      xValues <- append(xValues, cdf1$xValues[idx1])
      cumValues <- append(cumValues, cdf1$cdfValues[idx1] + cdf2$cdfValues[idx2])
      acum1 <- cdf1$cdfValues[idx1]
      acum2 <- cdf2$cdfValues[idx2]
      idx1 <- idx1 + 1
      idx2 <- idx2 + 1
    }
    else if (cdf1$xValues[idx1] < cdf2$xValues[idx2]) {
      xValues <- append(xValues, cdf1$xValues[idx1])
      cumValues <- append(cumValues, cdf1$cdfValues[idx1] + acum2)
      acum1 <- cdf1$cdfValues[idx1]
      idx1 <- idx1 + 1
    } else {
      xValues <- append(xValues, cdf2$xValues[idx2])
      cumValues <- append(cumValues, cdf2$cdfValues[idx2] + acum1)
      acum2 <- cdf2$cdfValues[idx2]
      idx2 <- idx2 + 1
    }
  }

  if (idx1 <= length(cdf1$xValues)) {
    while (idx1 <= length(cdf1$xValues)) {
      xValues <- append(xValues, cdf1$xValues[idx1])
      cumValues <- append(cumValues, cdf1$cdfValues[idx1] + acum2)
      idx1 <- idx1 + 1
    }
  } else {
    while (idx2 <= length(cdf2$xValues)) {
      xValues <- append(xValues, cdf2$xValues[idx2])
      cumValues <- append(cumValues, cdf2$cdfValues[idx2] + acum1)
      idx2 <- idx2 + 1
    }
  }
  return(list(xValues, cumValues))
}
