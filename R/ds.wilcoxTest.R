#-------------------------------------- HEADER --------------------------------------------#
#' @title Distributed Wilcoxon Rank Sum and Signed Rank Tests
#' @description Computes one and two-sample Wilcoxon tests on vectors of data.
#'
#' @details The formula interface is only applicable for the 2-sample tests.
#' f only x is given, or if both x and y are given and paired is TRUE, a Wilcoxon signed rank test of the null that the
#' distribution of x (in the one sample case) or of x - y (in the paired two sample case) is symmetric about mu is performed.
#' Otherwise, if both x and y are given and paired is FALSE, a Wilcoxon rank sum test
#' is carried out. In this case, the null hypothesis is that the distributions of x and y differ by a location shift of mu and
#' the alternative is that they differ by some other location shift (and the one-sided alternative "greater" is that x is shifted to the right of y).
#'
#' @param x a character, the name of a numeric vector of data values.
#' @param y a character, the name of an optional numeric vector of data values.
#' @param alternative a character string specifying the alternative hypothesis, must be one of "two.sided" (default), "greater" or "less".
#' You can specify just the initial letter.
#' @param mu anumber specifying an optional parameter used to form the null hypothesis.
#' @param paired a logical indicating whether you want a paired test.
#' @param exact a logical indicating whether an exact p-value should be computed.
#' @param correct a logical indicating whether to apply continuity correction in the normal approximation for the p-value.
#' @param conf.int a logical indicating whether a confidence interval should be computed.
#' @param conf.level confidence level of the interval.
#' @param datasources a list of opal object(s) obtained after login in to opal servers;
#' these objects hold also the data assign to R, as \code{data frame}, from opal datasources.
#'
#' @return A list containg the following components:
#' \item{statistic}{the value of the test statistic with a name describing it.}
#' \item{parameter}{the parameter(s) for the exact distribution of the test statistic.}
#' \item{p.value}{the p-value for the test.}
#' \item{null.value}{the location parameter mu.}
#' \item{conf.int}{a confidence interval for the location parameter. If argument conf.int = TRUE.}
#' \item{estimate}{an estimate of the location parameter. If argument conf.int = TRUE.}
#'
#' @author Paula Raissa Costa e Silva
#' @section Dependencies:
#' \code{\link{getLength}}, \code{\link{getVariance}}
#' @export
#' @examples {
#' ds.wilcoxTest(x)
#' ds.wilcoxTest(x, y)
#' }
#'

ds.wilcoxTest <- function(x, y = NULL, alternative = c("two.sided", "less", "greater"), mu = 0, paired = FALSE, exact = NULL,
                          correct = TRUE, conf.int = FALSE, conf.level = 0.95, datasources=NULL) {

  #-------------------------------------- BASIC CHECKS ----------------------------------------------#
  alternative <- match.arg(alternative)
  if(!missing(mu) && ((length(mu) > 1L) || !is.finite(mu))) stop("'mu' must be a single number")

  if(conf.int) {
    if(!((length(conf.level) == 1L) && is.finite(conf.level) && (conf.level > 0) && (conf.level < 1)))
      stop("'conf.level' must be a single number between 0 and 1")
  }
  xLength <- ds.length(x, datasources = datasources)
  if (!is.null(y)) {
    yLength <- ds.length(y, datasources = datasources)
    if (paired) {
      if (xLength != yLength) stop("The variables 'x' and 'y' must have the same length")
    }
  } else {
    if (paired) stop("Please provide the 'y' vector for paired test")
  }

}
