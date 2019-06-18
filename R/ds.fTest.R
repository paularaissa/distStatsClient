#-------------------------------------- HEADER --------------------------------------------#
#' @title Distributed F Test to Compare Two Variances
#' @description Performs an F test to compare the variances of two samples from normal populations.
#' @details The null hypothesis is that the ratio of the variances of the populations from which x and y were drawn, or in
#' the data to which the linear models x and y were fitted, is equal to ratio.
#'
#' @param x a character, the name of a numeric vector of data values.
#' @param y a character, the name of a numeric vector of data values.
#' @param ratio the hypothesized ratio of the population variances of x and y.
#' @param conf.level confidence level for the returned confidence interval.
#' @param alternative a character string specifying the alternative hypothesis, must be one of "two.sided" (default),
#' "greater" or "less". You can specify just the initial letter.
#' @param datasources a list of opal object(s) obtained after login in to opal servers;
#' these objects hold also the data assign to R, as \code{data frame}, from opal datasources.
#'
#' @return A list containing the following components:
#' \item{statistic}{the value of the F test statistic.}
#' \item{parameter}{the degrees of the freedom of the F distribution of the test statistic.}
#' \item{p.value}{the p-value of the test.}
#' \item{conf.int}{a confidence interval for the ratio of the population variances.}
#' \item{estimate}{the ratio of the sample variances of x and y.}
#' \item{null.value}{the ratio of population variances under the null.}
#' \item{alternative}{a character string describing the alternative hypothesis.}
#' \item{method}{the character string "F test to compare two variances".}
#'
#' @author Paula Raissa Costa e Silva
#' @section Dependencies:
#' \code{\link{getLength}}, \code{\link{getVariance}}
#' @export
#' @examples {
#' ds.fTest(x, y)
#' }
#'

ds.fTest <- function(x=NULL, y=NULL, ratio=1, conf.level=0.95, alternative = c("two.sided", "less", "greater"), datasources=NULL) {

  #-------------------------------------- BASIC CHECKS ----------------------------------------------#
  if(is.null(x)){
    stop("Please provide the name of the input vector 'x'", call. = FALSE)
  }
  if (is.null(y)) {
    stop("Please provide the name of the input vector 'y'", call. = FALSE)
  }

  if (!((length(ratio) == 1L) && is.finite(ratio) && (ratio > 0)))
    stop("Please provide a single positive number for 'ratio'")
  if (!((length(conf.level) == 1L) && is.finite(conf.level) && (conf.level > 0) && (conf.level < 1)))
    stop("Please provide a single number between 0 and 1 for 'conf.level'")

  alternative <- match.arg(alternative)

  if(is.null(datasources)){
    datasources <- findLoginObjects()
  }
    xnames <- extract(x)
    varname <- xnames$elements
    obj2lookfor <- xnames$holders

    if(is.na(obj2lookfor)){
      defined <- isDefined(datasources, varname)
    } else {
      defined <- isDefined(datasources, obj2lookfor)
    }


  xLength <- ds.length(x)
  yLength <- ds.length(y)
  xDf <- xLength - 1L

  if (xDf < 1L)
    stop("not enough 'x' observations")

  yDf <- yLength - 1L
  if (yDf < 1L)
    stop("not enough 'y' observations")

  xVar <- ds.var(x=x)
  yVar <- ds.var(x=y)

  estimate <- xVar / yVar
  statistic <- estimate / ratio
  parameter <- c("num df"=xDf, "denom df"=yDf)
  pVal <- pf(statistic, xDf, yDf)

  switch(alternative,
         two.sided = {
          pVal <- 2 * min(pVal, 1-pVal)
          beta <- (1 - conf.level) / 2
          cInt <- c(estimate / qf(1-beta, xDf, yDf),
                    estimate / qf(beta, xDf, yDf))
         },
         greater = {
           pVal <- 1 - pVal
           cInt <- c(estimate / qf(conf.level, xDf, yDf), Inf)
         },
         less = {
           cInt <- c(0, estimate / qf(1 - conf.level, xDf, yDf))
         })

  names(statistic) <- "F"
  names(estimate) <- names(ratio) <- "ratio of variances"
  attr(cInt, "conf.level") <- conf.level

  rVal <- list(statistic = statistic,
               parameter = parameter,
               p.value = pVal,
               conf.int = cInt,
               estimate = estimate,
               null.value = ratio,
               alternative = alternative,
               method = "F test to compare two variances")


  return(rVal)
}

