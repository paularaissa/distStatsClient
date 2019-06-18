#-------------------------------------- HEADER --------------------------------------------#
#' @title Distributed Student's t-Test
#' @description Computes one and two sample t-tests on vectors of data
#' @details If paired is TRUE then both x and y must be specified and they must be the same length. Missing values are silently
#' removed (in pairs if paired is TRUE). If var.equal is TRUE then the pooled estimate of the variance is used. By default, if
#' var.equal is FALSE then the variance is estimated separately for both groups and the Welch modification to the degrees of freedom is used.
#'
#' @param x a character, the name of a numeric vector of data values.
#' @param y x a character, the name of a optional numeric vector of data values.
#' @param mu a number indicating the true value of the mean (or difference in means if you are performing a two sample test).
#' @param paired a logical indicating whether you want a paired t-test.
#' @param conf.level confidence level of the interval.
#' @param var.equal a logical variable indicating whether to treat the two variances as being equal.
#' If TRUE then the pooled variance is used to estimate the variance otherwise the Welch (or Satterthwaite) approximation to the degrees of freedom is used.
#' @param datasources a list of opal object(s) obtained after login in to opal servers;
#' these objects hold also the data assign to R, as \code{data frame}, from opal datasources.
#'
#' @return A list containing the following components:
#' \item{statistic}{the value of the t-statistic.}
#' \item{parameter}{the degrees of freedom for the t-statistic.}
#' \item{p.value}{the p-value for the test.}
#' \item{conf.int}{a confidence interval for the mean appropriate to the specified alternative hypothesis.}
#' \item{estimate}{the estimated mean or difference in means depending on whether it was a one-sample test or a two-sample test.}
#' \item{null.value}{the specified hypothesized value of the mean or mean difference depending on whether it was a one-sample test or a two-sample test.}
#' \item{method}{a character string indicating what type of t-test was performed.}
#'
#' @author Paula Raissa Costa e Silva
#' @section Dependencies:
#' \code{\link{getLength}}, \code{\link{getVariance}}
#' @export
#' @examples {
#' ds.tTest(x)
#' ds.tTest(x, y)
#' }
#'

ds.tTest <- function(x, y=NULL, mu=0, paired=FALSE, conf.level=0.95, var.equal=FALSE, datasources=NULL) {
  #-------------------------------------- BASIC CHECKS ----------------------------------------------#
  if(is.null(x)){
    stop("Please provide the name of the input vector", call. = FALSE)
  }
  if (!missing(mu) && (length(mu)!=1 || is.na(mu)))
    stop("Please provide a single value as mu")
  if (!missing(conf.level) && (length(conf.level) != 1 || !is.finite(conf.level) || conf.level < 0 || conf.level > 1))
    stop("Please provide a single number between 0 and 1")

  # if no opal login details are provided look for 'opal' objects in the environment
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


  xLenght <- ds.length(x, datasources=datasources)
  xMean <- ds.arMean(x, datasources=datasources)
  xVar <- ds.var(x, datasources=datasources)

  if (!is.null(y)) {
    yLenght <- ds.length(y, datasources=datasources)
    if(xLenght < 1 || (!var.equal && xLenght < 2)) stop("not enough 'x' observations")
    if(yLenght < 1 || (!var.equal && yLenght < 2)) stop("not enough 'y' observations")
    if(var.equal && xLenght+yLenght < 3) stop("not enough observations")
    yMean <- ds.arMean(y, datasources=datasources)
    yVar <- ds.var(y, datasources=datasources)
    method <- paste(if(!var.equal) "Welch", "Two Sample t-test")
    estimate <- c(xMean, yMean)
    names(estimate) <- c("mean of x", "mean of y")
    if(var.equal) {
      df <- xLenght + yLenght - 2
      variance <- 0
      if(xLenght > 1) variance <- variance + (xLenght-1) * xVar
      if(yLenght > 1) variance <- variance + (yLenght-1) * yVar
      variance <- variance / df
      stderr <- sqrt(variance * (1/xLenght + 1/yLenght))
    } else {
      stderrx <- sqrt(xVar / xLenght)
      stderry <- sqrt(yVar / yLenght)
      stderr <- sqrt(stderrx^2 + stderry^2)
      df <- stderr^4 / (stderrx^4/(xLenght-1) + stderry^4 / (yLenght-1))
    }
    if(stderr < 10 * .Machine$double.eps * max(abs(xMean), abs(yMean))) stop("data are essentially constant")
    tstat <- (xMean - yMean - mu)/stderr
  } else {
    if(xLenght < 2) stop("not enough 'x' observations")
    df <- xLenght - 1
    stderr <- sqrt(xVar / xLenght)
    if(stderr < 10 * .Machine$double.eps * abs(xMean)) stop("data are essentially constant")
    tstat <- (xMean - mu) / stderr
    method <- if (paired) "Paired t-test" else "One Sample t-test"
    estimate <- setNames(xMean, if(paired) "mean of the differences" else "mean of x")
  }

  # mean <- ds.arMean(x, type, datasources)
  # sem <- ds.sem(x, type, datasources)
  #
  # t <- (mean - mu) / sem

  pval <- 2 * pt(-abs(tstat), df)
  alpha <- 1 - conf.level
  # Confidence interval
  confidence <- qt(1 - alpha/2, df)
  confidence <- tstat + c(-confidence, confidence)
  confidence <- mu + confidence * stderr

  names(tstat) <- "t"
  names(df) <- "df"
  names(mu) <- if(paired || !is.null(y)) "difference in means" else "mean"
  attr(confidence, "conf.level") <- conf.level
  t <- list(statistic = tstat, parameter = df, p.value = pval,
            conf.int = confidence, estimate = estimate, null.value = mu,
            method = method)

  return(t)
}

