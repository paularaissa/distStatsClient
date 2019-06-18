#-------------------------------------- HEADER --------------------------------------------#
#' @title Distributed test for Equal Means in a One-Way Layout
#' @description Test whether two or more samples from normal distributions have the same means.
#' The variances are not necessarily assumed to be equal.
#' @details If the right-hand side of the formula contains more than one term, their interaction is taken to form the grouping.
#'
#' @param formula a text, transformed to formula object of the form y ~ x where 'y' gives the sample values and 'x' the corresponding groups.
#' @param subset an optional vector specifying a subset of observations to be used.
#' @param na.action a function which indicates what should happen when the data contain NAs. Defaults to getOption("na.action").
#' @param var.equal a logical variable indicating whether to treat the variances in the samples as equal. If TRUE, then a simple F test for
#' the equality of means in a one-way analysis of variance is performed. If FALSE, an approximate method of Welch (1951) is used, which generalizes
#' the commonly known 2-sample Welch test to the case of arbitrarily many samples.
#' @param datasources a list of opal object(s) obtained after login in to opal servers;
#' these objects hold also the data assign to R, as \code{data frame}, from opal datasources.
#'
#' @return A list containing the following components:
#' \item{statistic}{the value of the test statistic.}
#' \item{parameter}{the degrees of freedom of the exact or approximate F distribution of the test statistic.}
#' \item{p.value}{the p-value of the test.}
#'
#' @author Paula Raissa Costa e Silva
#' @section Dependencies:
#' \code{\link{getLength}}, \code{\link{getVariance}}
#' @export
#' @examples {
#' #For equal variances
#' ds.oneWayTest(folate ~ ventilation, data = redCellFolate, var.equal=TRUE)
#' #Not assuming equal variances
#' ds.oneWayTest(folate ~ ventilation, data = redCellFolate)
#' #The result must be the same as
#' ds.anova(ds.linear(folate ~ ventilation, data = redCellFolate))
#' }
#'

ds.oneWayTest <- function(formula, subset=NULL, na.action, var.equal=TRUE, datasources=NULL) {

  return("OK")

}
