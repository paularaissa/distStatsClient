#'
#' @title Combined Derivative Computation of \eqn{\beta}'s.
#' @description Computes the \eqn{\beta} for each interation.
#' @details The straightforward way to compute the \eqn{b} coefficients is the Newton's method.
#' Suppose that there is a valued function \eqn{y = f(b)}.
#' The problem is find the value \eqn{b[k]} such that \eqn{f(b[k]) = 0}.
#' Starting with an initial value for \eqn{b[0]}, the Taylor expansion of \emph{f} can be done around \eqn{b[0]}:
#' \deqn{f(b[0] + \beta) =~ f(b[0]) + f'(b[0]) * \beta}
#' The \emph{f'} is a matrix, a Jacobean if first derivative of \emph{f} with respect to \emph{b}.
#' In this equation, setting the left side as zero, the \eqn{\beta} can be solved as
#' \deqn{\beta[0] = -[f'(\beta[0])]^(-1) * f(b[0])}
#' So the update of estimated for \emph{b} is:
#' \deqn{b[1] = b[0] + \beta[0]}
#' and iterate until convergence.
#' @param formula a character that can be coerced to an object of class \code{\link[stats]{formula}}. It is a symbolic
#' description of the model to be fitted.
#' @param n.vars a numeric, the number of study variables.
#' @param family a string character with the name of the error distribution and link function to be used in the analysis.
#' If \code{family} is set to 'binomial' it defines the link function as logit, and likelihood as binomial.
#' If \code{family} is set to 'poisson' it defines the link function as log, and likelihood as poisson.
#' @param learningrate a numeric, controls how much we are adjusting the regression model.
#' @param dif a numeric, controls the learning convergence.
#' @param datasources a list of opal object(s) obtained after login in to opal servers;
#' these objects hold also the data assign to R, as \code{data frame}, from opal datasources.
#' @family regressions
#' @section Dependencies:
#' \code{\link{getDerivativeDS}}
#' @return Returns a list with the following components:
#' \item{call}{the model formula.}
#' \item{coefficients}{a vector of linear regression coefficients.}
#' \item{xtxw}{a data matrix, the \emph{Hessian} matrix.}
#' \item{xtyp}{a data matrix, that integrates the computation of derivatives.}
#' @author Paula R. Silva


getDerivative <- function(formula, n.vars, family, learningrate, dif, datasources) {
  beta <- rep(0, (n.vars+1)) # initial beta
  beta.vect.temp <- paste0(as.character(beta), collapse="x")
  derivative <- 1:(n.vars+1)
  diff <- 10000

  while(diff > dif) {
    cally <- call("getDerivativeDS", formula, beta.vect.temp, family)
    result.derivative <- opal::datashield.aggregate(datasources, cally)
    sum.xtxw <- 0
    sum.xtyp <- 0
     for (result in result.derivative) {
       sum.xtxw <- sum.xtxw + result$xtxw
       sum.xtyp <- sum.xtyp + result$xtyp
     }
     derivative <- solve(sum.xtxw) %*% sum.xtyp
     beta = beta + derivative
     beta.vect.temp <- paste0(as.character(beta), collapse="x")
     diff <- sum(derivative^2)
  }

  return(list(call=formula, coefficients=beta, xtxw=sum.xtxw, xtyp=sum.xtyp))
}
