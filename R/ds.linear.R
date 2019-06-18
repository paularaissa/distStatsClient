#-------------------------------------- HEADER --------------------------------------------#
#' @title Linear Regression
#' @family regressions
#' @description Computes linear models. It can be used to fit univariate, multivariate and weighted linear models.
#' It also can be used to compute single stratum analysis of variance and analysis of covariance.
#' @details Models for ds.linear are specified symbolically.
#' A typical model has the form response "~" terms where response is a numeric vector and the terms is a series of terms which
#' specifies a linear predictor for response.
#' A terms specification of the form first + second indicates all the terms in first together with all the terms in second with
#' duplicates removed.
#' A specification of the form first:second indicates the set of terms obtained by taking the interactions of all terms in
#' first with all terms in second. The specification first*second indicates the cross of first and second.
#'
#' Non-NULL weights can be used to indicate that each independent variable have different variances.
#'
#' In the case of distributed univariate and multivariate linear regression, the coefficients are
#' computed by least squares using the \emph{the method of matrices}.
#' Mathematically, the method of matrices has the same approach than the other methods, but the data is transformed
#' into matrices \emph{A} and \emph{g}.
#'
#' According to \insertCite{walpole1993probability}{distStatsS}, the method of matrices consists in build the
#' matrix \emph{A}, the matrix \emph{g} and calculate the coefficients by the equation \eqn{b={A}^{-1}g}.
#' In distributed environments without data sharing the solution is compute the matrices
#' \emph{A} and \emph{g} for each data node, and return these results to the central node.
#' The central node combines $A$ and $g$, and compute the regression coefficients by the equation \eqn{b={A}^{-1}g}.
#'
#' ds.linear calls the server side function \code{\link{matrixMethod2DS}}, to compute the matrices
#' \emph{A} and \emph{g}.
#' @section Dependencies:
#' \code{\link{matrixMethod2DS}}
#' @param formula a character that can be coerced to an object of class \code{\link[stats]{formula}}. It is a symbolic
#' description of the model to be fitted.
#' The details about the model specification are given under 'Details' section.
#' @param weight a character, the name of an optional vector of weights to be used in the fitting process.
#' Should be null or a numeric vector.
#' If it is not NULL, the weighted least squares is computed, otherwise ordinary least squares is computed.
#' See also 'Details'.
#' @param type a character which represents the type of analysis to carry out.
#' If \code{type} is set to 'combine', a global quantile is calculated;
#' if \code{type} is set to 'split', the quantile is calculated separately for each study
#' @param checks a boolean, if TRUE (default) checks that verify elements on the server side
#' such checks lengthen the run-time so the default is FALSE and one can switch these checks
#' on (set to TRUE) when faced with some error(s).
#' @param datasources a list of opal object(s) obtained after login in to opal servers;
#' these objects hold also the data assign to R, as \code{data frame}, from opal datasources.
#' @return Returns a list with the following components:
#'         \item{call}{the model formula.}
#'         \item{coefficients}{a vector of linear regression coefficients.}
#'         \item{n.rows}{numerical, the sample size.}
#'         \item{sum.y}{numerical, the sum of elements for a given dependent variable \emph{y}.}
#'         \item{sum.xtx}{matrix, the combined A matrix.}
#'
#' @author Paula Raissa Costa e Silva
#' @references
#' \insertRef{walpole1993probability}{distStats}
#' @export
#' @family regressions
#' @examples {
#' lm <- ds.linear('D$maternal_age~D$birth_weight')
#' }

ds.linear = function(formula, weight=1, type='combine', checks=FALSE, datasources=NULL){

  if(is.null(formula)){
    stop("Please provide the model formula!", call.=FALSE)
  } else {
    model.formula <- as.formula(formula)
  }

  result <- list()

  # if no opal login details are provided look for 'opal' objects in the environment
  if(is.null(datasources)){
    datasources <- findLoginObjects()
  }

    # the input variable might be given as column table (i.e. D$x)
    # or just as a vector not attached to a table (i.e. x)
    # we have to make sure the function deals with each case
    model.vars.aux <- all.vars(model.formula, functions = FALSE, unique = FALSE)
    model.vars <- model.vars.aux[model.vars.aux != "D"]
    depVar <- model.vars[1]
    indepVars <- model.vars[-1]
    xnames <- extract(indepVars)
    ynames <- extract(depVar)
    varnamex <- xnames$elements
    varnamey <- ynames$elements
    objxlookfor <- xnames$holders
    objylookfor <- ynames$holders

    #-------------------------------------- SERVER SIDE CHECKS ----------------------------------------#
    if(checks){
      # check if the input object(s) is(are) defined in all the studies
      if(is.na(objxlookfor)){
        defined <- isDefined(datasources, varnamex)
      }else{
        defined <- isDefined(datasources, objxlookfor)
      }
      if(is.na(objylookfor)){
        definedy <- isDefined(datasources, varnamey)
      } else {
        definedy <- isDefined(datasources, objylookfor)
      }
    }
    # number of studies
    num.sources <- length(datasources)
    for (i in 1:num.sources){
      cally <- call('matrixMethod', model.formula)
      result <- opal::datashield.aggregate(datasources, cally)
    }


  #-------------------------------------- FINALIZING RESULTS ------------------------------------------#
  sum.xtx <- 0
  sum.xty <- 0

  nrows <- 0
  sum_y <- 0
  for (node in result) {
    sum.xtx <- sum.xtx + node$xtx
    sum.xty <- sum.xty + node$xty
    nrows <- nrows + node$n.rows
    sum_y <- sum_y + node$sum.y
  }

  #Regression
  #beta.reg <- matlib::inv(as.matrix(sum.xtx)) %*% as.matrix(sum.xty)

  #Solution for problems with 'floating-point'
  beta.reg <- qr.coef(qr(as.matrix(sum.xtx)), sum.xty)

  colnames(beta.reg) = "Coeficients"
  #rownames(beta.reg) = rbind("(Intercept)", indepVars)

  return(list(call=model.formula, coefficients=beta.reg, n.rows=nrows, sum.y=sum_y, sum.xtx=sum.xtx))

}
