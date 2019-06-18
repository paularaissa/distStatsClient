#-------------------------------------- HEADER --------------------------------------------#
#' @title Poisson Regression
#' @description Computes Poisson regression model.
#' It can be used to fit univariate, multivariate and weighted Poisson models.
#' @details It is a wrapper for the client side function \code{\link{getDerivative}},
#' setting the model \code{family} as 'poisson'.
#' @param formula a character that can be coerced to an object of class \code{\link[stats]{formula}}. It is a symbolic
#' description of the model to be fitted.
#' The Newton Raphson Method is apllied to compute the Poisson coefficients.
#' @param learningrate a numeric, controls how much we are adjusting the regression model.
#' @param dif a numeric, controls the learning convergence.
#' @param checks a boolean, if TRUE (default) checks that verify elements on the server side
#' such checks lengthen the run-time so the default is FALSE and one can switch these checks
#' on (set to TRUE) when faced with some error(s).
#' @param datasources a list of opal object(s) obtained after login in to opal servers;
#' these objects hold also the data assign to R, as \code{data frame}, from opal datasources.
#' @return Returns a list with the following components:
#' \item{call}{the model formula.}
#' \item{coefficients}{a vector of poisson regression coefficients.}
#' \item{xtxw}{a data matrix, the \emph{Hessian} matrix.}
#' \item{xtyp}{a data matrix, that integrates the computation of derivatives.}
#' @family regressions
#' @section Dependencies:
#' \code{\link[distStatsC]{getDerivative}}
#' @author Paula Raissa Costa e Silva
#' @export
#' @examples {
#' ds.poisson('D$y ~ D$x')
#' }

ds.poisson <- function(formula, learningrate=0.01, dif=0.000000001, checks=FALSE, datasources=NULL) {

  #-------------------------------------- BASIC CHECKS ----------------------------------------------#
  # if no opal login details are provided look for 'opal' objects in the environment
  if(is.null(datasources)){
    datasources <- findLoginObjects()
  }

  model.formula <- as.formula(formula)
  model.vars.aux <- all.vars(model.formula, functions = FALSE, unique = FALSE)
  model.vars <- model.vars.aux[model.vars.aux != "D"]
  response <- model.vars[1]
  x.vars <- model.vars[-1]

  if(is.null(response)){
    stop("Please provide the name of the response vector!", call.=FALSE)
  }
  if(is.null(x.vars)){
    stop("Please provide the name of the predictors variables!", call.=FALSE)
  }

  # the input variable might be given as column table (i.e. D$x)
  # or just as a vector not attached to a table (i.e. x)
  # we have to make sure the function deals with each case
  xnames <- extract(x.vars)
  ynames <- extract(response)
  varnamex <- xnames$elements
  varnamey <- ynames$elements
  objxlookfor <- xnames$holders
  objylookfor <- ynames$holders

  #--------------------------------------------------------------------------------------------------#
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
  #----------------------------------------------------------------------------------------------------#

  # number of studies
  num.sources <- length(datasources)

  #-------------------------------------- CALLING SERVER SIDE FUNCTION --------------------------------#

  n.vars <- length(x.vars)
  family <- "poisson"
  logistic <- getDerivative(model.formula, n.vars, family, learningrate, dif, datasources)

  return(logistic)



}
