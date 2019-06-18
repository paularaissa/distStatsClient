#-------------------------------------- HEADER --------------------------------------------#
#' @title Summarizing Regression Models
#' @description Combined computations for model checking.
#' @details Combines the computation of the coefficients, standard errors, etc.
#' The coefficients component of the result gives the estimated coefficients and their estimated standard errors,
#' together with their ratio. This third column is labelled t ratio if the dispersion is estimated, and z ratio if the
#' dispersion is known (or fixed by the family). A fourth column gives the two-tailed p-value corresponding to the t or
#' z ratio based on a Student t or Normal reference distribution. (It is possible that the dispersion is not known and
#' there are no residual degrees of freedom from which to estimate it. In that case the estimate is NaN.)
#' @param regression an object of regression model.
#' @param formula a string character to be transformed as an object of class \code{formula}.
#' @param datasources a list of opal object(s) obtained after login in to opal servers;
#' these objects hold also the data assign to R, as \code{data frame}, from opal datasources.
#' @return Returns a list with the following components:
#' \item{residuals}{the weighted residuals, the usual residuals rescaled by the square root of the weights specified in the call to lm.}
#' \item{residuals.quantis}{the residuals quartiles}
#' \item{coefficients}{the matrix of coefficients, standard errors, z-values and p-values. Aliased coefficients are omitted.}
#' \item{sigma}{the square root of the estimated variance of the random error.}
#' \item{df}{degrees of freedom, a 3-vector (p, n-p, p*), the first being the number of non-aliased coefficients, the last being the total number of coefficients.}
#' \item{r.squared}{\eqn{R^2}, the fraction of variance explained by the model
#' \deqn{R^2 = 1 - Sum(R[i]^2) / Sum((y[i]-y*)^2)},
#' where \eqn{y*} is the mean of y[i] if there is an intercept and zero otherwise.}
#' \item{adj.r.squared}{the above \eqn{R^2} statistic 'adjusted', penalizing for higher \emph{p}.}
#' \item{fstatistic}{(for models including non-intercept terms) a 3-vector with the value of the F-statistic with its numerator and denominator degrees of freedom.}
#' \item{pvalue}{p-value from f-statistic.}
#' \item{n}{the sample size.}
#' \item{cov.unscaled}{the unscaled (dispersion = 1) estimated covariance matrix of the estimated coefficients.}
#' @author Paula Raissa Costa e Silva
#' @section Dependencies:
#'  \code{\link{modelCheckingDS}}
#' @export
#' @examples {
#' linear <- ds.linear('D$maternal_age~D$birth_weight')
#' logistic <- ds.logistic('menarche ~ D$age')
#' poisson <- ds.poisson('D$y ~ D$x')
#' ds.summary(linear)
#' ds.summary(logistic)
#' ds.summary(poisson)
#' }


ds.modelCheckings <- function(regression=NULL, formula=NULL, weight=NULL, datasources=NULL){

  result.residuals <- list()

  # if no opal login details are provided look for 'opal' objects in the environment
  if (is.null(datasources)) {
    datasources <- findLoginObjects()
  }

  if (!(is.null(regression))) {
    beta.reg <- regression
  }

  #Get the coefficients
  if (!(is.null(formula))){
    beta.reg <- ds.linear(formula=formula, weight=weight, datasources=NULL)
  }

  #Data transformations
  beta.vect.temp <- paste0(as.character(beta.reg$coefficients), collapse="x")

  #Residuals
  media_y <- beta.reg$sum.y / beta.reg$n.rows
  media.y.temp <- paste0(as.character(media_y))


  cally <- call('modelCheckingDS', beta.vect.temp, beta.reg$call, media.y.temp)
  result.residuals <- opal::datashield.aggregate(datasources, cally)


  n <- 0
  lbx <- 0
  residuals <- matrix()
  yhat <- matrix()
  tbx <- 0
  mean_y <- 0
  quad_y <- 0
  sse <- 0
  sst <- 0
  sum_std_residuals <- 0
  cols_x <- 0

  for(residual in result.residuals) {
    n <- n + residual$rows.x
    residuals <- rbind(residuals, residual$residuals)
    yhat <- rbind(yhat, residual$y.hat)
    tbx <- as.matrix(tbx + residual$tbx)
    lbx <- lbx + residual$lbx
    mean_y <- mean_y + residual$mean.y
    quad_y <- quad_y + residual$quad.y
    sse <- sse + residual$sse
    sst <- sst + residual$sst
    sum_std_residuals <- sum_std_residuals + residual$std.residuals
    cols_x <- residual$cols.x
  }

  #ssr : sum of squares regression
  ssr <- sst - sse

  #coefficient of determination
  r_2 <- ssr / sst

  #Residuals
  residuals_fim <- residuals[-1]
  resisuals_quantis <- quantile(residuals_fim)
  residuals.quantis.fim <- data.matrix(resisuals_quantis)
  ##colnames(residuals.quantis.fim) <- c("Min", "1Q", "Median", "3Q", "Max")

  k <- cols_x - 1
  s2 <- sum(residuals_fim ^ 2) / (n - 2)
  s <- sqrt(s2)
  freedon <- n - k - 1

  #std error
  mVarCovar <- s2 * chol2inv(chol(beta.reg$sum.xtx)) # variance covariance matrix
  vStdErr <- sqrt(diag(mVarCovar)) # coeff. est. standard errors

  # Adjusted R-squared
  r2_adj <- 1 - (((beta.reg$n.rows-1)/(beta.reg$n.rows-(k+1))) * (1-r_2))
  r2_adj <- 1 - (((1-r_2)*(n-1))/(n-k-1))

  # F-Statistic
  f_test <- (ssr / 1) / (sse/(freedon))

  # t-statistics value
  t_value <- (beta.reg$coefficients - 0) / as.matrix(vStdErr)

  # p-value from f-statistic
  p.value <- pf(f_test, 1, freedon, lower.tail=FALSE)

  # p-value from t-test
  pr1 <- 2 * pt(-abs(t_value), freedon)

  coefficients <- data.frame(estimate=beta.reg$coefficients, std.error=vStdErr, t.value=t_value, pt.t=pr1)
  #colnames(coefficients) <- c("Estimate", "Std. Error", "t value", "Pr(>|t|)")

  #Return the calculated values
  invisible(list(residual=residuals, residuals.quantis=residuals.quantis.fim, coefficients=coefficients, sigma=s, df=freedon, r.squared=r_2, adj.r.squared=r2_adj, fstatistic=f_test, pvalue=p.value, n=n, cov.unscaled=mVarCovar))

  #return(result.residuals)
}
