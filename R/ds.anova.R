#-------------------------------------- HEADER --------------------------------------------#
#' @title Fit an Analysis of Variance Model
#' @description Computes the combined equations for analysis of variance.
#' @details The variation between and within groups for a one-way analysis of variance generalizes to \emph{model variation}
#' and \emph{residual variation} which partition the total variation \eqn{SSD[model] = \sum[i](y[i]-y*)^2}.
#' This can be applied only when the model contains an intercept.
#' The model is considered to be statistically significant if it can account for a large amount of variability in the response.
#' @param formula a character that can be coerced to an object of class \code{\link[stats]{formula}}. It is a symbolic
#' description of the model to be fitted.
#' @param model a character, the name of regression model.
#' If \code{model} is set to 'linear', computes Anova for a linear model;
#' if \code{model} is set to 'logistic', computes Anova for a logistic model;
#' if \code{model} is set to 'poissin', computes Anova for a Poisson model.
#' @param weights a character, the name of an optional vector of weights to be used in the fitting process.
#' Should be null or a numeric vector.
#' @param learningrate a numeric, controls how much we are adjusting the regression model.
#' It is an optional parameter. Should be set if the Anova will be computed for logistic ou Poisson model.
#' @param dif a numeric, controls the learning convergence.
#' It is an optional parameter. Should be set if the Anova will be computed for logistic ou Poisson model.
#' @param checks a boolean, if TRUE (default) checks that verify elements on the server side
#' such checks lengthen the run-time so the default is FALSE and one can switch these checks
#' on (set to TRUE) when faced with some error(s).
#' @param datasources a list of opal object(s) obtained after login in to opal servers;
#' these objects hold also the data assign to R, as \code{data frame}, from opal datasources.
#' @return Returns the Anova table with the following components:
#'         \item{Df}{degrees of freedon}
#'         \item{Sum.Sq}{sum of squares}
#'         \item{Mean.Sq}{mean of squares}
#'         \item{F.value}{f-test}
#'         \item{Pr(>F)}{p-value from f-statistic}
#' @author Paula Raissa Costa e Silva
#' @section Dependencies:
#' \code{\link{getAnova}}
#' @export

ds.anova <- function(formula=NULL, model, weights=NULL, learningrate=0.01, dif=0.000000001, checks=FALSE, datasources=NULL) {

  if(is.null(datasources)){
    datasources <- findLoginObjects()
  }

  if(is.null(formula)){
    stop("Please provide the model formula!", call.=FALSE)
  } else {
    model.formula <- as.formula(formula)
    model.vars.aux <- all.vars(model.formula, functions = FALSE, unique = TRUE)
    model.vars <- model.vars.aux[model.vars.aux != "D"]
    depVar <- model.vars[1]
    indepVars <- model.vars[-1]
     if(model == 'linear'){
      beta.reg <- ds.linear(formula)
     }
     if(model == 'logistic'){
       beta.reg <- ds.logistic(formula, learningrate, dif)
     }
     if(model == 'poisson'){
       beta.reg <- ds.poisson(formula, learningrate, dif)
     }
  }

  variables <- as.list(as.character(attr(terms(model.formula), "variables"))[-1L])

  #Data transformations
  beta.vect.temp <- paste0(as.character(beta.reg$coefficients), collapse="x")

  #Residuals
  media.y <- ds.arMean(variables[[1]])
  #media_y <- beta.reg$sum.y / beta.reg$n.rows
  media.y.temp <- paste0(as.character(media.y))

  cally2 <- call('getAnova', beta.vect.temp, beta.reg$call, media.y.temp)
  result <- opal::datashield.aggregate(datasources, cally2)

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

  for(residual in result) {
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
    cols_x <- cols_x + residual$cols.x
  }

  #Residuals
  residuals_fim <- residuals[-1]
  resisuals_quantis <- quantile(residuals_fim)
  residuals.quantis.fim <- data.matrix(resisuals_quantis)

  ssr <- sst - sse
  s2 <- sum(residuals_fim ^ 2) / (n - 2)

  computed.f <- ssr/s2

  # p-value from f-statistic
  k <- length(indepVars)
  freedon <- n - k - 1
  p.value <- pf(computed.f, 1, freedon, lower.tail=FALSE)

  partial.anova <- data.frame(ssr,ssr,computed.f,p.value)
  anova.table <- cbind(1, partial.anova)
  colnames(anova.table) <- c("Df", "Sum.Sq", "Mean.Sq", "F.value", "Pr(>F)")
  rownames(anova.table) <- as.vector(indepVars)

  table2 <- data.frame(n-2, sse, NA, NA, NA)
  colnames(table2) <- c("Df", "Sum.Sq", "Mean.Sq", "F.value", "Pr(>F)")
  rownames(table2) <- "Residuals"
  anova.table <- rbind(anova.table, table2)

  return(anova.table)

}
