% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ds.arMean.R
\name{ds.arMean}
\alias{ds.arMean}
\title{Arithmetic mean.}
\usage{
ds.arMean(x = NULL, datasources = NULL)
}
\arguments{
\item{x}{a character, the name of study variable.}

\item{datasources}{a list of opal object(s) obtained after login in to opal servers;
these objects hold also the data assign to R, as \code{data frame}, from opal datasources.}
}
\value{
return a numeric value.
}
\description{
Computes the arithmetic mean.
}
\details{
The \eqn{\sum{x}} and the sample size is computed is computed for each data node.
Combines these values by the equation \eqn{{\sum{dn}}/{l}}, where \eqn{\sum{dn}} is the sum of the sums of nodes,
and \emph{l} is the global sample size.
Requires the functions \code{\link{getSum}} and \code{\link{getLength}} from server side.
}
\section{Dependencies}{

\code{\link{getSum}}, \code{\link{getLength}}
}

\examples{
{
ds.arMean('D$birth_weight')
}
}
\author{
Paula Raissa Costa e Silva
}
