\name{cross_list}
\alias{cross_list}
\title{Cross variables in a list...}
\usage{cross_list(l, funs=c(mean, sd, quantile, n, na), ..., cum=FALSE,
    margin=0:2, useNA=c("no", "ifany", "always"), propNA=TRUE,
    revert=FALSE, method=c("pearson", "kendall", "spearman"))}
\description{Cross variables in a list}
\author{David Hajage}
\keyword{internal}
\arguments{\item{x}{x}
\item{y}{y}
\item{funs}{funs}
\item{...}{\dots}
\item{cum}{cum}
\item{margin}{margin}
\item{useNA}{useNA}
\item{revert}{whether to regroup factors or numeric variables when crossing factor with numeric variables}
\item{method}{method}}
