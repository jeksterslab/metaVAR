% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/metaVAR-meta.R
\name{Meta}
\alias{Meta}
\title{Fit Multivariate Meta-Analysis}
\usage{
Meta(
  y,
  v,
  x = NULL,
  alpha_values = NULL,
  alpha_free = NULL,
  alpha_lbound = NULL,
  alpha_ubound = NULL,
  beta_values = NULL,
  beta_free = NULL,
  beta_lbound = NULL,
  beta_ubound = NULL,
  tau_values = NULL,
  tau_free = NULL,
  tau_lbound = NULL,
  tau_ubound = NULL,
  random = TRUE,
  diag = FALSE,
  try = 1000,
  ncores = NULL,
  ...
)
}
\arguments{
\item{y}{A list.
Each element of the list is a numeric vector
of estimated coefficients.}

\item{v}{A list.
Each element of the list
is a sampling variance-covariance matrix of \code{y}.}

\item{x}{An optional list.
Each element of the list is a numeric vector
of covariates for the mixed-effects model.}

\item{alpha_values}{Numeric vector.
Optional vector of starting values for \code{alpha}.}

\item{alpha_free}{Logical vector.
Optional vector of free (\code{TRUE}) parameters for \code{alpha}.}

\item{alpha_lbound}{Numeric vector.
Optional vector of lower bound values for \code{alpha}.}

\item{alpha_ubound}{Numeric vector.
Optional vector of upper bound values for \code{alpha}.}

\item{beta_values}{Numeric matrix.
Optional matrix of starting values for \code{beta}.}

\item{beta_free}{Logical matrix.
Optional matrix of free (\code{TRUE}) parameters for \code{beta}.}

\item{beta_lbound}{Numeric matrix.
Optional matrix of lower bound values for \code{beta}.}

\item{beta_ubound}{Numeric matrix.
Optional matrix of upper bound values for \code{beta}.}

\item{tau_values}{Numeric matrix.
Optional matrix of starting values for \code{t(chol(tau_sqr))}.}

\item{tau_free}{Numeric matrix.
Optional matrix of free (\code{TRUE}) parameters for \code{t(chol(tau_sqr))}.}

\item{tau_lbound}{Numeric matrix.
Optional matrix of lower bound values for \code{t(chol(tau_sqr))}.}

\item{tau_ubound}{Numeric matrix.
Optional matrix of upper bound values for \code{t(chol(tau_sqr))}.}

\item{random}{Logical.
If \code{random = TRUE},
estimates random effects.
If \code{random = FALSE},
\code{tau_sqr} is a null matrix.}

\item{diag}{Logical.
If \code{diag = TRUE},
\code{tau_sqr} is a diagonal matrix.
If \code{diag = FALSE},
\code{tau_sqr} is a symmetric matrix.}

\item{try}{Positive integer.
Number of extra optimization tries.}

\item{ncores}{Positive integer.
Number of cores to use.}

\item{...}{Additional optional arguments to pass to \code{mxTryHardctsem}.}
}
\description{
This function estimates
fixed-, random-, or mixed-effects meta-analysis parameters
using the estimated coefficients and sampling variance-covariance matrix
from each individual.
}
\references{
Cheung, M. W.-L. (2015).
\emph{Meta-analysis: A structural equation modeling approach}.
Wiley.
\doi{10.1002/9781118957813}

Neale, M. C., Hunter, M. D., Pritikin, J. N.,
Zahery, M., Brick, T. R., Kirkpatrick, R. M., Estabrook, R.,
Bates, T. C., Maes, H. H., & Boker, S. M. (2015).
OpenMx 2.0: Extended structural equation and statistical modeling.
\emph{Psychometrika},
\emph{81}(2), 535–549.
\doi{10.1007/s11336-014-9435-8}
}
\seealso{
Other Meta-Analysis of VAR Functions: 
\code{\link{MetaVARMx}()}
}
\author{
Ivan Jacob Agaloos Pesigan
}
\concept{Meta-Analysis of VAR Functions}
\keyword{meta}
\keyword{metaVAR}
