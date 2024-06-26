#' Fit Multivariate Meta-Analysis
#'
#' This function estimates the mean and covariance matrix
#' of a vector of coefficients
#' using the estimated coefficients and sampling variance-covariance matrix
#' from each individual.
#'
#' @details For \eqn{i = \left\{1, \cdots, n \right\}},
#'   the objective function used to estimate the mean \eqn{\boldsymbol{\mu}}
#'   and covariance matrix \eqn{\boldsymbol{\Sigma}}
#'   of the random coefficients \eqn{\mathbf{y}_{i}} is given by
#'   \deqn{
#'     \ell
#'     \left(
#'     \boldsymbol{\mu} ,
#'     \boldsymbol{\Sigma} \mid \mathbf{y}_{i},
#'     \mathbb{V} \left( \mathbf{y}_{i} \right)
#'     \right)
#'     =
#'     - \frac{1}{2}
#'     \left[
#'     q \log \left( 2 \pi \right)
#'     +
#'     \log
#'     \left(
#'     \left|
#'       \mathbb{V} \left( \mathbf{y}_{i} \right) - \boldsymbol{\Sigma}
#'     \right|
#'     \right)
#'     +
#'     \left( \mathbf{y}_{i} - \boldsymbol{\mu} \right)^{\prime}
#'     \left(
#'       \mathbb{V} \left( \mathbf{y}_{i} \right) - \boldsymbol{\Sigma}
#'     \right)^{-1}
#'     \left( \mathbf{y}_{i} - \boldsymbol{\mu} \right)
#'     \right]
#'   }
#'  where
#'  \eqn{q} is the number of unique elements
#'  in \eqn{\boldsymbol{\mu}} and \eqn{\boldsymbol{\Sigma}}, and
#'  \eqn{\mathbb{V} \left( \mathbf{y}_{i} \right)}
#'  is the sampling variance-covariance matrix of \eqn{\mathbf{y}_{i}}.
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @param y Object of class `metavardtvar`.
#' @param mu_start Numeric matrix.
#'   Matrix of starting values of `mu`.
#' @param sigma_l_start Numeric matrix.
#'   Matrix of starting values of `t(chol(sigma))`.
#' @param try Positive integer.
#'   Number of extra tries for [OpenMx::mxTryHard()].
#' @param ncores Positive integer.
#'   Number of cores to use.
#' @param ... Additional arguments.
#'
#' @family Meta-Analysis of VAR Functions
#' @keywords metaVAR meta
#' @importFrom stats coef vcov
#' @importFrom Matrix nearPD
#' @export
Meta.metavardtvar <- function(y,
                              mu_start = NULL,
                              sigma_l_start = NULL,
                              try = 1000,
                              ncores = NULL,
                              ...) {
  return(
    .MetaMx(
      object = y,
      mu_start = mu_start,
      sigma_l_start = sigma_l_start,
      try = try,
      ncores = ncores
    )
  )
}
