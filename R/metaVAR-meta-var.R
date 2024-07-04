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
#' @param object Output of the [fitDTVARMx::FitDTVARIDMx()] function.
#' @param psi Logical.
#'   If `psi = TRUE`,
#'   include estimates of the `psi` matrix.
#'   If `psi = FALSE`,
#'   exclude estimates of the `psi` matrix.
#' @param theta Logical.
#'   If `theta = TRUE`,
#'   include estimates of the `theta` matrix if available.
#'   If `theta = FALSE`,
#'   exclude estimates of the `theta` matrix.
#' @inheritParams Meta
#' @family Meta-Analysis of VAR Functions
#' @keywords metaVAR meta
#' @export
MetaVAR <- function(object,
                    mu_start = NULL,
                    mu_lbound = NULL,
                    mu_ubound = NULL,
                    sigma_l_start = NULL,
                    sigma_l_lbound = NULL,
                    sigma_l_ubound = NULL,
                    psi = FALSE,
                    theta = FALSE,
                    try = 1000,
                    ncores = NULL) {
  return(
    Meta(
      y = fitDTVARMx:::coef.fitdtvaridmx(
        object = object,
        psi = psi,
        theta = theta
      ),
      v = fitDTVARMx:::vcov.fitdtvaridmx(
        object = object,
        psi = psi,
        theta = theta
      ),
      mu_start = mu_start,
      mu_lbound = mu_lbound,
      mu_ubound = mu_ubound,
      sigma_l_start = sigma_l_start,
      sigma_l_lbound = sigma_l_lbound,
      sigma_l_ubound = sigma_l_ubound,
      try = try,
      ncores = ncores
    )
  )
}
