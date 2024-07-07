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
#' @param object Output of the [fitDTVARMx::FitDTVARIDMx()]
#'   or [fitCTVARMx::FitCTVARIDMx()] functions.
#' @param noise Logical.
#'   If `noise = TRUE`,
#'   include estimates of the process noise matrix, if available.
#'   If `noise = FALSE`,
#'   exclude estimates of the process noise matrix.
#' @param error Logical.
#'   If `error = TRUE`,
#'   include estimates of the measurement error matrix, if available.
#'   If `error = FALSE`,
#'   exclude estimates of the measurement error matrix.
#' @inheritParams Meta
#'
#' @examples
#' \dontrun{
#' # Generate data using the simStateSpace package------------------------------
#' beta_mu <- matrix(
#'   data = c(
#'     0.7, 0.5, -0.1,
#'     0.0, 0.6, 0.4,
#'     0, 0, 0.5
#'   ),
#'   nrow = 3
#' )
#' beta_sigma <- diag(3 * 3)
#' beta <- simStateSpace::SimBetaN(
#'   n = 5,
#'   beta = beta_mu,
#'   vcov_beta_vec_l = t(chol(beta_sigma))
#' )
#' sim <- simStateSpace::SimSSMVARIVary(
#'   n = 5,
#'   time = 100,
#'   mu0 = list(rep(x = 0, times = 3)),
#'   sigma0_l = list(t(chol(diag(3)))),
#'   alpha = list(rep(x = 0, times = 3)),
#'   beta = beta,
#'   psi_l = list(t(chol(diag(3))))
#' )
#' data <- as.data.frame(sim)
#'
#' # Fit the model--------------------------------------------------------------
#' library(fitDTVARMx)
#' fit <- FitDTVARIDMx(
#'   data = data,
#'   observed = c("y1", "y2", "y3"),
#'   id = "id"
#' )
#' # Multivariate meta-analysis-------------------------------------------------
#' library(metaVAR)
#' meta <- MetaVARMx(fit)
#' print(meta)
#' summary(meta)
#' coef(meta)
#' vcov(meta)
#' }
#'
#' @family Meta-Analysis of VAR Functions
#' @keywords metaVAR meta
#' @export
MetaVARMx <- function(object,
                      mu_start = NULL,
                      mu_lbound = NULL,
                      mu_ubound = NULL,
                      sigma_l_start = NULL,
                      sigma_l_lbound = NULL,
                      sigma_l_ubound = NULL,
                      noise = FALSE,
                      error = FALSE,
                      try = 1000,
                      ncores = NULL) {
  stopifnot(
    inherits(
      object,
      "fitdtvaridmx"
    ) || inherits(
      object,
      "fitctvaridmx"
    )
  )
  if (
    inherits(
      object,
      "fitdtvaridmx"
    )
  ) {
    y <- fitDTVARMx:::coef.fitdtvaridmx(
      object = object,
      psi = noise,
      theta = error
    )
    v <- fitDTVARMx:::vcov.fitdtvaridmx(
      object = object,
      psi = noise,
      theta = error
    )
  }
  if (
    inherits(
      object,
      "fitctvaridmx"
    )
  ) {
    y <- fitCTVARMx:::coef.fitctvaridmx(
      object = object,
      sigma = noise,
      theta = error
    )
    v <- fitCTVARMx:::vcov.fitctvaridmx(
      object = object,
      sigma = noise,
      theta = error
    )
  }
  return(
    Meta(
      y = y,
      v = v,
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
