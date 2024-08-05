#' Fit Multivariate Meta-Analysis
#'
#' This function estimates
#' fixed-, random-, or mixed-effects meta-analysis parameters
#' using the estimated coefficients and sampling variance-covariance matrix
#' from each individual.
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @param object Output of the [fitDTVARMx::FitDTVARIDMx()]
#'   or [fitCTVARMx::FitCTVARIDMx()] functions.
#' @param intercept Logical.
#'   If `intercept = TRUE`,
#'   include estimates of the process intercept vector, if available.
#'   If `intercept = FALSE`,
#'   exclude estimates of the process intercept vector.
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
#' @references
#' Cheung, M. W.-L. (2015).
#' *Meta-analysis: A structural equation modeling approach*.
#' Chichester, West Sussex: John Wiley & Sons, Inc.
#'
#' Neale, M. C., Hunter, M. D., Pritikin, J. N.,
#' Zahery, M., Brick, T. R., Kirkpatrick, R. M., Estabrook, R.,
#' Bates, T. C., Maes, H. H., & Boker, S. M. (2015).
#' OpenMx 2.0: Extended structural equation and statistical modeling.
#' *Psychometrika*,
#' *81*(2), 535–549.
#' \doi{10.1007/s11336-014-9435-8}
#'
#' @family Meta-Analysis of VAR Functions
#' @keywords metaVAR meta
#' @export
MetaVARMx <- function(object,
                      x = NULL,
                      beta0_values = NULL,
                      beta0_free = NULL,
                      beta0_lbound = NULL,
                      beta0_ubound = NULL,
                      beta1_values = NULL,
                      beta1_free = NULL,
                      beta1_lbound = NULL,
                      beta1_ubound = NULL,
                      tau_values = NULL,
                      tau_free = NULL,
                      tau_lbound = NULL,
                      tau_ubound = NULL,
                      random = TRUE,
                      diag = FALSE,
                      intercept = FALSE,
                      noise = FALSE,
                      error = FALSE,
                      try = 1000,
                      ncores = NULL,
                      ...) {
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
      alpha = intercept,
      psi = noise,
      theta = error
    )
    v <- fitDTVARMx:::vcov.fitdtvaridmx(
      object = object,
      alpha = intercept,
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
      iota = intercept,
      sigma = noise,
      theta = error
    )
    v <- fitCTVARMx:::vcov.fitctvaridmx(
      object = object,
      iota = intercept,
      sigma = noise,
      theta = error
    )
  }
  return(
    Meta(
      y = y,
      v = v,
      x = x,
      beta0_values = beta0_values,
      beta0_free = beta0_free,
      beta0_lbound = beta0_lbound,
      beta0_ubound = beta0_ubound,
      beta1_values = beta1_values,
      beta1_free = beta1_free,
      beta1_lbound = beta1_lbound,
      beta1_ubound = beta1_ubound,
      tau_values = tau_values,
      tau_free = tau_free,
      tau_lbound = tau_lbound,
      tau_ubound = tau_ubound,
      random = random,
      diag = diag,
      try = try,
      ncores = ncores,
      ...
    )
  )
}
