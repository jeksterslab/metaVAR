#' Fit Multivariate Meta-Analysis
#'
#' This function estimates
#' fixed-, random-, or mixed-effects meta-analysis parameters
#' using the estimated coefficients and sampling variance-covariance matrix
#' from each individual.
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @param y A list.
#'   Each element of the list is a numeric vector
#'   of estimated coefficients.
#' @param v A list.
#'   Each element of the list
#'   is a sampling variance-covariance matrix of `y`.
#' @param x An optional list.
#'   Each element of the list is a numeric vector
#'   of covariates for the mixed-effects model.
#' @param alpha_values Numeric vector.
#'   Optional vector of starting values for `alpha`.
#' @param alpha_free Logical vector.
#'   Optional vector of free (`TRUE`) parameters for `alpha`.
#' @param alpha_lbound Numeric vector.
#'   Optional vector of lower bound values for `alpha`.
#' @param alpha_ubound Numeric vector.
#'   Optional vector of upper bound values for `alpha`.
#' @param beta_values Numeric matrix.
#'   Optional matrix of starting values for `beta`.
#' @param beta_free Logical matrix.
#'   Optional matrix of free (`TRUE`) parameters for `beta`.
#' @param beta_lbound Numeric matrix.
#'   Optional matrix of lower bound values for `beta`.
#' @param beta_ubound Numeric matrix.
#'   Optional matrix of upper bound values for `beta`.
#' @param tau_values Numeric matrix.
#'   Optional matrix of starting values for `t(chol(tau_sqr))`.
#' @param tau_free Numeric matrix.
#'   Optional matrix of free (`TRUE`) parameters for `t(chol(tau_sqr))`.
#' @param tau_lbound Numeric matrix.
#'   Optional matrix of lower bound values for `t(chol(tau_sqr))`.
#' @param tau_ubound Numeric matrix.
#'   Optional matrix of upper bound values for `t(chol(tau_sqr))`.
#' @param random Logical.
#'   If `random = TRUE`,
#'   estimates random effects.
#'   If `random = FALSE`,
#'   `tau_sqr` is a null matrix.
#' @param diag Logical.
#'   If `diag = TRUE`,
#'   `tau_sqr` is a diagonal matrix.
#'   If `diag = FALSE`,
#'   `tau_sqr` is a symmetric matrix.
#' @param try Positive integer.
#'   Number of extra optimization tries.
#' @param ncores Positive integer.
#'   Number of cores to use.
#' @param ... Additional optional arguments to pass to `mxTryHardctsem`.
#'
#' @references
#' Cheung, M. W.-L. (2015).
#' *Meta-analysis: A structural equation modeling approach*.
#' Wiley.
#' \doi{10.1002/9781118957813}
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
#' @import OpenMx
#' @importFrom stats coef vcov
#' @export
Meta <- function(y,
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
                 ...) {
  p <- length(y[[1]])
  n <- length(y)
  stopifnot(
    length(v) == length(y)
  )
  if (is.null(x)) {
    m <- NULL
  } else {
    m <- length(x[[1]])
    stopifnot(
      length(x) == length(y)
    )
  }
  args <- list(
    y = y,
    v = v,
    x = x,
    p = p,
    m = m,
    n = n,
    alpha_values = alpha_values,
    alpha_free = alpha_free,
    alpha_lbound = alpha_lbound,
    alpha_ubound = alpha_ubound,
    beta_values = beta_values,
    beta_free = beta_free,
    beta_lbound = beta_lbound,
    beta_ubound = beta_ubound,
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
  output <- .MetaGeneric(
    y = y,
    v = v,
    x = x,
    p = p,
    m = m,
    n = n,
    alpha_values = alpha_values,
    alpha_free = alpha_free,
    alpha_lbound = alpha_lbound,
    alpha_ubound = alpha_ubound,
    beta_values = beta_values,
    beta_free = beta_free,
    beta_lbound = beta_lbound,
    beta_ubound = beta_ubound,
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
  out <- list(
    call = match.call(),
    args = args,
    fun = "Meta",
    output = output
  )
  class(out) <- c(
    "metavarmeta",
    class(out)
  )
  out
}
