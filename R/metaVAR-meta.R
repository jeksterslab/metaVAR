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
#' @param object Object of class `metavardtvar`,
#'   that is, the output of [FitDTVAR()].
#' @param mu_start Numeric matrix.
#'   Matrix of starting values of `mu`.
#' @param sigma_l_start Numeric matrix.
#'   Matrix of starting values of `t(chol(sigma))`.
#' @param try Positive integer.
#'   Number of extra tries for [OpenMx::mxTryHard()].
#' @param ncores Positive integer.
#'   Number of cores to use.
#'
#' @return Returns an object
#'   of class `metavarmeta` which is a list with the following elements:
#'   \describe{
#'     \item{call}{Function call.}
#'     \item{args}{Function arguments.}
#'     \item{fun}{Function used ("Meta").}
#'     \item{output}{Fitted model.}
#'     \item{transform}{Transformed estimates.}
#'   }
#'
#' @family Meta-Analysis of VAR Functions
#' @keywords metaVAR meta
#' @importFrom stats coef vcov
#' @importFrom Matrix nearPD
#' @import OpenMx
#' @export
Meta <- function(object,
                 mu_start = NULL,
                 sigma_l_start = NULL,
                 try = 1000,
                 ncores = NULL) {
  stopifnot(
    inherits(
      object,
      "metavardtvar"
    ) | inherits(
      object,
      "metavarctvar"
    )
  )
  args <- list(
    object = object,
    mu_start = mu_start,
    sigma_l_start = sigma_l_start,
    try = try,
    ncores = ncores
  )
  y <- lapply(
    X = object$output,
    FUN = function(x) {
      coefs <- coef(x)
      idx <- grep(
        pattern = "^beta_|^phi_",
        x = names(coefs)
      )
      return(
        coefs[idx]
      )
    }
  )
  vcov_y <- lapply(
    X = object$output,
    FUN = function(x) {
      vcovs <- vcov(x)
      idx <- grep(
        pattern = "^beta_|^phi_",
        x = colnames(vcovs)
      )
      return(vcovs[idx, idx])
    }
  )
  output <- .Meta(
    y = y,
    vcov_y = vcov_y,
    n = length(y),
    p = length(y[[1]]),
    mu_start = mu_start,
    sigma_l_start = sigma_l_start,
    try = try,
    ncores = ncores
  )
  out <- list(
    call = match.call(),
    args = args,
    fun = "Meta",
    output = output,
    transform = .Transform(
      coef = coef(output),
      vcov = vcov(output),
      p = length(y[[1]])
    )
  )
  class(out) <- c(
    "metavarmeta",
    class(out)
  )
  return(out)
}
