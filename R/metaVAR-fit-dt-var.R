#' Fit First Order Discrete-Time Vector Autoregressive Model by ID
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @param data Data frame.
#'   A data frame object of data for potentially
#'   multiple subjects that contain
#'   a column of subject ID numbers
#'   (i.e., an ID variable), and
#'   at least one column of observed values.
#' @param observed Character vector.
#'   A vector of character strings
#'   of the names of the observed variables in the data.
#' @param id Character string.
#'   A character string of the name of the ID variable in the data.
#' @param beta_start Optional starting values for `beta`.
#' @param beta_lbound Optional lower bound for `beta`.
#' @param beta_ubound Optional upper bound for `beta`.
#' @param psi_start Optional starting values for `psi`.
#' @param psi_lbound Optional lower bound for `psi`.
#' @param psi_ubound Optional upper bound for `psi`.
#' @param try Positive integer.
#'   Number of extra tries for [OpenMx::mxTryHard()].
#' @param ncores Positive integer.
#'   Number of cores to use.
#'
#' @family Meta-Analysis of VAR Functions
#' @keywords metaVAR fit
#' @import OpenMx
#' @export
FitDTVAR <- function(data,
                     observed,
                     id,
                     beta_start = NULL,
                     beta_lbound = NULL,
                     beta_ubound = NULL,
                     psi_start = NULL,
                     psi_lbound = NULL,
                     psi_ubound = NULL,
                     try = 100,
                     ncores = NULL) {
  args <- list(
    data = data,
    observed = observed,
    id = id,
    beta_start = beta_start,
    beta_lbound = beta_lbound,
    beta_ubound = beta_ubound,
    psi_start = psi_start,
    psi_lbound = psi_lbound,
    psi_ubound = psi_ubound,
    try = try,
    ncores = ncores
  )
  output <- .FitDTVAR(
    data = data,
    observed = observed,
    id = id,
    beta_start = beta_start,
    beta_lbound = beta_lbound,
    beta_ubound = beta_ubound,
    psi_start = psi_start,
    psi_lbound = psi_lbound,
    psi_ubound = psi_ubound,
    try = try,
    ncores = ncores
  )
  out <- list(
    call = match.call(),
    args = args,
    fun = "FitDTVAR",
    output = output
  )
  class(out) <- c(
    "metavardtvar",
    class(out)
  )
  return(out)
}
