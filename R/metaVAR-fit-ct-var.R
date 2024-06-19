#' Fit First Order Continuous-Time Vector Autoregressive Model by ID
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
#' @param time Character string.
#'   A character string of the name of the TIME variable in the data.
#' @param phi_start Optional starting values for `phi`.
#' @param phi_lbound Optional lower bound for `phi`.
#' @param phi_ubound Optional upper bound for `phi`.
#' @param sigma_start Optional starting values for `sigma`.
#' @param sigma_lbound Optional lower bound for `sigma`.
#' @param sigma_ubound Optional upper bound for `sigma`.
#' @param try Positive integer.
#'   Number of extra tries for [OpenMx::mxTryHard()].
#' @param ncores Positive integer.
#'   Number of cores to use.
#'
#' @family Meta-Analysis of VAR Functions
#' @keywords metaVAR fit
#' @import OpenMx
#' @export
FitCTVAR <- function(data,
                     observed,
                     id,
                     time,
                     phi_start = NULL,
                     phi_lbound = NULL,
                     phi_ubound = NULL,
                     sigma_start = NULL,
                     sigma_lbound = NULL,
                     sigma_ubound = NULL,
                     try = 1000,
                     ncores = NULL) {
  args <- list(
    data = data,
    observed = observed,
    id = id,
    time = time,
    phi_start = phi_start,
    phi_lbound = phi_lbound,
    phi_ubound = phi_ubound,
    sigma_start = sigma_start,
    sigma_lbound = sigma_lbound,
    sigma_ubound = sigma_ubound,
    try = try,
    ncores = ncores
  )
  output <- .FitCTVAR(
    data = data,
    observed = observed,
    id = id,
    time = time,
    phi_start = phi_start,
    phi_lbound = phi_lbound,
    phi_ubound = phi_ubound,
    sigma_start = sigma_start,
    sigma_lbound = sigma_lbound,
    sigma_ubound = sigma_ubound,
    try = try,
    ncores = ncores
  )
  out <- list(
    call = match.call(),
    args = args,
    fun = "FitCTVAR",
    output = output
  )
  class(out) <- c(
    "metavarctvar",
    class(out)
  )
  return(out)
}
