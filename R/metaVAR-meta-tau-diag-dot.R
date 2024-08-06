# no tau_free as all elements of the diagonal matrix are estimated
.MetaTauDiag <- function(p,
                         tau_values,
                         tau_lbound,
                         tau_ubound) {
  idx <- seq_len(p)
  if (is.null(tau_values)) {
    tau_values <- rep(
      x = 1,
      times = p
    )
  } else {
    if (is.matrix(tau_values)) {
      tau_values <- diag(tau_values)
    }
    stopifnot(
      is.vector(tau_values),
      length(tau_values) == p
    )
  }
  tau_labels <- paste0(
    "t_",
    paste0(
      idx,
      "_",
      idx
    )
  )
  if (is.null(tau_lbound)) {
    tau_lbound <- rep(
      x = .Machine$double.xmin,
      times = p
    )
  } else {
    if (is.matrix(tau_lbound)) {
      tau_lbound <- diag(tau_lbound)
    }
    stopifnot(
      is.vector(tau_lbound),
      length(tau_lbound) == p
    )
  }
  if (is.null(tau_ubound)) {
    tau_ubound <- rep(
      x = NA,
      times = p
    )
  } else {
    if (is.matrix(tau_ubound)) {
      tau_ubound <- diag(tau_ubound)
    }
    stopifnot(
      is.vector(tau_ubound),
      length(tau_ubound) == p
    )
  }
  return(
    OpenMx::mxMatrix(
      type = "Diag",
      nrow = p,
      ncol = p,
      free = TRUE,
      values = tau_values,
      labels = tau_labels,
      lbound = tau_lbound,
      ubound = tau_ubound,
      byrow = FALSE,
      name = "tau"
    )
  )
}
