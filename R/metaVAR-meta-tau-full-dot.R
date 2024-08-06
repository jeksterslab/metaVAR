.MetaTauFull <- function(p,
                         tau_values,
                         tau_free,
                         tau_lbound,
                         tau_ubound) {
  idx <- seq_len(p)
  tau_labels <- outer(
    X = idx,
    Y = idx,
    FUN = function(x, y) {
      paste0(
        "t_",
        x,
        "_",
        y
      )
    }
  )
  if (is.null(tau_values)) {
    tau_values <- diag(p)
  } else {
    stopifnot(
      is.matrix(tau_values),
      dim(tau_values) == c(p, p)
    )
  }
  if (is.null(tau_lbound)) {
    tau_lbound <- matrix(
      data = NA,
      nrow = p,
      ncol = p
    )
    diag(tau_lbound) <- .Machine$double.xmin
  } else {
    stopifnot(
      is.matrix(tau_lbound),
      dim(tau_lbound) == c(p, p)
    )
  }
  if (is.null(tau_ubound)) {
    tau_ubound <- matrix(
      data = NA,
      nrow = p,
      ncol = p
    )
  } else {
    stopifnot(
      is.matrix(tau_ubound),
      dim(tau_ubound) == c(p, p)
    )
  }
  if (is.null(tau_free)) {
    tau_free <- matrix(
      data = TRUE,
      nrow = p,
      ncol = p
    )
  }
  idx <- seq_len(p)
  for (i in idx) {
    for (j in idx) {
      if (!tau_free[i, j]) {
        tau_labels[i, j] <- NA
        tau_lbound[i, j] <- NA
        tau_ubound[i, j] <- NA
      }
    }
  }
  # make sure that matrices are lower triangular
  tau_free[upper.tri(tau_free)] <- FALSE
  tau_labels[upper.tri(tau_labels)] <- NA
  tau_values[upper.tri(tau_values)] <- 0
  tau_lbound[upper.tri(tau_lbound)] <- NA
  tau_ubound[upper.tri(tau_ubound)] <- NA
  return(
    OpenMx::mxMatrix(
      type = "Lower",
      nrow = p,
      ncol = p,
      free = tau_free,
      values = tau_values,
      labels = tau_labels,
      lbound = tau_lbound,
      ubound = tau_ubound,
      byrow = FALSE,
      name = "tau"
    )
  )
}
