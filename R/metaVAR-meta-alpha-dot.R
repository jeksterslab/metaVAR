.MetaAlpha <- function(p,
                       ynames,
                       alpha_values,
                       alpha_free,
                       alpha_lbound,
                       alpha_ubound) {
  if (is.null(alpha_values)) {
    alpha_values <- matrix(
      data = 0,
      nrow = p
    )
  } else {
    if (is.vector(alpha_values)) {
      alpha_values <- matrix(
        data = alpha_values,
        ncol = 1
      )
    }
    stopifnot(
      is.matrix(alpha_values),
      dim(alpha_values) == c(p, 1)
    )
  }
  if (is.null(alpha_lbound)) {
    alpha_lbound <- matrix(
      data = NA,
      nrow = p
    )
  } else {
    if (is.vector(alpha_lbound)) {
      alpha_lbound <- matrix(
        data = alpha_lbound,
        ncol = 1
      )
    }
    stopifnot(
      is.matrix(alpha_lbound),
      dim(alpha_lbound) == c(p, 1)
    )
  }
  if (is.null(alpha_ubound)) {
    alpha_ubound <- matrix(
      data = NA,
      nrow = p
    )
  } else {
    if (is.vector(alpha_ubound)) {
      alpha_ubound <- matrix(
        data = alpha_ubound,
        ncol = 1
      )
    }
    stopifnot(
      is.matrix(alpha_ubound),
      dim(alpha_ubound) == c(p, 1)
    )
  }
  alpha_labels <- matrix(
    data = paste0(
      "b0_",
      seq_len(p)
    ),
    ncol = 1
  )
  if (is.null(alpha_free)) {
    alpha_free <- matrix(
      data = rep(
        x = TRUE,
        times = p
      ),
      ncol = 1
    )
  } else {
    if (is.vector(alpha_free)) {
      alpha_free <- matrix(
        data = alpha_free,
        ncol = 1
      )
    }
    stopifnot(
      is.matrix(alpha_free),
      dim(alpha_free) == c(p, 1)
    )
    idx <- seq_len(p)
    for (i in idx) {
      if (!alpha_free[i, 1]) {
        alpha_labels[i, 1] <- NA
        alpha_lbound[i, 1] <- NA
        alpha_ubound[i, 1] <- NA
      }
    }
  }
  OpenMx::mxMatrix(
    type = "Full",
    nrow = p,
    ncol = 1,
    free = alpha_free,
    values = alpha_values,
    labels = alpha_labels,
    lbound = alpha_lbound,
    ubound = alpha_ubound,
    byrow = FALSE,
    dimnames = list(
      ynames,
      1
    ),
    name = "alpha"
  )
}
