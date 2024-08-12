.MetaBeta0 <- function(p,
                       ynames,
                       beta0_values,
                       beta0_free,
                       beta0_lbound,
                       beta0_ubound) {
  if (is.null(beta0_values)) {
    beta0_values <- matrix(
      data = 0,
      nrow = p
    )
  } else {
    if (is.vector(beta0_values)) {
      beta0_values <- matrix(
        data = beta0_values,
        ncol = 1
      )
    }
    stopifnot(
      is.matrix(beta0_values),
      dim(beta0_values) == c(p, 1)
    )
  }
  if (is.null(beta0_lbound)) {
    beta0_lbound <- matrix(
      data = NA,
      nrow = p
    )
  } else {
    if (is.vector(beta0_lbound)) {
      beta0_lbound <- matrix(
        data = beta0_lbound,
        ncol = 1
      )
    }
    stopifnot(
      is.matrix(beta0_lbound),
      dim(beta0_lbound) == c(p, 1)
    )
  }
  if (is.null(beta0_ubound)) {
    beta0_ubound <- matrix(
      data = NA,
      nrow = p
    )
  } else {
    if (is.vector(beta0_ubound)) {
      beta0_ubound <- matrix(
        data = beta0_ubound,
        ncol = 1
      )
    }
    stopifnot(
      is.matrix(beta0_ubound),
      dim(beta0_ubound) == c(p, 1)
    )
  }
  beta0_labels <- matrix(
    data = paste0(
      "b0_",
      seq_len(p)
    ),
    ncol = 1
  )
  if (is.null(beta0_free)) {
    beta0_free <- matrix(
      data = rep(
        x = TRUE,
        times = p
      ),
      ncol = 1
    )
  } else {
    if (is.vector(beta0_free)) {
      beta0_free <- matrix(
        data = beta0_free,
        ncol = 1
      )
    }
    stopifnot(
      is.matrix(beta0_free),
      dim(beta0_free) == c(p, 1)
    )
  }
  idx <- seq_len(p)
  for (i in idx) {
    if (!beta0_free[i, 1]) {
      beta0_labels[i, 1] <- NA
      beta0_lbound[i, 1] <- NA
      beta0_ubound[i, 1] <- NA
    }
  }
  return(
    OpenMx::mxMatrix(
      type = "Full",
      nrow = p,
      ncol = 1,
      free = beta0_free,
      values = beta0_values,
      labels = beta0_labels,
      lbound = beta0_lbound,
      ubound = beta0_ubound,
      byrow = FALSE,
      dimnames = list(
        ynames,
        1
      ),
      name = "beta0"
    )
  )
}
