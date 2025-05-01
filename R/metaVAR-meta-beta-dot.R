.MetaBeta <- function(p,
                      m,
                      ynames,
                      xnames,
                      beta_values,
                      beta_free,
                      beta_lbound,
                      beta_ubound) {
  if (is.null(m)) {
    out <- OpenMx::mxMatrix(
      type = "Zero",
      nrow = p,
      ncol = 1,
      name = "beta"
    )
  } else {
    if (is.null(beta_values)) {
      beta_values <- matrix(
        data = 0,
        nrow = p,
        ncol = m
      )
    }
    if (is.null(beta_lbound)) {
      beta_lbound <- matrix(
        data = NA,
        nrow = p,
        ncol = m
      )
    } else {
      stopifnot(
        is.matrix(beta_lbound),
        dim(beta_lbound) == c(p, m)
      )
    }
    if (is.null(beta_ubound)) {
      beta_ubound <- matrix(
        data = NA,
        nrow = p,
        ncol = m
      )
    } else {
      stopifnot(
        is.matrix(beta_ubound),
        dim(beta_ubound) == c(p, m)
      )
    }
    beta_labels <- outer(
      X = seq_len(p),
      Y = seq_len(m),
      FUN = function(x, y) {
        paste0(
          "b1_",
          x,
          y
        )
      }
    )
    if (is.null(beta_free)) {
      beta_free <- matrix(
        data = TRUE,
        nrow = p,
        ncol = m
      )
    } else {
      for (i in seq_len(p)) {
        for (j in seq_len(m)) {
          if (!beta_free[i, j]) {
            beta_labels[i, j] <- NA
            beta_lbound[i, j] <- NA
            beta_ubound[i, j] <- NA
          }
        }
      }
    }
    out <- OpenMx::mxMatrix(
      type = "Full",
      nrow = p,
      ncol = m,
      free = beta_free,
      values = beta_values,
      labels = beta_labels,
      lbound = beta_lbound,
      ubound = beta_ubound,
      byrow = FALSE,
      dimnames = list(
        ynames,
        xnames
      ),
      name = "beta"
    )
  }
  out
}
