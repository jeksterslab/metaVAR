.MetaBeta1 <- function(p,
                       m,
                       ynames,
                       xnames,
                       beta1_values,
                       beta1_free,
                       beta1_lbound,
                       beta1_ubound) {
  if (is.null(m)) {
    return(
      OpenMx::mxMatrix(
        type = "Zero",
        nrow = p,
        ncol = 1,
        name = "beta1"
      )
    )
  } else {
    if (is.null(beta1_values)) {
      beta1_values <- matrix(
        data = 0,
        nrow = p,
        ncol = m
      )
    }
    if (is.null(beta1_lbound)) {
      beta1_lbound <- matrix(
        data = NA,
        nrow = p,
        ncol = m
      )
    } else {
      stopifnot(
        is.matrix(beta1_lbound),
        dim(beta1_lbound) == c(p, m)
      )
    }
    if (is.null(beta1_ubound)) {
      beta1_ubound <- matrix(
        data = NA,
        nrow = p,
        ncol = m
      )
    } else {
      stopifnot(
        is.matrix(beta1_ubound),
        dim(beta1_ubound) == c(p, m)
      )
    }
    beta1_labels <- outer(
      X = seq_len(p),
      Y = seq_len(m),
      FUN = function(x, y) {
        paste0(
          "beta1_",
          x,
          y
        )
      }
    )
    if (is.null(beta1_free)) {
      beta1_free <- matrix(
        data = TRUE,
        nrow = p,
        ncol = m
      )
    }
    for (i in seq_len(p)) {
      for (j in seq_len(m)) {
        if (!beta1_free[i, j]) {
          beta1_labels[i, j] <- NA
          beta1_lbound[i, j] <- NA
          beta1_ubound[i, j] <- NA
        }
      }
    }
    return(
      OpenMx::mxMatrix(
        type = "Full",
        nrow = p,
        ncol = m,
        free = beta1_free,
        values = beta1_values,
        labels = beta1_labels,
        lbound = beta1_lbound,
        ubound = beta1_ubound,
        byrow = FALSE,
        dimnames = list(
          ynames,
          xnames
        ),
        name = "beta1"
      )
    )
  }
}
