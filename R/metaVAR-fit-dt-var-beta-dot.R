.FitDTVARBeta <- function(p,
                          statenames,
                          beta_start = NULL,
                          beta_lbound = NULL,
                          beta_ubound = NULL) {
  idx <- seq_len(p)
  # A
  # auto regression and cross regression coefficients
  if (is.null(beta_start)) {
    beta_start <- 0.10 * diag(p)
  } else {
    stopifnot(
      is.matrix(beta_start),
      dim(beta_start) == c(p, p)
    )
  }
  beta_labels <- matrix(
    data = "0",
    nrow = p,
    ncol = p
  )
  for (i in idx) {
    for (j in idx) {
      beta_labels[i, j] <- paste0(
        "beta",
        "_",
        idx[i],
        idx[j]
      )
    }
  }
  if (is.null(beta_lbound)) {
    beta_lbound <- matrix(
      data = -10,
      nrow = p,
      ncol = p
    )
  } else {
    stopifnot(
      is.matrix(beta_lbound),
      dim(beta_lbound) == c(p, p)
    )
  }
  if (is.null(beta_ubound)) {
    beta_ubound <- matrix(
      data = 10,
      nrow = p,
      ncol = p
    )
  } else {
    stopifnot(
      is.matrix(beta_ubound),
      dim(beta_ubound) == c(p, p)
    )
  }
  return(
    mxMatrix(
      type = "Full",
      nrow = p,
      ncol = p,
      free = TRUE,
      values = beta_start,
      labels = beta_labels,
      lbound = beta_lbound,
      ubound = beta_ubound,
      byrow = FALSE,
      dimnames = list(
        statenames,
        statenames
      ),
      name = "beta"
    )
  )
}
