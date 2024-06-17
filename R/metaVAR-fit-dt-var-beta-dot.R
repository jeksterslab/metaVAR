.FitDTVARBeta <- function(p,
                          beta_start = NULL,
                          beta_lbound = NULL,
                          beta_ubound = NULL) {
  idx <- seq_len(p)
  statenames <- paste0("eta", idx)
  # A
  # auto regression and cross regression coefficients
  if (is.null(beta_start)) {
    beta_start <- diag(p)
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
      data = NA,
      nrow = p,
      ncol = p
    )
  } else {
    beta_lbound <- matrix(
      data = beta_lbound,
      nrow = p,
      ncol = p
    )
  }
  if (is.null(beta_ubound)) {
    beta_ubound <- matrix(
      data = NA,
      nrow = p,
      ncol = p
    )
  } else {
    beta_ubound <- matrix(
      data = beta_ubound,
      nrow = p,
      ncol = p
    )
  }
  return(
    OpenMx::mxMatrix(
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
