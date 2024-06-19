.FitCTVARPhi <- function(p,
                         statenames,
                         phi_start = NULL,
                         phi_lbound = NULL,
                         phi_ubound = NULL) {
  idx <- seq_len(p)
  # A
  # auto effect and cross effect coefficients
  if (is.null(phi_start)) {
    phi_start <- -1 * diag(p)
  }
  phi_labels <- matrix(
    data = "0",
    nrow = p,
    ncol = p
  )
  for (i in idx) {
    for (j in idx) {
      phi_labels[i, j] <- paste0(
        "phi",
        "_",
        idx[i],
        idx[j]
      )
    }
  }
  if (is.null(phi_lbound)) {
    phi_lbound <- matrix(
      data = NA,
      nrow = p,
      ncol = p
    )
  }
  if (is.null(phi_ubound)) {
    phi_ubound <- matrix(
      data = NA,
      nrow = p,
      ncol = p
    )
    diag(phi_ubound) <- .Machine$double.xmin
  }
  return(
    OpenMx::mxMatrix(
      type = "Full",
      nrow = p,
      ncol = p,
      free = TRUE,
      values = phi_start,
      labels = phi_labels,
      lbound = phi_lbound,
      ubound = phi_ubound,
      byrow = FALSE,
      dimnames = list(
        statenames,
        statenames
      ),
      name = "phi"
    )
  )
}
