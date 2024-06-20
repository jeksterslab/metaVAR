.FitDTVARPsi <- function(p,
                         psi_start = NULL,
                         psi_lbound = NULL,
                         psi_ubound = NULL) {
  idx <- seq_len(p)
  statenames <- paste0("eta", idx)
  # Q
  # process noise
  if (is.null(psi_start)) {
    psi_start <- rep(
      x = 0.10,
      times = p
    )
  } else {
    stopifnot(
      is.vector(psi_start),
      length(psi_start) == p
    )
  }
  psi_labels <- paste0(
    "psi_",
    paste0(
      idx,
      idx
    )
  )
  if (is.null(psi_lbound)) {
    psi_lbound <- rep(
      x = .Machine$double.xmin,
      times = p
    )
  } else {
    stopifnot(
      is.vector(psi_lbound),
      length(psi_lbound) == p
    )
  }
  if (is.null(psi_ubound)) {
    psi_ubound <- rep(
      x = NA,
      times = p
    )
  } else {
    stopifnot(
      is.vector(psi_ubound),
      length(psi_ubound) == p
    )
  }
  return(
    mxMatrix(
      type = "Diag",
      nrow = p,
      ncol = p,
      free = TRUE,
      values = psi_start,
      labels = psi_labels,
      lbound = psi_lbound,
      ubound = psi_ubound,
      byrow = FALSE,
      dimnames = list(
        statenames,
        statenames
      ),
      name = "psi"
    )
  )
}
