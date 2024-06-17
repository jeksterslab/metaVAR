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
      x = 1,
      times = p
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
    psi_lbound <- rep(
      x = psi_lbound,
      times = p
    )
  }
  if (is.null(psi_ubound)) {
    psi_ubound <- rep(
      x = NA,
      times = p
    )
  } else {
    psi_ubound <- rep(
      x = psi_ubound,
      times = p
    )
  }
  return(
    OpenMx::mxMatrix(
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
