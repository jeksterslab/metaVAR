.FitCTVARSigma <- function(p,
                           sigma_start = NULL,
                           sigma_lbound = NULL,
                           sigma_ubound = NULL) {
  idx <- seq_len(p)
  statenames <- paste0("eta", idx)
  # Q
  # process noise
  if (is.null(sigma_start)) {
    sigma_start <- rep(
      x = 0.10,
      times = p
    )
  } else {
    stopifnot(
      is.vector(sigma_start),
      length(sigma_start) == p
    )
  }
  sigma_labels <- paste0(
    "sigma_",
    paste0(
      idx,
      idx
    )
  )
  if (is.null(sigma_lbound)) {
    sigma_lbound <- rep(
      x = .Machine$double.xmin,
      times = p
    )
  } else {
    stopifnot(
      is.vector(sigma_lbound),
      length(sigma_lbound) == p
    )
  }
  if (is.null(sigma_ubound)) {
    sigma_ubound <- rep(
      x = NA,
      times = p
    )
  } else {
    stopifnot(
      is.vector(sigma_ubound),
      length(sigma_ubound) == p
    )
  }
  return(
    OpenMx::mxMatrix(
      type = "Diag",
      nrow = p,
      ncol = p,
      free = TRUE,
      values = sigma_start,
      labels = sigma_labels,
      lbound = sigma_lbound,
      ubound = sigma_ubound,
      byrow = FALSE,
      dimnames = list(
        statenames,
        statenames
      ),
      name = "sigma"
    )
  )
}
