.MetaMu <- function(p,
                    varnames,
                    mu_start = NULL,
                    mu_lbound = NULL,
                    mu_ubound = NULL) {
  if (is.null(mu_start)) {
    mu_start <- rep(x = 0, times = p)
  }
  if (is.null(mu_lbound)) {
    mu_lbound <- rep(x = NA, times = p)
  } else {
    stopifnot(
      is.vector(mu_lbound),
      length(mu_lbound) == p
    )
  }
  if (is.null(mu_ubound)) {
    mu_ubound <- rep(x = NA, times = p)
  } else {
    stopifnot(
      is.vector(mu_ubound),
      length(mu_ubound) == p
    )
  }
  mu_free <- matrix(
    data = rep(
      x = TRUE,
      times = p
    ),
    nrow = 1
  )
  mu_labels <- matrix(
    data = paste0(
      "mu_",
      seq_len(p)
    ),
    nrow = 1
  )
  return(
    OpenMx::mxMatrix(
      type = "Full",
      nrow = 1,
      ncol = p,
      free = mu_free,
      values = mu_start,
      labels = mu_labels,
      lbound = mu_lbound,
      ubound = mu_ubound,
      byrow = FALSE,
      dimnames = list(
        "m",
        varnames
      ),
      name = "mu"
    )
  )
}
