.MetaSigmaDiag <- function(p,
                           varnames,
                           sigma_l_start = NULL,
                           sigma_l_lbound = NULL,
                           sigma_l_ubound = NULL) {
  idx <- seq_len(p)
  if (is.null(sigma_l_start)) {
    sigma_l_start <- rep(
      x = 1,
      times = p
    )
  } else {
    if (is.matrix(sigma_l_start)) {
      sigma_l_start <- diag(sigma_l_start)
    }
    stopifnot(
      is.vector(sigma_l_start),
      length(sigma_l_start) == p
    )
  }
  sigma_l_labels <- paste0(
    "sigma_l_",
    paste0(
      idx,
      idx
    )
  )
  if (is.null(sigma_l_lbound)) {
    sigma_l_lbound <- rep(
      x = .Machine$double.xmin,
      times = p
    )
  } else {
    if (is.matrix(sigma_l_lbound)) {
      sigma_l_lbound <- diag(sigma_l_lbound)
    }
    stopifnot(
      is.vector(sigma_l_lbound),
      length(sigma_l_lbound) == p
    )
  }
  if (is.null(sigma_l_ubound)) {
    sigma_l_ubound <- rep(
      x = NA,
      times = p
    )
  } else {
    if (is.matrix(sigma_l_ubound)) {
      sigma_l_ubound <- diag(sigma_l_ubound)
    }
    stopifnot(
      is.vector(sigma_l_ubound),
      length(sigma_l_ubound) == p
    )
  }
  sigma_l <- OpenMx::mxMatrix(
    type = "Diag",
    nrow = p,
    ncol = p,
    free = TRUE,
    values = sigma_l_start,
    labels = sigma_l_labels,
    lbound = sigma_l_lbound,
    ubound = sigma_l_ubound,
    byrow = FALSE,
    dimnames = list(
      varnames,
      varnames
    ),
    name = "sigma_l"
  )
  sigma <- OpenMx::mxAlgebra(
    expression = sigma_l %*% t(sigma_l),
    dimnames = list(
      varnames,
      varnames
    ),
    name = "sigma"
  )
  return(
    list(
      sigma_l = sigma_l,
      sigma = sigma
    )
  )
}
