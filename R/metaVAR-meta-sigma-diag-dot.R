.MetaSigmaDiag <- function(p,
                           varnames,
                           sigma_l_start = NULL,
                           sigma_l_lbound = NULL,
                           sigma_l_ubound = NULL) {
  sigma_l_labels <- matrix(
    data = NA,
    nrow = p,
    ncol = p
  )
  diag(sigma_l_labels) <- paste0(
    "sigma_l_",
    seq_len(p),
    seq_len(p)
  )
  if (is.null(sigma_l_start)) {
    sigma_l_start <- diag(p)
  }
  if (is.null(sigma_l_lbound)) {
    sigma_l_lbound <- rep(
      x = .Machine$double.xmin,
      times = p
    )
  } else {
    if (is.matrix(sigma_l_lbound)) {
      sigma_l_lbound <- diag(sigma_l_lbound)
    }
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
  }
  sigma_l_free <- rep(
    x = TRUE,
    times = p
  )
  sigma_l <- OpenMx::mxMatrix(
    type = "Diag",
    nrow = p,
    ncol = p,
    free = sigma_l_free,
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
