.MetaSigmaFull <- function(p,
                           varnames,
                           sigma_l_start = NULL,
                           sigma_l_lbound = NULL,
                           sigma_l_ubound = NULL) {
  idx <- seq_len(p)
  sigma_l_labels <- matrix(
    data = NA,
    nrow = p,
    ncol = p
  )
  for (j in idx) {
    for (i in idx) {
      sigma_l_labels[i, j] <- paste0(
        "sigma_l_",
        i,
        j
      )
    }
  }
  if (is.null(sigma_l_start)) {
    sigma_l_start <- diag(p)
  } else {
    stopifnot(
      is.matrix(sigma_l_start),
      dim(sigma_l_start) == c(p, p)
    )
  }
  if (is.null(sigma_l_lbound)) {
    sigma_l_lbound <- matrix(
      data = NA,
      nrow = p,
      ncol = p
    )
    diag(sigma_l_lbound) <- .Machine$double.xmin
  } else {
    stopifnot(
      is.matrix(sigma_l_lbound),
      dim(sigma_l_lbound) == c(p, p)
    )
  }
  if (is.null(sigma_l_ubound)) {
    sigma_l_ubound <- matrix(
      data = NA,
      nrow = p,
      ncol = p
    )
  } else {
    stopifnot(
      is.matrix(sigma_l_ubound),
      dim(sigma_l_ubound) == c(p, p)
    )
  }
  sigma_l_free <- matrix(
    data = TRUE,
    nrow = p,
    ncol = p
  )
  # make sure that matrices are lower triangular
  sigma_l_free[upper.tri(sigma_l_free)] <- FALSE
  sigma_l_labels[upper.tri(sigma_l_labels)] <- NA
  sigma_l_start[upper.tri(sigma_l_start)] <- 0
  sigma_l_lbound[upper.tri(sigma_l_lbound)] <- NA
  sigma_l_ubound[upper.tri(sigma_l_ubound)] <- NA
  sigma_l <- OpenMx::mxMatrix(
    type = "Lower",
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
