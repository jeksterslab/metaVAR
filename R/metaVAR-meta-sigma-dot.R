.MetaSigma <- function(p,
                       varnames,
                       sigma_l_start = NULL) {
  if (is.null(sigma_l_start)) {
    sigma_l_start <- diag(p)
  }
  sigma_l_lbound <- sigma_l_labels <- sigma_l_free <- matrix(
    data = NA,
    nrow = p,
    ncol = p
  )
  for (j in seq_len(p)) {
    for (i in seq_len(p)) {
      if (i >= j) {
        sigma_l_free[i, j] <- TRUE
        sigma_l_labels[i, j] <- paste0(
          "sigma_l_",
          i,
          j
        )
      } else {
        sigma_l_free[i, j] <- FALSE
      }
      if (i == j) {
        sigma_l_lbound[i, j] <- .Machine$double.xmin
      }
    }
  }
  sigma_l <- OpenMx::mxMatrix(
    type = "Full",
    nrow = p,
    ncol = p,
    free = sigma_l_free,
    values = sigma_l_start,
    labels = sigma_l_labels,
    lbound = sigma_l_lbound,
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
