.FitDTVARSigma0 <- function(p) {
  # R0
  # initial condition
  # covariance
  return(
    OpenMx::mxMatrix(
      type = "Full",
      nrow = p,
      ncol = p,
      free = FALSE,
      values = diag(p),
      labels = NA,
      lbound = 10,
      ubound = -10,
      byrow = FALSE,
      name = "sigma0"
    )
  )
}
