.FitDTVARMu0 <- function(p) {
  # x0
  # initial condition
  # mean
  return(
    OpenMx::mxMatrix(
      type = "Full",
      nrow = p,
      ncol = 1,
      free = FALSE,
      values = rep(x = 0, times = p),
      labels = NA,
      lbound = 10,
      ubound = -10,
      byrow = FALSE,
      name = "mu0"
    )
  )
}
