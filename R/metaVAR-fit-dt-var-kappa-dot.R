.FitDTVARKappa <- function(p) {
  # D
  # observed variables on covariates
  return(
    OpenMx::mxMatrix(
      type = "Zero",
      nrow = p,
      ncol = 1,
      name = "kappa"
    )
  )
}
