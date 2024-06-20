.FitDTVARKappa <- function(p) {
  # D
  # observed variables on covariates
  return(
    mxMatrix(
      type = "Zero",
      nrow = p,
      ncol = 1,
      name = "kappa"
    )
  )
}
