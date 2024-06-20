.FitDTVARGamma <- function(p) {
  # B
  # latent variables on covariates
  return(
    mxMatrix(
      type = "Zero",
      nrow = p,
      ncol = 1,
      name = "gamma"
    )
  )
}
