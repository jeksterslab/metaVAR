.FitDTVARGamma <- function(p) {
  # B
  # latent variables on covariates
  return(
    OpenMx::mxMatrix(
      type = "Zero",
      nrow = p,
      ncol = 1,
      name = "gamma"
    )
  )
}
