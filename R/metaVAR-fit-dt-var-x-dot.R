.FitDTVARX <- function() {
  # u
  # covariates
  return(
    OpenMx::mxMatrix(
      type = "Zero",
      nrow = 1,
      ncol = 1,
      name = "x"
    )
  )
}
