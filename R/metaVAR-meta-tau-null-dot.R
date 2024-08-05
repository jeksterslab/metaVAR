.MetaTauNull <- function(p) {
  return(
    OpenMx::mxMatrix(
      type = "Zero",
      nrow = p,
      ncol = p,
      name = "tau"
    )
  )
}
