.MetaTauNull <- function(p) {
  OpenMx::mxMatrix(
    type = "Zero",
    nrow = p,
    ncol = p,
    name = "tau"
  )
}
