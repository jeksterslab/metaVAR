.V <- function(vnames,
               p) {
  OpenMx::mxMatrix(
    type = "Symm",
    nrow = p,
    ncol = p,
    labels = paste0(
      "data.",
      vnames
    ),
    name = "v"
  )
}
