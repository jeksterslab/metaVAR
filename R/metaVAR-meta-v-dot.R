.V <- function(vnames,
               p) {
  return(
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
  )
}
