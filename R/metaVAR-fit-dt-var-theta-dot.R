.FitDTVARTheta <- function(p,
                           varnames) {
  # R
  # measurement error
  return(
    OpenMx::mxMatrix(
      type = "Diag",
      nrow = p,
      ncol = p,
      free = FALSE,
      values = .Machine$double.xmin,
      labels = NA,
      lbound = .Machine$double.xmin,
      ubound = NA,
      byrow = FALSE,
      dimnames = list(
        varnames,
        varnames
      ),
      name = "theta"
    )
  )
}
