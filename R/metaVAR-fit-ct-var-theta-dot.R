.FitCTVARTheta <- function(p,
                           varnames) {
  # R
  # measurement error
  return(
    .FitDTVARTheta(
      p = p,
      varnames = varnames
    )
  )
}
