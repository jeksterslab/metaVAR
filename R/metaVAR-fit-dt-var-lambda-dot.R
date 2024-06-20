.FitDTVARLambda <- function(p) {
  # C
  # measurement model factor loadings
  return(
    mxMatrix(
      type = "Diag",
      nrow = p,
      ncol = p,
      free = FALSE,
      values = 1,
      labels = NA,
      lbound = NA,
      ubound = NA,
      byrow = FALSE,
      dimnames = list(
        paste0("y", seq_len(p)),
        paste0("eta", seq_len(p))
      ),
      name = "lambda"
    )
  )
}
