.VHat <- function(v,
                  p,
                  n) {
  return(
    OpenMx::mxMatrix(
      type = "Full",
      ncol = 1,
      nrow = p,
      free = FALSE,
      values = matrix(
        data = .AverageWithin(
          v = v,
          n = n
        ),
        ncol = 1
      ),
      name = "v_hat"
    )
  )
}
