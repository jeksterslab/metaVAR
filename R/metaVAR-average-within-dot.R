.AverageWithin <- function(v,
                           n) {
  sum_v_inv <- colSums(
    do.call(
      what = "rbind",
      args = lapply(
        X = v,
        FUN = function(i) {
          1 / diag(i)
        }
      )
    )
  )
  sum_v_sqr_inv <- colSums(
    do.call(
      what = "rbind",
      args = lapply(
        X = v,
        FUN = function(i) {
          1 / diag(i)^2
        }
      )
    )
  )
  (
    (n - 1) * sum_v_inv
  ) / (
    sum_v_inv^2 - sum_v_sqr_inv
  )
}
