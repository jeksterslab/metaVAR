.I2 <- function(object) {
  v_hat <- .AverageWithin(
    v = object$args$v,
    n = object$args$n
  )
  tau_sqr <- diag(vcov(object))
  return(
    tau_sqr / (tau_sqr + v_hat)
  )
}
