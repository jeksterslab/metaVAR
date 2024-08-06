.I2 <- function(object) {
  sigma_hat <- .AverageWithin(
    v = object$args$v,
    n = object$args$n
  )
  sigma <- diag(vcov(object))
  return(
    sigma / (sigma + sigma_hat)
  )
}
