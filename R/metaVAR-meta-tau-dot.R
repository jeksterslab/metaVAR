.MetaTau <- function(p,
                     vnames,
                     tau_values,
                     tau_free,
                     tau_lbound,
                     tau_ubound,
                     random,
                     diag) {
  if (random) {
    if (diag) {
      tau <- .MetaTauDiag(
        p = p,
        tau_values = tau_values,
        tau_lbound = tau_lbound,
        tau_ubound = tau_ubound
      )
    } else {
      tau <- .MetaTauFull(
        p = p,
        tau_values = tau_values,
        tau_free = tau_free,
        tau_lbound = tau_lbound,
        tau_ubound = tau_ubound
      )
    }
  } else {
    tau <- .MetaTauNull(
      p = p
    )
  }
  tau_sqr <- OpenMx::mxAlgebra(
    expression = tau %*% t(tau),
    name = "tau_sqr"
  )
  v <- .V(
    vnames = vnames,
    p = p
  )
  expected_covariance <- OpenMx::mxAlgebra(
    expression = v + tau_sqr,
    name = "expected_covariance"
  )
  return(
    list(
      tau = tau,
      tau_sqr = tau_sqr,
      v = v,
      expected_covariance = expected_covariance
    )
  )
}
