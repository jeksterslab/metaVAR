.MetaTau <- function(v,
                     p,
                     n,
                     vnames,
                     tau_values,
                     tau_free,
                     tau_lbound,
                     tau_ubound,
                     random,
                     diag) {
  v_hat <- .VHat(
    v = v,
    p = p,
    n = n
  )
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
  i_sqr <- OpenMx::mxAlgebra(
    expression = (
      diag2vec(
        tau_sqr
      ) / (
        diag2vec(
          tau_sqr
        ) + v_hat
      )
    ),
    name = "i_sqr"
  )
  return(
    list(
      tau = tau,
      tau_sqr = tau_sqr,
      v_hat = v_hat,
      i_sqr = i_sqr,
      v = v,
      expected_covariance = expected_covariance
    )
  )
}
