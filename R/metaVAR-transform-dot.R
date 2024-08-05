.Transform <- function(coef,
                       vcov,
                       p,
                       diag) {
  idx <- seq_len(p)
  beta0_names <- paste0(
    "beta0_",
    idx
  )
  if (diag) {
    varnames <- c(
      beta0_names,
      paste0(
        "tau_sqr_",
        idx,
        idx
      )
    )
  } else {
    tau_sqr_names <- outer(
      X = idx,
      Y = idx,
      FUN = function(x,
                     y) {
        paste0(
          "tau_sqr_",
          x,
          "_",
          y
        )
      }
    )
    tau_sqr_names <- tau_sqr_names[
      lower.tri(
        x = tau_sqr_names,
        diag = TRUE
      )
    ]
    varnames <- c(
      beta0_names,
      tau_sqr_names
    )
  }
  chol2cov <- function(x,
                       p) {
    tau <- matrix(
      data = 0,
      nrow = p,
      ncol = p
    )
    if (diag) {
      out <- unname(
        c(
          x[seq_len(p)],
          (x[(p + 1):length(x)])^2
        )
      )
    } else {
      tau[lower.tri(tau, diag = TRUE)] <- x[(p + 1):length(x)]
      tau_sqr <- tau %*% t(tau)
      out <- unname(
        c(
          x[seq_len(p)],
          tau_sqr[
            lower.tri(
              x = tau_sqr,
              diag = TRUE
            )
          ]
        )
      )
    }
    return(
      out
    )
  }
  constructor <- function(p) {
    return(
      function(x) {
        return(
          chol2cov(
            x = x,
            p = p
          )
        )
      }
    )
  }
  func <- constructor(
    p = p
  )
  jacobian <- numDeriv::jacobian(
    func = func,
    x = coef
  )
  est <- func(x = coef)
  names(est) <- varnames
  vcov <- jacobian %*% vcov %*% t(
    jacobian
  )
  colnames(vcov) <- rownames(vcov) <- varnames
  return(
    list(
      jacobian = jacobian,
      est = est,
      vcov = vcov
    )
  )
}
