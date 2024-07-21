.Transform <- function(coef,
                       vcov,
                       p,
                       diag) {
  idx <- seq_len(p)
  mu_names <- paste0(
    "mu_",
    idx
  )
  if (diag) {
    varnames <- c(
      mu_names,
      paste0(
        "sigma_",
        idx,
        idx
      )
    )
  } else {
    k <- ((p * (p - 1)) / 2) + p
    sigma_names <- outer(
      X = idx,
      Y = idx,
      FUN = function(x,
                     y) {
        paste0(
          "sigma_",
          x,
          "_",
          y
        )
      }
    )
    sigma_names <- sigma_names[
      lower.tri(
        x = sigma_names,
        diag = TRUE
      )
    ]
    varnames <- c(
      mu_names,
      sigma_names
    )
  }
  chol2cov <- function(x,
                       p,
                       k) {
    sigma_l <- matrix(
      data = 0,
      nrow = p,
      ncol = p
    )
    if (diag) {
      out <- unname(
        c(
          x[(p + 1):length(x)],
          (x[seq_len(p)])^2
        )
      )
    } else {
      sigma_l[lower.tri(sigma_l, diag = TRUE)] <- x[seq_len(k)]
      sigma <- sigma_l %*% t(sigma_l)
      out <- unname(
        c(
          x[(k + 1):length(x)],
          sigma[
            lower.tri(
              x = sigma,
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
  constructor <- function(p, k) {
    return(
      function(x) {
        return(
          chol2cov(
            x = x,
            p = p,
            k = k
          )
        )
      }
    )
  }
  func <- constructor(
    p = p,
    k = k
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
