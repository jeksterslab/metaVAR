.Transform <- function(coef,
                       vcov,
                       p) {
  k <- ((p * (p - 1)) / 2) + p
  sigma_names <- outer(
    X = seq_len(p),
    Y = seq_len(p),
    FUN = function(x,
                   y) {
      paste0(
        "sigma_",
        x,
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
  mu_names <- paste0(
    "mu_",
    seq_len(p)
  )
  varnames <- c(
    sigma_names,
    mu_names
  )
  chol2cov <- function(x,
                       p,
                       k) {
    sigma_l <- matrix(
      data = 0,
      nrow = p,
      ncol = p
    )
    sigma_l[lower.tri(sigma_l, diag = TRUE)] <- x[seq_len(k)]
    sigma <- sigma_l %*% t(sigma_l)
    out <- unname(
      c(
        sigma[
          lower.tri(
            x = sigma,
            diag = TRUE
          )
        ],
        x[(k + 1):length(x)]
      )
    )
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
