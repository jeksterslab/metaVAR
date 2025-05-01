.MetaMixed <- function(p,
                       m,
                       ynames,
                       xnames,
                       alpha_values,
                       alpha_free,
                       alpha_lbound,
                       alpha_ubound,
                       beta_values,
                       beta_free,
                       beta_lbound,
                       beta_ubound) {
  alpha <- .MetaAlpha(
    p = p,
    ynames = ynames,
    alpha_values = alpha_values,
    alpha_free = alpha_free,
    alpha_lbound = alpha_lbound,
    alpha_ubound = alpha_ubound
  )
  beta <- .MetaBeta(
    p = p,
    m = m,
    ynames = ynames,
    xnames = xnames,
    beta_values = beta_values,
    beta_free = beta_free,
    beta_lbound = beta_lbound,
    beta_ubound = beta_ubound
  )
  if (is.null(m)) {
    # intercept only model
    x <- OpenMx::mxMatrix(
      type = "Unit",
      nrow = 1,
      ncol = 1,
      name = "x"
    )
    beta <- OpenMx::mxAlgebra(
      expression = alpha,
      name = "beta"
    )
    mixed <- OpenMx::mxAlgebra(
      expression = alpha,
      name = "mixed"
    )
    expected_mean <- OpenMx::mxAlgebra(
      expression = t(beta),
      name = "expected_mean"
    )
  } else {
    # mixed-effects model
    nrow <- 1 + m
    ncol <- 1
    x_values <- matrix(
      data = NA,
      nrow = nrow,
      ncol = ncol
    )
    x_values[1, 1] <- 1
    x_labels <- matrix(
      data = NA,
      nrow = nrow,
      ncol = ncol
    )
    x_labels[, 1] <- c(
      NA,
      paste0(
        "data.",
        xnames
      )
    )
    x <- OpenMx::mxMatrix(
      type = "Full",
      nrow = nrow,
      ncol = ncol,
      free = FALSE,
      values = x_values,
      labels = x_labels,
      name = "x"
    )
    mixed <- OpenMx::mxAlgebra(
      expression = cbind(
        alpha,
        beta
      ),
      name = "mixed"
    )
    expected_mean <- OpenMx::mxAlgebra(
      expression = t(mixed %*% x),
      name = "expected_mean"
    )
  }
  list(
    x = x,
    alpha = alpha,
    beta = beta,
    mixed = mixed,
    expected_mean = expected_mean
  )
}
