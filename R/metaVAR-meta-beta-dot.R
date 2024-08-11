.MetaBeta <- function(p,
                      m,
                      ynames,
                      xnames,
                      beta0_values,
                      beta0_free,
                      beta0_lbound,
                      beta0_ubound,
                      beta1_values,
                      beta1_free,
                      beta1_lbound,
                      beta1_ubound) {
  beta0 <- .MetaBeta0(
    p = p,
    ynames = ynames,
    beta0_values = beta0_values,
    beta0_free = beta0_free,
    beta0_lbound = beta0_lbound,
    beta0_ubound = beta0_ubound
  )
  beta1 <- .MetaBeta1(
    p = p,
    m = m,
    ynames = ynames,
    xnames = xnames,
    beta1_values = beta1_values,
    beta1_free = beta1_free,
    beta1_lbound = beta1_lbound,
    beta1_ubound = beta1_ubound
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
      expression = beta0,
      name = "beta"
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
    beta <- OpenMx::mxAlgebra(
      expression = cbind(
        beta0,
        beta1
      ),
      name = "beta"
    )
    expected_mean <- OpenMx::mxAlgebra(
      expression = t(beta %*% x),
      name = "expected_mean"
    )
  }
  return(
    list(
      x = x,
      beta0 = beta0,
      beta1 = beta1,
      beta = beta,
      expected_mean = expected_mean
    )
  )
}
