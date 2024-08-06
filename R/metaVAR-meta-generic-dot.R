.MetaGeneric <- function(y,
                         v,
                         x,
                         p,
                         m,
                         n,
                         beta0_values,
                         beta0_free,
                         beta0_lbound,
                         beta0_ubound,
                         beta1_values,
                         beta1_free,
                         beta1_lbound,
                         beta1_ubound,
                         tau_values,
                         tau_free,
                         tau_lbound,
                         tau_ubound,
                         random,
                         diag,
                         try,
                         ncores,
                         ...) {
  idx <- seq_len(p)
  ynames <- paste0(
    "y",
    idx
  )
  vnames <- .Vech(
    x = outer(
      X = idx,
      Y = idx,
      FUN = function(x, y) {
        return(
          paste0(
            "v",
            x,
            y
          )
        )
      }
    )
  )
  if (!is.null(x)) {
    xnames <- paste0(
      "x",
      seq_len(m)
    )
  } else {
    xnames <- NULL
  }
  estimates <- .CheckEstimates(
    y = y,
    v = v,
    p = p,
    ynames = ynames,
    ncores = ncores
  )
  y <- estimates$y
  v <- estimates$v
  # nocov start
  if (!is.null(ncores)) {
    ncores <- as.integer(ncores)
    if (ncores > 1) {
      OpenMx::mxOption(
        key = "Number of Threads",
        value = ncores
      )
    }
  }
  # nocov end
  beta <- .MetaBeta(
    p = p,
    m = m,
    ynames = ynames,
    xnames = xnames,
    beta0_values = beta0_values,
    beta0_free = beta0_free,
    beta0_lbound = beta0_lbound,
    beta0_ubound = beta0_ubound,
    beta1_values = beta1_values,
    beta1_free = beta1_free,
    beta1_lbound = beta1_lbound,
    beta1_ubound = beta1_ubound
  )
  tau_sqr <- .MetaTau(
    v = v,
    p = p,
    n = n,
    vnames = vnames,
    tau_values = tau_values,
    tau_free = tau_free,
    tau_lbound = tau_lbound,
    tau_ubound = tau_ubound,
    random = random,
    diag = diag
  )
  model <- OpenMx::mxModel(
    model = "Model",
    beta$x,
    beta$beta0,
    beta$beta1,
    beta$beta,
    beta$expected_mean,
    tau_sqr$tau,
    tau_sqr$tau_sqr,
    tau_sqr$v_hat,
    tau_sqr$i_sqr,
    tau_sqr$v,
    tau_sqr$expected_covariance,
    OpenMx::mxData(
      type = "raw",
      observed = .PrepData(
        y = y,
        v = v,
        x = x,
        ynames = ynames,
        vnames = vnames,
        xnames = xnames
      )
    ),
    OpenMx::mxExpectationNormal(
      covariance = "expected_covariance",
      means = "expected_mean",
      dimnames = ynames
    ),
    OpenMx::mxFitFunctionML()
  )
  return(
    OpenMx::mxTryHardctsem(
      model = model,
      extraTries = try,
      ...
    )
  )
}
