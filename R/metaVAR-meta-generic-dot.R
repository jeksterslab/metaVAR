.MetaGeneric <- function(y,
                         v,
                         x,
                         p,
                         m,
                         n,
                         alpha_values,
                         alpha_free,
                         alpha_lbound,
                         alpha_ubound,
                         beta_values,
                         beta_free,
                         beta_lbound,
                         beta_ubound,
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
        paste0(
          "v",
          x,
          y
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
  mixed <- .MetaMixed(
    p = p,
    m = m,
    ynames = ynames,
    xnames = xnames,
    alpha_values = alpha_values,
    alpha_free = alpha_free,
    alpha_lbound = alpha_lbound,
    alpha_ubound = alpha_ubound,
    beta_values = beta_values,
    beta_free = beta_free,
    beta_lbound = beta_lbound,
    beta_ubound = beta_ubound
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
    mixed$x,
    mixed$alpha,
    mixed$beta,
    mixed$mixed,
    mixed$expected_mean,
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
  OpenMx::mxTryHardctsem(
    model = model,
    extraTries = try,
    ...
  )
}
