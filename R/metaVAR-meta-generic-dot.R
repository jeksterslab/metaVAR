.MetaGeneric <- function(y,
                         v,
                         n,
                         p,
                         mu_start = NULL,
                         mu_lbound = NULL,
                         mu_ubound = NULL,
                         sigma_l_start = NULL,
                         sigma_l_lbound = NULL,
                         sigma_l_ubound = NULL,
                         try = 1000,
                         ncores = NULL) {
  varnames <- paste0(
    "y",
    seq_len(p)
  )
  estimates <- .CheckEstimates(
    y = y,
    v = v,
    p = p,
    varnames = varnames,
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
  mu <- .MetaMu(
    p = p,
    varnames = varnames,
    mu_start = mu_start,
    mu_lbound = mu_lbound,
    mu_ubound = mu_ubound
  )
  sigma <- .MetaSigma(
    p = p,
    varnames = varnames,
    sigma_l_start = sigma_l_start,
    sigma_l_lbound = sigma_l_lbound,
    sigma_l_ubound = sigma_l_ubound
  )
  sigma_l <- sigma$sigma_l
  sigma <- sigma$sigma
  model_i <- vector(
    mode = "list",
    length = n
  )
  for (i in seq_len(n)) {
    model_i[[i]] <- OpenMx::mxModel(
      model = paste0(
        "Model_",
        i
      ),
      sigma_l,
      sigma,
      mu,
      OpenMx::mxData(
        type = "cov",
        observed = v[[i]],
        means = y[[i]],
        numObs = n
      ),
      OpenMx::mxExpectationNormal(
        covariance = "sigma",
        means = "mu"
      ),
      OpenMx::mxFitFunctionML()
    )
  }
  model <- OpenMx::mxModel(
    model = "Model",
    model_i,
    OpenMx::mxFitFunctionMultigroup(
      paste0(
        "Model_",
        seq_len(n)
      )
    )
  )
  return(
    OpenMx::mxTryHardctsem(
      model = model,
      extraTries = try
    )
  )
}
