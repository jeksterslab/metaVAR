.Meta <- function(y,
                  vcov_y,
                  n,
                  p,
                  mu_start = NULL,
                  sigma_l_start = NULL,
                  try = 1000,
                  ncores = NULL) {
  varnames <- paste0(
    "y",
    seq_len(p)
  )
  estimates <- .CheckEstimates(
    y = y,
    vcov_y = vcov_y,
    p = p,
    varnames = varnames,
    ncores = ncores
  )
  y <- estimates$y
  vcov_y <- estimates$vcov_y
  if (!is.null(ncores)) {
    ncores <- as.integer(ncores)
    if (ncores > 1) {
      OpenMx::mxOption(
        key = "Number of Threads",
        value = ncores
      )
    }
  }
  mu <- .MetaMu(
    p = p,
    mu_start = mu_start
  )
  sigma <- .MetaSigma(
    p = p,
    sigma_l_start = sigma_l_start
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
        observed = vcov_y[[i]],
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
  OpenMx::mxTryHard(
    model = model,
    extraTries = try
  )
}
