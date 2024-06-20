.FitDTVAR <- function(data,
                      observed,
                      id,
                      beta_start = NULL,
                      beta_lbound = NULL,
                      beta_ubound = NULL,
                      psi_start = NULL,
                      psi_lbound = NULL,
                      psi_ubound = NULL,
                      try = 1000,
                      ncores = NULL) {
  p <- length(observed)
  idx <- seq_len(p)
  statenames <- paste0(
    "eta",
    idx
  )
  varnames <- paste0(
    "y",
    idx
  )
  ids <- sort(
    unique(data[, id])
  )
  beta <- .FitDTVARBeta(
    p = p,
    statenames = statenames,
    beta_start = beta_start,
    beta_lbound = beta_lbound,
    beta_ubound = beta_ubound
  )
  gamma <- .FitDTVARGamma(p = p)
  lambda <- .FitDTVARLambda(p = p)
  kappa <- .FitDTVARKappa(p = p)
  psi <- .FitDTVARPsi(
    p = p,
    psi_start = psi_start,
    psi_lbound = psi_lbound,
    psi_ubound = psi_ubound
  )
  theta <- .FitDTVARTheta(
    p = p,
    varnames = varnames
  )
  mu0 <- .FitDTVARMu0(p = p)
  sigma0 <- .FitDTVARSigma0(p = p)
  x <- .FitDTVARX()
  par <- FALSE
  if (!is.null(ncores)) {
    ncores <- as.integer(ncores)
    if (ncores > 1) {
      par <- TRUE
    }
  }
  expectation <- mxExpectationStateSpace(
    A = "beta",
    B = "gamma",
    C = "lambda",
    D = "kappa",
    Q = "psi",
    R = "theta",
    x0 = "mu0",
    P0 = "sigma0",
    u = "x",
    dimnames = paste0("y", idx)
  )
  if (par) {
    mxOption(
      key = "Number of Threads",
      value = 1
    )
    cl <- parallel::makeCluster(ncores)
    on.exit(
      parallel::stopCluster(cl = cl)
    )
    output <- parallel::parLapply(
      cl = cl,
      X = ids,
      fun = function(i) {
        model <- mxModel(
          model = "DTVAR",
          beta,
          gamma,
          lambda,
          kappa,
          psi,
          theta,
          mu0,
          sigma0,
          x,
          expectation,
          mxFitFunctionML(),
          mxData(
            observed = data[which(data[, id] == i), ],
            type = "raw"
          )
        )
        mxTryHard(
          model = model,
          extraTries = try
        )
      }
    )
  } else {
    output <- lapply(
      X = ids,
      FUN = function(i) {
        model <- mxModel(
          model = "DTVAR",
          beta,
          gamma,
          lambda,
          kappa,
          psi,
          theta,
          mu0,
          sigma0,
          x,
          expectation,
          mxFitFunctionML(),
          mxData(
            observed = data[which(data[, id] == i), ],
            type = "raw"
          )
        )
        mxTryHard(
          model = model,
          extraTries = try
        )
      }
    )
  }
  return(output)
}
