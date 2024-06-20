.FitCTVAR <- function(data,
                      observed,
                      id,
                      time,
                      phi_start = NULL,
                      phi_lbound = NULL,
                      phi_ubound = NULL,
                      sigma_start = NULL,
                      sigma_lbound = NULL,
                      sigma_ubound = NULL,
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
  phi <- .FitCTVARPhi(
    p = p,
    statenames = statenames,
    phi_start = phi_start,
    phi_lbound = phi_lbound,
    phi_ubound = phi_ubound
  )
  gamma <- .FitCTVARGamma(p = p)
  lambda <- .FitCTVARLambda(p = p)
  kappa <- .FitCTVARKappa(p = p)
  sigma <- .FitCTVARSigma(
    p = p,
    sigma_start = sigma_start,
    sigma_lbound = sigma_lbound,
    sigma_ubound = sigma_ubound
  )
  theta <- .FitCTVARTheta(
    p = p,
    varnames = varnames
  )
  mu0 <- .FitCTVARMu0(p = p)
  sigma0 <- .FitCTVARSigma0(p = p)
  x <- .FitCTVARX()
  time <- mxMatrix(
    "Full",
    nrow = 1,
    ncol = 1,
    free = FALSE,
    labels = paste0(
      "data.",
      time
    ),
    name = "time"
  )
  par <- FALSE
  if (!is.null(ncores)) {
    ncores <- as.integer(ncores)
    if (ncores > 1) {
      par <- TRUE
    }
  }
  expectation <- mxExpectationStateSpaceContinuousTime(
    A = "phi",
    B = "gamma",
    C = "lambda",
    D = "kappa",
    Q = "sigma",
    R = "theta",
    x0 = "mu0",
    P0 = "sigma0",
    u = "x",
    t = "time",
    dimnames = varnames
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
          model = "CTVAR",
          phi,
          gamma,
          lambda,
          kappa,
          sigma,
          theta,
          mu0,
          sigma0,
          x,
          time,
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
          model = "CTVAR",
          phi,
          gamma,
          lambda,
          kappa,
          sigma,
          theta,
          mu0,
          sigma0,
          x,
          time,
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
