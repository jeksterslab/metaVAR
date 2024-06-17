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
  par <- FALSE
  if (!is.null(ncores)) {
    ncores <- as.integer(ncores)
    if (ncores > 1) {
      OpenMx::mxOption(
        key = "Number of Threads",
        value = ncores
      )
    }
  }
  if (is.null(sigma_l_start)) {
    sigma_l_start <- diag(p)
  }
  if (is.null(mu_start)) {
    mu_start <- rep(x = 0, times = p)
  }
  mu_free <- matrix(
    data = rep(
      x = TRUE,
      times = p
    ),
    nrow = 1
  )
  mu_labels <- matrix(
    data = paste0(
      "mu_",
      seq_len(p)
    ),
    nrow = 1
  )
  sigma_l_lbound <- sigma_l_labels <- sigma_l_free <- matrix(
    data = NA,
    nrow = p,
    ncol = p
  )
  for (j in seq_len(p)) {
    for (i in seq_len(p)) {
      if (i >= j) {
        sigma_l_free[i, j] <- TRUE
        sigma_l_labels[i, j] <- paste0(
          "sigma_l_",
          i,
          j
        )
      } else {
        sigma_l_free[i, j] <- FALSE
      }
      if (i == j) {
        sigma_l_lbound[i, j] <- .Machine$double.xmin
      }
    }
  }
  mu <- OpenMx::mxMatrix(
    type = "Full",
    nrow = 1,
    ncol = p,
    free = mu_free,
    values = mu_start,
    labels = mu_labels,
    byrow = FALSE,
    dimnames = list(
      "m",
      varnames
    ),
    name = "mu"
  )
  sigma_l <- OpenMx::mxMatrix(
    type = "Full",
    nrow = p,
    ncol = p,
    free = sigma_l_free,
    values = sigma_l_start,
    labels = sigma_l_labels,
    lbound = sigma_l_lbound,
    byrow = FALSE,
    dimnames = list(
      varnames,
      varnames
    ),
    name = "sigma_l"
  )
  sigma <- OpenMx::mxAlgebra(
    expression = sigma_l %*% t(sigma_l),
    dimnames = list(
      varnames,
      varnames
    ),
    name = "sigma"
  )
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
