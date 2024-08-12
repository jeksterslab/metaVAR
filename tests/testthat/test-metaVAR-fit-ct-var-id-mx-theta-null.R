## ---- test-metaVAR-fit-ct-var-id-mx-theta-null
lapply(
  X = 1,
  FUN = function(i,
                 text,
                 tol) {
    message(text)
    set.seed(42)
    n <- 2
    time <- 500
    delta_t <- 0.10
    k <- p <- 3
    iden <- diag(k)
    null_vec <- rep(x = 0, times = k)
    null_mat <- matrix(
      data = 0,
      nrow = p,
      ncol = p
    )
    mu0 <- list(
      null_vec
    )
    sigma0 <- diag(p)
    sigma0_l <- list(
      t(chol(sigma0))
    )
    mu <- list(
      null_vec
    )
    sigma <- 0.1 * iden
    sigma_l <- list(
      t(chol(sigma))
    )
    nu <- list(
      null_vec
    )
    lambda <- list(
      iden
    )
    theta <- null_mat
    theta_l <- list(
      null_mat
    )
    phi_mu <- matrix(
      data = c(
        -0.357,
        0.771,
        -0.450,
        0.0,
        -0.511,
        0.729,
        0,
        0,
        -0.693
      ),
      nrow = k
    )
    phi_sigma <- 0.00001 * diag(p * p)
    phi <- simStateSpace::SimPhiN(
      n = n,
      phi = phi_mu,
      vcov_phi_vec_l = t(chol(phi_sigma))
    )
    sim <- simStateSpace::SimSSMOUIVary(
      n = n,
      time = time,
      delta_t = delta_t,
      mu0 = mu0,
      sigma0_l = sigma0_l,
      mu = mu,
      phi = phi,
      sigma_l = sigma_l,
      nu = nu,
      lambda = lambda,
      theta_l = theta_l
    )
    data <- as.data.frame(sim)
    fit <- fitCTVARMx::FitCTVARIDMx(
      data = data,
      observed = paste0("y", seq_len(k)),
      id = "id",
      time = "time",
      sigma_diag = TRUE,
      ncores = NULL
    )
    meta <- MetaVARMx(object = fit, noise = TRUE)
    print(meta)
    summary(meta)
    coef(meta)
    vcov(meta)
    results <- summary(meta)
    idx <- grep(
      pattern = "^b0_",
      x = rownames(results)
    )
    testthat::test_that(
      paste(text, 1),
      {
        testthat::expect_true(
          all(
            abs(
              c(
                phi_mu,
                diag(sigma)
              ) - results[idx, "est"]
            ) <= tol
          )
        )
      }
    )
    x <- lapply(
      X = seq_len(n),
      FUN = function(x) {
        stats::rnorm(n = 2)
      }
    )
    meta <- MetaVARMx(
      object = fit,
      x = x,
      beta0_values = c(phi_mu),
      beta0_free = c(
        TRUE,
        TRUE,
        TRUE,
        FALSE,
        TRUE,
        TRUE,
        FALSE,
        FALSE,
        TRUE
      ),
      beta0_lbound = rep(x = NA, times = p * p),
      beta0_ubound = rep(x = NA, times = p * p)
    )
    results <- summary(meta)
    idx <- grep(
      pattern = "^b0_",
      x = rownames(results)
    )
    testthat::test_that(
      paste(text, 2),
      {
        testthat::expect_true(
          all(
            abs(
              c(
                -0.357,
                0.771,
                -0.450,
                -0.511,
                0.729,
               -0.693
              ) - results[idx, "est"]
            ) <= tol
          )
        )
      }
    )
  },
  text = "test-metaVAR-fit-ct-var-id-mx-theta-null",
  tol = 0.3
)
