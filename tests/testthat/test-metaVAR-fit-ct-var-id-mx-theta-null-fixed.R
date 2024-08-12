## ---- test-metaVAR-fit-ct-var-id-mx-theta-null-fixed
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
    mu0 <- null_vec
    sigma0 <- diag(p)
    sigma0_l <- t(chol(sigma0))
    mu <- null_vec
    sigma <- 0.1 * iden
    sigma_l <- t(chol(sigma))
    nu <- null_vec
    lambda <- iden
    theta <- null_mat
    theta_l <- null_mat
    phi <- matrix(
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
    sim <- simStateSpace::SimSSMOUFixed(
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
    meta <- MetaVARMx(object = fit, random = FALSE, noise = TRUE)
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
                phi,
                diag(sigma)
              ) - results[idx, "est"]
            ) <= tol
          )
        )
      }
    )
  },
  text = "test-metaVAR-fit-ct-var-id-mx-theta-null-fixed",
  tol = 0.3
)
