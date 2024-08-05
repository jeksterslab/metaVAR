## ---- test-metaVAR-fit-dt-var-id-mx-theta-null
lapply(
  X = 1,
  FUN = function(i,
                 text,
                 tol) {
    message(text)
    set.seed(42)
    n <- 2
    time <- 500
    k <- p <- 3
    iden <- diag(k)
    null_vec <- rep(x = 0, times = k)
    mu0 <- list(
      null_vec
    )
    sigma0 <- diag(p)
    sigma0_l <- list(
      t(chol(sigma0))
    )
    alpha <- list(
      null_vec
    )
    psi <- 0.1 * iden
    psi_l <- list(
      t(chol(psi))
    )
    beta_mu <- matrix(
      data = c(
        0.7, 0.5, -0.1,
        0.0, 0.6, 0.4,
        0, 0, 0.5
      ),
      nrow = p
    )
    beta_sigma <- 0.00001 * diag(p * p)
    beta <- simStateSpace::SimBetaN(
      n = n,
      beta = beta_mu,
      vcov_beta_vec_l = t(chol(beta_sigma))
    )
    sim <- simStateSpace::SimSSMVARIVary(
      n = n,
      time = time,
      mu0 = mu0,
      sigma0_l = sigma0_l,
      alpha = alpha,
      beta = beta,
      psi_l = psi_l
    )
    data <- as.data.frame(sim)
    fit <- fitDTVARMx::FitDTVARIDMx(
      data = data,
      observed = paste0("y", seq_len(k)),
      id = "id",
      psi_diag = TRUE,
      ncores = NULL
    )
    meta <- MetaVARMx(fit, noise = TRUE)
    print(meta)
    summary(meta)
    coef(meta)
    vcov(meta)
    idx <- grep(
      pattern = "^beta0_",
      x = names(coef(meta))
    )
    testthat::test_that(
      text,
      {
        testthat::expect_true(
          all(
            abs(
              c(
                beta_mu,
                diag(psi)
              ) - coef(meta)[idx]
            ) <= tol
          )
        )
      }
    )
  },
  text = "test-metaVAR-fit-dt-var-id-mx-theta-null",
  tol = 0.3
)
