## ---- test-metaVAR-fit-dt-var-id-mx-theta-null-fixed
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
    mu0 <- null_vec
    sigma0 <- diag(p)
    sigma0_l <- t(chol(sigma0))
    alpha <- null_vec
    psi <- 0.1 * iden
    psi_l <- t(chol(psi))
    beta <- matrix(
      data = c(
        0.7, 0.5, -0.1,
        0.0, 0.6, 0.4,
        0, 0, 0.5
      ),
      nrow = p
    )
    sim <- simStateSpace::SimSSMVARFixed(
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
                beta,
                diag(psi)
              ) - results[idx, "est"]
            ) <= tol
          )
        )
      }
    )
  },
  text = "test-metaVAR-fit-dt-var-id-mx-theta-null-fixed",
  tol = 0.3
)
