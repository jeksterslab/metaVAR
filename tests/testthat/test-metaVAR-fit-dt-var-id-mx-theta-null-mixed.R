## ---- test-metaVAR-fit-dt-var-id-mx-theta-null-mixed
lapply(
  X = 1,
  FUN = function(i,
                 text,
                 tol) {
    message(text)
    set.seed(42)
    n <- 10
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
    beta_sigma <- 0.01 * diag(p * p)
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
    m <- 2
    x <- lapply(
      X = seq_len(n),
      FUN = function(x) {
        sample(
          x = c(0, 1),
          size = m,
          replace = TRUE
        )
      }
    )
    beta <- matrix(
      data = 0.05,
      nrow = p * p,
      ncol = m
    )
    beta[, m] <- 0
    y <- coef(fit)
    v <- vcov(fit)
    y <- lapply(
      X = seq_len(n),
      FUN = function(i) {
        c(
          y[[i]] + beta %*% x[[i]]
        )
      }
    )
    meta <- Meta(
      y = y,
      v = v,
      x = x,
      alpha_values = c(beta_mu),
      alpha_free = c(
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
      alpha_lbound = rep(x = NA, times = p * p),
      alpha_ubound = rep(x = NA, times = p * p),
      beta_values = beta,
      beta_free = matrix(
        data = c(
          rep(x = TRUE, times = 9),
          rep(x = FALSE, times = 9)
        ),
        nrow = p * p,
        ncol = m
      ),
      beta_lbound = matrix(
        data = NA,
        nrow = p * p,
        ncol = m
      ),
      beta_ubound = matrix(
        data = NA,
        nrow = p * p,
        ncol = m
      ),
      tau_values = beta_sigma,
      tau_free = matrix(
        data = c(
          c(TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE),
          c(FALSE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE),
          c(FALSE, FALSE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE),
          c(FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE),
          c(FALSE, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, FALSE, FALSE),
          c(FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, FALSE),
          c(FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE),
          c(FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, FALSE),
          c(FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE)
        ),
        nrow = p * p,
        ncol = p * p
      ),
      tau_lbound = matrix(
        data = .Machine$double.xmin,
        nrow = p * p,
        ncol = p * p
      ),
      tau_ubound = matrix(
        data = NA,
        nrow = p * p,
        ncol = p * p
      )
    )
    print(meta)
    summary(meta)
    coef(meta)
    vcov(meta)
    results <- summary(meta)
    idx <- grep(
      pattern = "^b0_|^b1_|^t2_",
      x = rownames(results)
    )
    testthat::test_that(
      paste(text, 1),
      {
        testthat::expect_true(
          all(
            abs(
              c(
                0.7,
                0.5,
                -0.1,
                0.6,
                0.4,
                0.5,
                beta[, 1],
                diag(beta_sigma)
              ) - results[idx, "est"]
            ) <= tol
          )
        )
      }
    )
  },
  text = "test-metaVAR-fit-dt-var-id-mx-theta-null-mixed",
  tol = 0.3
)
