## ---- test-meta-default
lapply(
  X = 1,
  FUN = function(i,
                 text,
                 tol) {
    message(text)
    set.seed(42)
    n <- 50
    p <- 2
    mu <- rep(x = 0.10, times = p)
    sigma <- 0.10 * diag(p)
    foo <- function(n,
                    mu,
                    sigma) {
      return(
        lapply(
          X = seq_len(n),
          FUN = function(i,
                         mu,
                         sigma) {
            y <- MASS::mvrnorm(
              n = 100000,
              mu = mu,
              Sigma = sigma
            )
            list(
              y = colMeans(y),
              v = cov(y)
            )
          },
          mu = mu,
          sigma = sigma
        )
      )
    }
    bar <- function(sim) {
      lapply(
        X = sim,
        FUN = function(i) {
          return(i$y)
        }
      )
    }
    baz <- function(sim) {
      lapply(
        X = sim,
        FUN = function(i) {
          return(i$v)
        }
      )
    }
    sim <- foo(
      n = n,
      mu = mu,
      sigma = sigma
    )
    y <- bar(sim)
    v <- baz(sim)
    meta <- Meta(
      y = y,
      v = v,
      mu_start = mu,
      mu_lbound = rep(x = NA, times = p),
      mu_ubound = rep(x = NA, times = p),
      sigma_l_start = t(chol(sigma)),
      sigma_l_lbound = matrix(data = NA, nrow = p, ncol = p),
      sigma_l_ubound = matrix(data = NA, nrow = p, ncol = p),
      try = 1000,
      ncores = NULL
    )
    testthat::test_that(
      paste0(text, "Meta"),
      {
        testthat::expect_true(
          all(
            abs(
              c(
                c(
                  mu,
                  sigma[
                    lower.tri(
                      x = sigma,
                      diag = TRUE
                    )
                  ]
                )
              ) - round(
                x = summary(meta)$estimates[, "est"],
                digits = 2
              )
            ) <= tol
          )
        )
      }
    )
    meta <- Meta(
      y = y,
      v = v,
      mu_start = mu,
      mu_lbound = rep(x = NA, times = p),
      mu_ubound = rep(x = NA, times = p),
      sigma_l_start = t(chol(sigma)),
      sigma_l_lbound = matrix(data = NA, nrow = p, ncol = p),
      sigma_l_ubound = matrix(data = NA, nrow = p, ncol = p),
      diag = TRUE,
      try = 1000,
      ncores = NULL
    )
    testthat::test_that(
      paste0(text, "Meta", "2"),
      {
        testthat::expect_true(
          all(
            abs(
              c(
                c(
                  mu,
                  diag(sigma)
                )
              ) - round(
                x = summary(meta)$estimates[, "est"],
                digits = 2
              )
            ) <= tol
          )
        )
      }
    )
  },
  text = "test-meta-default",
  tol = 0.001
)
