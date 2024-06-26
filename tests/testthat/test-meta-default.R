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
              vcov_y = cov(y)
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
          return(i$vcov_y)
        }
      )
    }
    sim <- foo(
      n = n,
      mu = mu,
      sigma = sigma
    )
    y <- bar(sim)
    vcov_y <- baz(sim)
    meta <- Meta(
      y = y,
      vcov_y = vcov_y,
      mu_start = NULL,
      sigma_l_start = NULL,
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
                  sigma[
                    lower.tri(
                      x = sigma,
                      diag = TRUE
                    )
                  ]
                ),
                mu
              ) - round(
                x = summary(meta)[, "est"],
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
