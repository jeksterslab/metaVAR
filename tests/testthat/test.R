## ---- test
lapply(
  X = 1,
  FUN = function(i) {
    testthat::test_that(
      "test",
      {
        testthat::expect_true(
          TRUE
        )
      }
    )
  }
)
