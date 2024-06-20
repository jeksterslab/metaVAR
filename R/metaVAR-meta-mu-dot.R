.MetaMu <- function(p,
                    varnames,
                    mu_start = NULL) {
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
  return(
    mxMatrix(
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
  )
}
