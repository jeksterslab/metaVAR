.CheckEstimates <- function(y,
                            v,
                            p,
                            ynames,
                            ncores) {
  par <- FALSE
  # nocov start
  if (!is.null(ncores)) {
    ncores <- as.integer(ncores)
    if (ncores > 1) {
      par <- TRUE
    }
  }
  foo <- function(x) {
    if (
      any(
        eigen(
          x = x,
          symmetric = TRUE,
          only.values = TRUE
        )$values <= 1e-06
      )
    ) {
      x <- as.matrix(
        Matrix::nearPD(x)$mat
      )
    }
    colnames(x) <- rownames(x) <- ynames
    return(x)
  }
  # nocov end
  if (par) {
    # nocov start
    cl <- parallel::makeCluster(ncores)
    on.exit(
      parallel::stopCluster(cl = cl)
    )
    y <- parallel::parLapply(
      cl = cl,
      X = y,
      fun = function(x) {
        names(x) <- ynames
        return(x)
      }
    )
    v <- parallel::parLapply(
      cl = cl,
      X = v,
      fun = foo
    )
    # nocov end
  } else {
    y <- lapply(
      X = y,
      FUN = function(x) {
        names(x) <- ynames
        return(x)
      }
    )
    v <- lapply(
      X = v,
      FUN = foo
    )
  }
  return(
    list(
      y = y,
      v = v
    )
  )
}
