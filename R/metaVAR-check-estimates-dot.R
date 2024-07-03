.CheckEstimates <- function(y,
                            v,
                            p,
                            varnames,
                            ncores = NULL) {
  par <- FALSE
  if (!is.null(ncores)) {
    ncores <- as.integer(ncores)
    if (ncores > 1) {
      par <- TRUE
    }
  }
  if (par) {
    cl <- parallel::makeCluster(ncores)
    on.exit(
      parallel::stopCluster(cl = cl)
    )
    y <- parallel::parLapply(
      cl = cl,
      X = y,
      fun = function(x) {
        names(x) <- varnames
        return(x)
      }
    )
    v <- parallel::parLapply(
      cl = cl,
      X = v,
      fun = function(x) {
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
        colnames(x) <- rownames(x) <- varnames
        return(x)
      }
    )
  } else {
    y <- lapply(
      X = y,
      FUN = function(x) {
        names(x) <- varnames
        return(x)
      }
    )
    v <- lapply(
      X = v,
      FUN = function(x) {
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
        colnames(x) <- rownames(x) <- varnames
        return(x)
      }
    )
  }
  return(
    list(
      y = y,
      v = v
    )
  )
}
