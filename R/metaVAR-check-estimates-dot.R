.CheckEstimates <- function(y,
                            vcov_y,
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
    vcov_y <- parallel::parLapply(
      cl = cl,
      X = vcov_y,
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
    vcov_y <- lapply(
      X = vcov_y,
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
      vcov_y = vcov_y
    )
  )
}
