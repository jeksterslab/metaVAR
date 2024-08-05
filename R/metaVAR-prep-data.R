.PrepData <- function(y,
                      v,
                      x,
                      ynames,
                      vnames,
                      xnames) {
  y <- do.call(
    what = "rbind",
    args = y
  )
  colnames(y) <- ynames
  v <- do.call(
    what = "rbind",
    args = lapply(
      X = v,
      FUN = function(x) {
        return(
          x[
            lower.tri(
              x = x,
              diag = TRUE
            )
          ]
        )
      }
    )
  )
  colnames(v) <- vnames
  data <- cbind(
    y,
    v
  )
  if (!is.null(x) && !is.null(xnames)) {
    x <- do.call(
      what = "rbind",
      args = x
    )
    colnames(x) <- xnames
    data <- cbind(
      data,
      x
    )
  }
  return(
    as.data.frame(data)
  )
}
