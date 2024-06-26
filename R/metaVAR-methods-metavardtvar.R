#' Print Method for Object of Class `metavardtvar`
#'
#' @author Ivan Jacob Agaloos Pesigan
#' @param x an object of class `metavardtvar`.
#' @param means Logical.
#'   If `means = TRUE`, return means.
#'   Otherwise, the function returns raw estimates.
#' @param ... further arguments.
#'
#' @method print metavardtvar
#' @keywords methods
#' @export
print.metavardtvar <- function(x,
                               means = TRUE,
                               ...) {
  out <- do.call(
    what = "rbind",
    args = lapply(
      X = x$output,
      FUN = coef
    )
  )
  if (means) {
    cat("\nMeans of the estimated paramaters per individual.\n")
    out <- colMeans(out)
  } else {
    cat("\nEstimated paramaters per individual.\n")
  }
  base::print(out)
}

#' Summary Method for Object of Class `metavardtvar`
#'
#' @author Ivan Jacob Agaloos Pesigan
#' @param object an object of class `metavardtvar`.
#' @param means Logical.
#'   If `means = TRUE`, return means.
#'   Otherwise, the function returns raw estimates.
#' @param ... further arguments.
#'
#' @method summary metavardtvar
#' @keywords methods
#' @export
summary.metavardtvar <- function(object,
                                 means = TRUE,
                                 ...) {
  out <- do.call(
    what = "rbind",
    args = lapply(
      X = object$output,
      FUN = coef
    )
  )
  if (means) {
    if (interactive()) {
      cat("\nMeans of the estimated paramaters per individual.\n")
    }
    out <- colMeans(out)
  } else {
    if (interactive()) {
      cat("\nEstimated paramaters per individual.\n")
    }
  }
  return(out)
}
