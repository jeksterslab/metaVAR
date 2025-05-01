#' Print Method for Object of Class `metavarmeta`
#'
#' @author Ivan Jacob Agaloos Pesigan
#' @param x an object of class `metavarmeta`.
#' @param alpha Numeric vector.
#'   Significance level \eqn{\alpha}.
#' @param digits Integer indicating the number of decimal places to display.
#' @param ... further arguments.
#'
#' @return Returns a matrix of
#'   estimates,
#'   standard errors,
#'   test statistics,
#'   degrees of freedom,
#'   p-values,
#'   and
#'   confidence intervals.
#'
#' @method print metavarmeta
#' @keywords methods
#' @export
print.metavarmeta <- function(x,
                              alpha = 0.05,
                              digits = 4,
                              ...) {
  ci <- .CIMeta(
    object = x,
    alpha = alpha
  )
  ci <- ci[
    !sapply(
      X = ci,
      FUN = is.null
    )
  ]
  base::print(
    round(
      x = do.call(
        what = "rbind",
        args = ci
      ),
      digits = digits
    )
  )
}

#' Summary Method for Object of Class `metavarmeta`
#'
#' @author Ivan Jacob Agaloos Pesigan
#' @param object an object of class `metavarmeta`.
#' @param alpha Numeric vector.
#'   Significance level \eqn{\alpha}.
#' @param digits Integer indicating the number of decimal places to display.
#' @param ... further arguments.
#'
#' @return Returns a matrix of
#'   estimates,
#'   standard errors,
#'   test statistics,
#'   degrees of freedom,
#'   p-values,
#'   and
#'   confidence intervals.
#'
#' @method summary metavarmeta
#' @keywords methods
#' @export
summary.metavarmeta <- function(object,
                                alpha = 0.05,
                                digits = 4,
                                ...) {
  ci <- .CIMeta(
    object = object,
    alpha = alpha
  )
  ci <- ci[
    !sapply(
      X = ci,
      FUN = is.null
    )
  ]
  round(
    x = do.call(
      what = "rbind",
      args = ci
    ),
    digits = digits
  )
}

#' Estimated Parameter Method for an Object of Class
#' `metavarmeta`
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @return Returns a vector of estimated parameters.
#'
#' @inheritParams summary.metavarmeta
#'
#' @method coef metavarmeta
#' @keywords methods
#' @export
coef.metavarmeta <- function(object,
                             ...) {
  coef(object$output)
}

#' Variance-Covariance Matrix Method for an Object of Class
#' `metavarmeta`
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @return Returns the sampling variance-covariance matrix
#'   of the estimated parameters.
#'
#' @inheritParams summary.metavarmeta
#'
#' @method vcov metavarmeta
#' @keywords methods
#' @export
vcov.metavarmeta <- function(object,
                             ...) {
  vcov(object$output)
}
