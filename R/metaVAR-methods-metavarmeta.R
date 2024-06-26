#' Print Method for Object of Class `metavarmeta`
#'
#' @author Ivan Jacob Agaloos Pesigan
#' @param x an object of class `metavarmeta`.
#' @param alpha Numeric vector.
#'   Significance level \eqn{\alpha}.
#' @param digits Integer indicating the number of decimal places to display.
#' @param ... further arguments.
#'
#' @method print metavarmeta
#' @keywords methods
#' @export
print.metavarmeta <- function(x,
                              alpha = 0.05,
                              digits = 4,
                              ...) {
  base::print(
    round(
      .CIWald(
        est = x$transform$est,
        se = sqrt(
          diag(
            x$transform$vcov
          )
        ),
        theta = 0,
        alpha = alpha,
        z = TRUE,
        test = FALSE
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
#' @method summary metavarmeta
#' @keywords methods
#' @export
summary.metavarmeta <- function(object,
                                alpha = 0.05,
                                digits = 4,
                                ...) {
  return(
    round(
      .CIWald(
        est = object$transform$est,
        se = sqrt(
          diag(
            object$transform$vcov
          )
        ),
        theta = 0,
        alpha = alpha,
        z = TRUE,
        test = FALSE
      ),
      digits = digits
    )
  )
}
