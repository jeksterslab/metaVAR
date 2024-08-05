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
  ci <- .CIWald(
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
  )
  parnames <- names(x$args$y[[1]])
  if (!is.null(parnames)) {
    v_names <- matrix(
      data = NA,
      nrow = x$args$p,
      ncol = x$args$p
    )
    for (j in seq_len(x$args$p)) {
      for (i in seq_len(x$args$p)) {
        if (i >= j) {
          v_names[i, j] <- paste0(
            "tau_sqr_",
            parnames[i],
            "_",
            parnames[j]
          )
        }
      }
    }
    if (x$args$diag) {
      v_names <- diag(v_names)
    } else {
      v_names <- v_names[
        lower.tri(
          x = v_names,
          diag = TRUE
        )
      ]
    }
    rownames(ci) <- c(
      paste0(
        "beta0_",
        parnames
      ),
      v_names
    )
  }
  het <- .I2(object = x)
  base::print(
    list(
      estimates = round(
        x = ci,
        digits = digits
      ),
      heterogeneity = round(
        x = het,
        digits = digits
      )
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
  ci <- .CIWald(
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
  )
  parnames <- names(object$args$y[[1]])
  if (!is.null(parnames)) {
    v_names <- matrix(
      data = NA,
      nrow = object$args$p,
      ncol = object$args$p
    )
    for (j in seq_len(object$args$p)) {
      for (i in seq_len(object$args$p)) {
        if (i >= j) {
          v_names[i, j] <- paste0(
            "tau_sqr_",
            parnames[i],
            "_",
            parnames[j]
          )
        }
      }
    }
    if (object$args$diag) {
      v_names <- diag(v_names)
    } else {
      v_names <- v_names[
        lower.tri(
          x = v_names,
          diag = TRUE
        )
      ]
    }
    rownames(ci) <- c(
      paste0(
        "beta0_",
        parnames
      ),
      v_names
    )
  }
  het <- .I2(object = object)
  return(
    list(
      estimates = round(
        x = ci,
        digits = digits
      ),
      heterogeneity = round(
        x = het,
        digits = digits
      )
    )
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
  return(object$transform$est)
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
  return(object$transform$vcov)
}

#' Confidence Intervals for an Object of Class
#' `metavarmeta`
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @return Returns a matrix of confidence intervals.
#'
#' @param object Object of class `metavarmeta`.
#' @param ... additional arguments.
#' @param parm a specification of which parameters
#'   are to be given confidence intervals,
#'   either a vector of numbers or a vector of names.
#'   If missing, all parameters are considered.
#' @param level the confidence level required.
#'
#' @keywords methods
#' @export
confint.metavarmeta <- function(object,
                                parm = NULL,
                                level = 0.95,
                                ...) {
  if (is.null(parm)) {
    parm <- seq_len(
      length(object$transform$est)
    )
  }
  ci <- .CIWald(
    est = object$transform$est,
    se = sqrt(
      diag(
        object$transform$vcov
      )
    ),
    theta = 0,
    alpha = 1 - level[1],
    z = TRUE,
    test = FALSE
  )[parm, 5:6, drop = FALSE] # always z
  varnames <- colnames(ci)
  varnames <- gsub(
    pattern = "%",
    replacement = " %",
    x = varnames
  )
  colnames(ci) <- varnames
  return(
    ci
  )
}
