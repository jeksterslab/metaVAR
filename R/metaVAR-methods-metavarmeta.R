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
            "sigma_",
            parnames[i],
            "_",
            parnames[j]
          )
        }
      }
    }
    rownames(ci) <- c(
      paste0(
        "mu_",
        parnames
      ),
      v_names[
        lower.tri(
          x = v_names,
          diag = TRUE
        )
      ]
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
            "sigma_",
            parnames[i],
            "_",
            parnames[j]
          )
        }
      }
    }
    rownames(ci) <- c(
      paste0(
        "mu_",
        parnames
      ),
      v_names[
        lower.tri(
          x = v_names,
          diag = TRUE
        )
      ]
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
#' @return Returns a vector of the mean estimated parameters.
#'
#' @inheritParams summary.metavarmeta
#'
#' @method coef metavarmeta
#' @keywords methods
#' @export
coef.metavarmeta <- function(object,
                             ...) {
  est <- object$transform$est
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
            "sigma_",
            parnames[i],
            "_",
            parnames[j]
          )
        }
      }
    }
    names(est) <- c(
      paste0(
        "mu_",
        parnames
      ),
      v_names[
        lower.tri(
          x = v_names,
          diag = TRUE
        )
      ]
    )
  }
  idx <- grep(
    pattern = "^mu_",
    x = names(est)
  )
  est <- est[idx]
  if (!is.null(parnames)) {
    names(est) <- parnames
  }
  return(est)
}

#' Variance-Covariance Matrix Method for an Object of Class
#' `metavarmeta`
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @return Returns the variance-covariance matrix of the estimated parameters.
#'
#' @inheritParams summary.metavarmeta
#'
#' @method vcov metavarmeta
#' @keywords methods
#' @export
vcov.metavarmeta <- function(object,
                             ...) {
  est <- object$transform$est
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
            "sigma_",
            parnames[i],
            "_",
            parnames[j]
          )
        }
      }
    }
    names(est) <- c(
      paste0(
        "mu_",
        parnames
      ),
      v_names[
        lower.tri(
          x = v_names,
          diag = TRUE
        )
      ]
    )
  }
  idx <- grep(
    pattern = "^sigma_",
    x = names(est)
  )
  sym <- matrix(
    data = 0,
    nrow = object$args$p,
    ncol = object$args$p
  )
  sym[lower.tri(sym, diag = TRUE)] <- est[idx]
  sym[upper.tri(sym)] <- t(sym)[upper.tri(sym)]
  if (!is.null(parnames)) {
    rownames(sym) <- colnames(sym) <- parnames
  }
  return(sym)
}
