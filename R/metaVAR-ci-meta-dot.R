.CIMeta <- function(object,
                    alpha = 0.05) {
  beta0 <- beta1 <- tau_sqr <- i_sqr <- NULL
  # beta0
  beta0_names <- object$output$matrices$beta0$labels
  dim(beta0_names) <- NULL
  coef_beta0 <- OpenMx::mxEval(
    beta0,
    model = object$output
  )
  dim(coef_beta0) <- NULL
  names(coef_beta0) <- beta0_names
  se_beta0 <- OpenMx::mxSE(
    beta0,
    model = object$output,
    silent = TRUE
  )
  dim(se_beta0) <- NULL
  b0 <- .CIWald(
    est = coef_beta0,
    se = se_beta0,
    theta = 0,
    alpha = alpha,
    z = TRUE,
    test = FALSE
  )
  if (is.null(object$args$x)) {
    b1 <- NULL
  } else {
    # TODO: mixed-effects
    b1 <- NULL
  }
  if (object$args$random) {
    # tau_sqr
    coef_tau_sqr <- .Vech(
      OpenMx::mxEval(
        tau_sqr,
        model = object$output
      )
    )
    names(coef_tau_sqr) <- gsub(
      pattern = "^t_",
      replacement = "t2_",
      x = .Vech(
        object$output$matrices$tau$labels
      )
    )
    se_tau_sqr <- .Vech(
      OpenMx::mxSE(
        tau_sqr,
        model = object$output,
        silent = TRUE
      )
    )
    # i_sqr
    coef_i_sqr <- OpenMx::mxEval(
      i_sqr,
      model = object$output
    )
    dim(coef_i_sqr) <- NULL
    names(coef_i_sqr) <- gsub(
      pattern = "^b0_",
      replacement = "i2_",
      x = beta0_names
    )
    se_i_sqr <- OpenMx::mxSE(
      i_sqr,
      model = object$output,
      silent = TRUE
    )
    dim(se_i_sqr) <- NULL
    t2 <- .CIWald(
      est = coef_tau_sqr,
      se = se_tau_sqr,
      theta = 0,
      alpha = alpha,
      z = TRUE,
      test = FALSE
    )
    i2 <- .CIWald(
      est = coef_i_sqr,
      se = se_i_sqr,
      theta = 0,
      alpha = alpha,
      z = TRUE,
      test = FALSE
    )
  } else {
    t2 <- NULL
    i2 <- NULL
  }
  return(
    list(
      b0 = b0,
      b1 = b1,
      t2 = t2,
      i2 = i2
    )
  )
}
