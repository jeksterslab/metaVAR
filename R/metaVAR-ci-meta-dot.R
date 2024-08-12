.CIMeta <- function(object,
                    alpha = 0.05) {
  beta0 <- beta1 <- tau_sqr <- i_sqr <- NULL
  # beta0
  beta0_free <- object$output$matrices$beta0$free
  dim(beta0_free) <- NULL
  idx <- ifelse(
    test = beta0_free,
    yes = TRUE,
    no = FALSE
  )
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
    est = coef_beta0[idx],
    se = se_beta0[idx],
    theta = 0,
    alpha = alpha,
    z = TRUE,
    test = FALSE
  )
  if (is.null(object$args$x)) {
    b1 <- NULL
  } else {
    # beta1
    beta1_free <- object$output$matrices$beta1$free
    dim(beta1_free) <- NULL
    idx <- ifelse(
      test = beta1_free,
      yes = TRUE,
      no = FALSE
    )
    beta1_names <- object$output$matrices$beta1$labels
    dim(beta1_names) <- NULL
    coef_beta1 <- OpenMx::mxEval(
      beta1,
      model = object$output
    )
    dim(coef_beta1) <- NULL
    names(coef_beta1) <- beta1_names
    se_beta1 <- OpenMx::mxSE(
      beta1,
      model = object$output,
      silent = TRUE
    )
    dim(se_beta1) <- NULL
    b1 <- .CIWald(
      est = coef_beta1[idx],
      se = se_beta1[idx],
      theta = 0,
      alpha = alpha,
      z = TRUE,
      test = FALSE
    )
  }
  if (object$args$random) {
    # tau_sqr
    tau_sqr_free <- .Vech(object$output$matrices$tau$free)
    idx <- ifelse(
      test = tau_sqr_free,
      yes = TRUE,
      no = FALSE
    )
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
    t2 <- .CIWald(
      est = coef_tau_sqr[idx],
      se = se_tau_sqr[idx],
      theta = 0,
      alpha = alpha,
      z = TRUE,
      test = FALSE
    )
    # i_sqr
    coef_i_sqr <- OpenMx::mxEval(
      i_sqr,
      model = object$output
    )
    dim(coef_i_sqr) <- NULL
    names(coef_i_sqr) <- paste0(
      "i2_",
      seq_len(length(coef_i_sqr))
    )
    se_i_sqr <- OpenMx::mxSE(
      i_sqr,
      model = object$output,
      silent = TRUE
    )
    dim(se_i_sqr) <- NULL
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
