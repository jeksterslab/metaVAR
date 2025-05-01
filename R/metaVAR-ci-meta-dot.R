.CIMeta <- function(object,
                    alpha = 0.05) {
  sig <- alpha
  alpha <- beta <- tau_sqr <- i_sqr <- NULL
  # alpha
  alpha_free <- object$output$matrices$alpha$free
  dim(alpha_free) <- NULL
  idx <- ifelse(
    test = alpha_free,
    yes = TRUE,
    no = FALSE
  )
  alpha_names <- object$output$matrices$alpha$labels
  dim(alpha_names) <- NULL
  coef_alpha <- OpenMx::mxEval(
    alpha,
    model = object$output
  )
  dim(coef_alpha) <- NULL
  names(coef_alpha) <- alpha_names
  se_alpha <- OpenMx::mxSE(
    alpha,
    model = object$output,
    silent = TRUE
  )
  dim(se_alpha) <- NULL
  b0 <- .CIWald(
    est = coef_alpha[idx],
    se = se_alpha[idx],
    theta = 0,
    alpha = sig,
    z = TRUE,
    test = FALSE
  )
  if (is.null(object$args$x)) {
    b1 <- NULL
  } else {
    # beta
    beta_free <- object$output$matrices$beta$free
    dim(beta_free) <- NULL
    idx <- ifelse(
      test = beta_free,
      yes = TRUE,
      no = FALSE
    )
    beta_names <- object$output$matrices$beta$labels
    dim(beta_names) <- NULL
    coef_beta <- OpenMx::mxEval(
      beta,
      model = object$output
    )
    dim(coef_beta) <- NULL
    names(coef_beta) <- beta_names
    se_beta <- OpenMx::mxSE(
      beta,
      model = object$output,
      silent = TRUE
    )
    dim(se_beta) <- NULL
    b1 <- .CIWald(
      est = coef_beta[idx],
      se = se_beta[idx],
      theta = 0,
      alpha = sig,
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
      alpha = sig,
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
      alpha = sig,
      z = TRUE,
      test = FALSE
    )
  } else {
    t2 <- NULL
    i2 <- NULL
  }
  list(
    b0 = b0,
    b1 = b1,
    t2 = t2,
    i2 = i2
  )
}
