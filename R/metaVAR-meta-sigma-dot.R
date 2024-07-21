.MetaSigma <- function(p,
                       varnames,
                       sigma_l_start = NULL,
                       sigma_l_lbound = NULL,
                       sigma_l_ubound = NULL,
                       diag = FALSE) {
  if (diag) {
    return(
      .MetaSigmaDiag(
        p = p,
        varnames = varnames,
        sigma_l_start = sigma_l_start,
        sigma_l_lbound = sigma_l_lbound,
        sigma_l_ubound = sigma_l_ubound
      )
    )
  } else {
    return(
      .MetaSigmaFull(
        p = p,
        varnames = varnames,
        sigma_l_start = sigma_l_start,
        sigma_l_lbound = sigma_l_lbound,
        sigma_l_ubound = sigma_l_ubound
      )
    )
  }
}
