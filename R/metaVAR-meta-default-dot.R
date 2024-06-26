.MetaDefault <- function(y,
                         vcov_y,
                         n,
                         p,
                         mu_start = NULL,
                         sigma_l_start = NULL,
                         try = 1000,
                         ncores = NULL) {
  args <- list(
    y = y,
    vcov_y = vcov_y,
    n = n,
    p = p,
    mu_start = mu_start,
    sigma_l_start = sigma_l_start,
    try = try,
    ncores = ncores
  )
  output <- .MetaGeneric(
    y = y,
    vcov_y = vcov_y,
    n = n,
    p = p,
    mu_start = mu_start,
    sigma_l_start = sigma_l_start,
    try = try,
    ncores = ncores
  )
  out <- list(
    call = match.call(),
    args = args,
    fun = "MetaDefault",
    output = output,
    transform = .Transform(
      coef = coef(output),
      vcov = vcov(output),
      p = p
    )
  )
  class(out) <- c(
    "metavarmeta",
    class(out)
  )
  return(out)
}
