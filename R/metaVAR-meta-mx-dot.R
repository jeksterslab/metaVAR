.MetaMx <- function(object,
                    mu_start = NULL,
                    sigma_l_start = NULL,
                    try = 1000,
                    ncores = NULL) {
  args <- list(
    object = object,
    mu_start = mu_start,
    sigma_l_start = sigma_l_start,
    try = try,
    ncores = ncores
  )
  y <- lapply(
    X = object$output,
    FUN = function(x) {
      coefs <- coef(x)
      idx <- grep(
        pattern = "^beta_|^phi_",
        x = names(coefs)
      )
      return(
        coefs[idx]
      )
    }
  )
  vcov_y <- lapply(
    X = object$output,
    FUN = function(x) {
      vcovs <- vcov(x)
      idx <- grep(
        pattern = "^beta_|^phi_",
        x = colnames(vcovs)
      )
      return(vcovs[idx, idx])
    }
  )
  output <- .MetaGeneric(
    y = y,
    vcov_y = vcov_y,
    n = length(y),
    p = length(y[[1]]),
    mu_start = mu_start,
    sigma_l_start = sigma_l_start,
    try = try,
    ncores = ncores
  )
  out <- list(
    call = match.call(),
    args = args,
    fun = "MetaMx",
    output = output,
    transform = .Transform(
      coef = coef(output),
      vcov = vcov(output),
      p = length(y[[1]])
    )
  )
  class(out) <- c(
    "metavarmeta",
    class(out)
  )
  return(out)
}
