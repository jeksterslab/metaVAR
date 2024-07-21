.MetaSigmaNull <- function(p,
                           varnames) {
  sigma_l <- OpenMx::mxMatrix(
    type = "Zero",
    nrow = p,
    ncol = p,
    dimnames = list(
      varnames,
      varnames
    ),
    name = "sigma_l"
  )
  sigma <- OpenMx::mxAlgebra(
    expression = sigma_l %*% t(sigma_l),
    dimnames = list(
      varnames,
      varnames
    ),
    name = "sigma"
  )
  return(
    list(
      sigma_l = sigma_l,
      sigma = sigma
    )
  )
}
