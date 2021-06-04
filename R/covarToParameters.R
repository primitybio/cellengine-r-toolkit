#' Covert covariance matrix to ellipse parameters
#'
#' Returns list("major" = major, "minor" = minor, "angle" = angle)
#'
#' @param covar Covariance matrix
#'
covarToParameters <- function (covar) {
  # eigenvalues are flipped from JS Sylvester.Matrix.eigenvalues
  eigens <- eigen(covar)
  lambdas <- eigens$values
  minor <- sqrt(lambdas[2])
  major <- sqrt(lambdas[1])

  eigenVectors <- eigens$vectors

  if (length(eigenVectors) == 4) {
    ev2 <- eigenVectors[2,]
    angle <- atan(ev2[1] / ev2[2])
  } else {
    ev1 <- eigenVectors[1]
    angle <- -atan(ev1[2] / ev1[1])
  }

  return(list("major" = major, "minor" = minor, "angle" = angle))
}
