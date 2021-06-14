#' Covert covariance matrix to ellipse parameters
#'
#' Returns list("major" = major, "minor" = minor, "angle" = angle)
#'
#' @param covar Covariance matrix
#' @export
covarToParameters <- function (covar) {
  eigens <- eigen(covar)
  lambdas <- eigens$values
  minor <- sqrt(lambdas[1])
  major <- sqrt(lambdas[2])

  eigenVectors <- t(eigens$vectors)

  if (length(eigenVectors) == 4) {
    ev2 <- eigenVectors[2,]
    angle <- atan(ev2[2] / ev2[1])
  } else {
    ev1 <- eigenVectors[1]
    angle <- -atan(ev1[1] / ev1[2])
  }

  return(list("major" = major, "minor" = minor, "angle" = angle))
}
