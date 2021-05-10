#' Fit ellipse points
#'
#' Calculate the minimum volume enclosing ellipsoid for a set of points
#'
#' Uses the Khachiyan Algorithm to find the the minimum volume enclosing
#' ellipsoid (MVEE) of a set of points. In two dimensions, this is just
#' the bounding ellipse (this function is limited to two dimensions).
#' Adapted from https://stat.ethz.ch/pipermail/r-help/2011-March/272997.html
#'
#' @param xy a two-column data frame containing x and y coordinates.
#' @param tolerance a tolerance value (default = 0.005).
#' @param max.iter Maximum algorithm iterations.
#'
#' @return A named list containing:
#'  - "covar", the 2x2 covariance matrix of the ellipse
#'  - "center", a 2x1 vector of the center of the ellipse such that:
#'      (x - C)' A (x - C) <= 1
#'  - "axes", a 2x1 vector whose elements are one-half the
#'      lengths of the major and minor axes
#'  - "angle", the angle of rotation
fitEllipsePoints <- function(xy, tolerance = 0.005, max.iter = 500) {
  if (ncol(xy) != 2) {
    stop("xy must be a two-column data frame")
  }
  n = nrow(xy)
  d = ncol(xy)
  if (n <= d) stop("There cannot be more columns than rows")
  
  ## Add a column of 1s to the (n x 2) matrix xy - so it is now (n x 3)
  Q <- t(cbind(xy, rep(1,n)))
  
  ## Initialize
  count <- 1
  err <- 1
  u <- rep(1/n, n)
  ## Khachiyan Algorithm
  while (err > tolerance)
  {
    X <- Q %*% diag(u) %*% t(Q)
    M <- diag(t(Q) %*% solve(X) %*% Q)
    maximum <- max(M)
    j <- which(M == maximum)
    step_size = (maximum - d -1) / ((d+1)*(maximum-1))
    new_u <- (1 - step_size) * u
    new_u[j] <- new_u[j] + step_size
    err <- sqrt(sum((new_u - u)^2))
    count <- count + 1
    if (count > max.iter) {
      warning(paste("Iterated", max.iter, "times and still can't find the bounding ellipse. \n", sep=""))
      warning("Either increase the tolerance or the maximum number of iterations")
      return(NULL)
    }
    u <- new_u
  }
  # Put the elements of the vector u into the diagonal of a matrix
  U <- diag(u)
  # Take the transpose of xy
  P <- t(xy)
  # Compute the center, adding back the shifted values
  c <- as.vector((P %*% u))
  # Compute the A-matrix
  A <- (1/d) * solve(P %*% U %*% t(P) - (P %*% u) %*% t(P %*% u))
  # Find the Eigenvalues of matrix A which will be used to get the major and minor axes
  A.eigen <- eigen(A)
  # Calculate the length of the semi-major and semi-minor axes
  # from the Eigenvalues of A.
  semi.axes <- sort(1 / sqrt(A.eigen$values), decreasing=TRUE)
  # Calculate the rotation angle from the first Eigenvector
  alpha <- atan2(A.eigen$vectors[2,1], A.eigen$vectors[1,1]) - pi/2
  ellipse.params <- list("covar" = A, "center" = c, "axes" = semi.axes, angle = alpha)
  return(ellipse.params)
}
