library("flowCore")

#' Convert a gate to flowCore
#'
#' Converts a CellEngine gate ("PolygonGate", "EllipseGate", or "RectangleGate") to
#' its flowCore analogue.
#'
#' @param gate The CellEngine gate to be converted
convertGate <- function (gate) {
  switch(
    gate$type,
    "RectangleGate" = rectangleGate(filterId = gate$name, setNames(list(c(gate$model$rectangle$x1, gate$model$rectangle$x2), c(gate$model$rectangle$y1, gate$model$rectangle$y2)), c(gate$xChannel, gate$yChannel))),
    "EllipseGate" = {
      ellipse <- gate$model$ellipse
      points <- getEllipsePoints(ellipse$center[1], ellipse$center[2], ellipse$angle, ellipse$major, ellipse$minor)
      points <- t(data.frame(points))

      result <- fitEllipsePoints(points)

      cov <- result$covar
      colnames(cov) <- c(gate$xChannel, gate$yChannel)
      rownames(cov) <- c(gate$xChannel, gate$yChannel)

      return(ellipsoidGate(filterId = gate$name, .gate = cov, mean = result$center))
    },
    "PolygonGate" = {
      m <- gate$model$polygon$vertices
      colnames(m) <- c(gate$xChannel, gate$yChannel)
      polygonGate(filterId = gate$name, m)
    }
  )
}
