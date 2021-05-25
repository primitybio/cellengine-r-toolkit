library("stats")

#' Convert a gate to flowCore
#'
#' Converts a CellEngine gate ("PolygonGate", "EllipseGate", or "RectangleGate") to
#' its flowCore analogue.
#'
#' @param gate The CellEngine gate to be converted
convertToFlowCore <- function (gate) {
  if (!requireNamespace("flowCore")) {
    message("These utilities require the 'flowCore' package.")
    return(invisible())
  }

  switch(
    gate$type,
    "RectangleGate" = flowCore::rectangleGate(
      filterId = gate$name,
      stats::setNames(list(
        c(gate$model$rectangle$x1, gate$model$rectangle$x2),
        c(gate$model$rectangle$y1, gate$model$rectangle$y2)),
      c(gate$xChannel, gate$yChannel))),
    "EllipseGate" = {
      ellipse <- gate$model$ellipse
      points <- getEllipsePoints(ellipse$center[1], ellipse$center[2], ellipse$angle, ellipse$major, ellipse$minor)
      points <- t(data.frame(points))

      result <- fitEllipsePoints(points)

      cov <- result$covar
      colnames(cov) <- c(gate$xChannel, gate$yChannel)
      rownames(cov) <- c(gate$xChannel, gate$yChannel)

      return(flowCore::ellipsoidGate(filterId = gate$name, .gate = cov, mean = result$center))
    },
    "PolygonGate" = {
      m <- gate$model$polygon$vertices
      colnames(m) <- c(gate$xChannel, gate$yChannel)
      flowCore::polygonGate(filterId = gate$name, m)
    }
  )
}

#' Scale flowCore gate
#'
#' Applies a scale to points from a flowCore gate, so that it may
#' be saved to CellEngine.
#'
#' @param gate The flowCore gate points (i.e. PologonalGate@boundaries)
#' @param scaleSet The CellEngine ScaleSet.
#' @export
#' @examples
#' \dontrun{
#' sngl <- flowDensity(file, params)
#' flowGate <- sngl@filter
#' scaleSet <- getScaleSets("5d2f8b4b21fd0676fb3a6a8c")
#' gatePoints <- scaleFlowCoreGate(flowGate, scaleSet)
#' createPolygonGate(id, colnames(flowGate)[1], colnames(flowGate)[2], 'my gate', vertices=gatePoints)
#' }
scaleFlowCoreGate <- function (gate, scaleSet) {
  xChannel <- colnames(gate)[1]
  yChannel <- colnames(gate)[2]
  scales = data.frame(scaleSet$scales)
  s = scales$scale
  rownames(s) = scales$channelName

  scaleX <- s[xChannel,]
  scaleY <- s[yChannel,]

  scaledGate = cbind(applyScale(scaleX, gate[,xChannel]), applyScale(scaleY, gate[,yChannel]))
  colnames(scaledGate) <- c(xChannel, yChannel)
  gate[,xChannel] <- unlist(scaledGate[,xChannel])
  gate[,yChannel] <- unlist(scaledGate[,yChannel])

  return(lapply(seq_len(nrow(gate)), function(i) gate[i,]))
}
