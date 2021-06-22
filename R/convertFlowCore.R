library("stats")

#' Convert a gate to flowCore
#'
#' Converts a CellEngine gate ("PolygonGate", "EllipseGate", or "RectangleGate") to
#' its flowCore analogue.
#'
#' @param gate The CellEngine gate to be converted
#' @export
convertToFlowCore <- function (gate) {
  if (!requireNamespace("flowCore")) {
    stop("These utilities require the 'flowCore' package.")
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
      x <- unlist(ellipse$center)[1]
      y <- unlist(ellipse$center)[2]
      points <- getEllipsePoints(ellipse$angle, ellipse$major, ellipse$minor, x, y)
      points <- t(data.frame(points))

      result <- fitEllipsePoints(points)

      cov <- result$covar
      colnames(cov) <- c(gate$xChannel, gate$yChannel)
      rownames(cov) <- c(gate$xChannel, gate$yChannel)

      return(flowCore::ellipsoidGate(filterId = gate$name, .gate = cov, mean = c(result$x, result$y)))
    },
    "PolygonGate" = {
      m <- gate$model$polygon$vertices
      if (is.null(dim(m))) {
        m <- m[[1]]
      }
      colnames(m) <- c(gate$xChannel, gate$yChannel)
      flowCore::polygonGate(filterId = gate$name, m)
    },
    "QuadrantGate" = stop("This gate has no representation in flowCore"),
    "SplitGate" = stop("This gate has no representation in flowCore"),
    "RangeGate" = stop("This function is not yet implemented."),
  )
}

#' Scale a flowCore gate or flowDensity filter
#'
#' Applies a scale to points from a flowCore gate, so that it may
#' be saved to CellEngine.
#'
#' @param gate The flowCore gate points (i.e. polygonGate@boundaries)
#' @param scaleSet The CellEngine ScaleSet to apply.
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
  scales <- data.frame(scaleSet$scales)
  ss <- scales$scale
  rownames(ss) <- scales$channelName

  scaleX <- ss[xChannel,]
  scaleY <- ss[yChannel,]

  scaledGate = cbind(applyScale(scaleX, gate[,xChannel]), applyScale(scaleY, gate[,yChannel]))
  colnames(scaledGate) <- c(xChannel, yChannel)
  gate[,xChannel] <- unlist(scaledGate[,xChannel])
  gate[,yChannel] <- unlist(scaledGate[,yChannel])

  return(lapply(seq_len(nrow(gate)), function(i) gate[i,]))
}

scaleEllipseCenter <- function (center, scaleSet) {
  xChannel <- names(center)[1]
  yChannel <- names(center)[2]
  scales <- data.frame(scaleSet$scales)
  s <- scales$scale
  rownames(s) <- scales$channelName

  scaleX <- s[xChannel,]
  scaleY <- s[yChannel,]

  scaledCenter = cbind(applyScale(scaleX, center[xChannel]), applyScale(scaleY, center[yChannel]))
  names(scaledCenter) <- c(xChannel, yChannel)
  center[xChannel] <- unlist(scaledCenter[xChannel])
  center[yChannel] <- unlist(scaledCenter[yChannel])

  return(center)
}

#' Convert flowCore to gate
#'
#' Converts points in a flowCore gate or flowDensity filter to a CellEngine
#' Gate and saves it to CellEngine. A ScaleSet must be provided to scale the
#' gate correctly.
#'
#' @param flowObject A flowDensity object or a flowCore Gate
#' @param scaleSet The CellEngine ScaleSet to be applied.
#' @export
#' @examples
#' \dontrun{
#' sngl <- flowDensity(file, params)
#' scaleSet <- getScaleSets("5d2f8b4b21fd0676fb3a6a8c")
#' convertFromFlowCore(sngl, scaleSet, name = "my gate")
#' }
convertFromFlowCore <- function (flowObject, scaleSet, name=NULL) {
  if (is.null(name)) {
    name <- flowObject@filterId
  }

  switch(
    class(flowObject)[1],
    "CellPopulation" = { # flowDensity@filter is always a convex polygon
      flowGate <- flowObject@filter
      createPolygonGate(
        scaleSet$experimentId,
        colnames(flowGate)[1],
        colnames(flowGate)[2],
        name,
        vertices = list(flowGate)
      )
    },
    "ellipsoidGate" = {
      params <- covarToParameters(flowObject@cov)
      x <- flowObject@mean[[1]]
      y <- flowObject@mean[[2]]

      createEllipseGate(
        scaleSet$experimentId,
        colnames(flowObject@cov)[1],
        colnames(flowObject@cov)[2],
        name,
        x,
        y,
        params$angle,
        params$major,
        params$minor
      )
    },
    "polygonGate" = {
      flowGate <- flowObject@boundaries
      createPolygonGate(
        scaleSet$experimentId,
        colnames(flowGate)[1],
        colnames(flowGate)[2],
        name,
        vertices = list(flowGate)
      )
    },
    "rectangleGate" = {
      createRectangleGate(
        scaleSet$experimentId,
        names(flowObject@min)[1],
        names(flowObject@min)[2],
        name,
        flowObject@min[[1]],
        flowObject@max[[1]],
        flowObject@min[[2]],
        flowObject@max[[2]]
      )
    }
  )
}
