#' Get scale sets
#'
#' Retrieves the list of scale sets in an experiment. Currently each experiment
#' has exactly one scale set.
#'
#' @param experimentId ID of experiment.
#' @param params Optional query parameters.
#' @export
#' @examples
#' \dontrun{
#' getScaleSets(experimentId)
#' }
getScaleSets = function(experimentId, params = list()) {
  checkDefined(experimentId)
  experimentId = lookupByName("experiments", experimentId)
  scaleSet = baseGet(paste("experiments", experimentId, "scalesets", sep = "/"), params)
  scales = data.frame(scaleSet$scales)
  simpleScales = scales$scale
  rownames(simpleScales) <- scales$channelName
  scaleSet$scales = simpleScales
  scaleSet
}
