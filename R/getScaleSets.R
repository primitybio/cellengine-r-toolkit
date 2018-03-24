#' Get scale sets
#'
#' Retrieves the list of scale sets in an experiment.
#'
#' @param experimentId ID of experiment.
#' @param params Optional query parameters.
#' @export
#' @examples
#' \dontrun{
#' getScaleSets(experimentId)
#' getScaleSets(experimentId, params = list("limit" = "5"))
#' }
getScaleSets = function(experimentId, params = list()) {
  checkDefined(experimentId)
  baseGet(paste("experiments", experimentId, "scalesets", sep = "/"), params)
}
