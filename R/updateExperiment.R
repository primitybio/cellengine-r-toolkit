#' Update experiment
#'
#' Updates an experiment.
#'
#' @param experimentId ID of experiment.
#' @param properties Properties to set on the experiment.
#' @param params Optional query parameters.
#' @export
#' @examples
#' \dontrun{
#' updateExperiment(experimentId, list("name" = "my experiment"))
#' }
updateExperiment = function(experimentId, properties = list(), params = list()) {
  checkDefined(experimentId)
  experimentId = lookupByName("experiments", experimentId)
  body = jsonlite::toJSON(body, null = "null")
  basePatch(paste("experiments", experimentId, sep = "/"), body, params)
}
