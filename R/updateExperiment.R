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
  body = jsonlite::toJSON(body, null = "null")
  basePut(paste("experiments", experimentId, sep = "/"), body, params)
}
