#' Get experiment
#'
#' Retrieves an experiment.
#'
#' @param experimentId ID of experiment.
#' @param params Optional query parameters.
#' @export
#' @examples
#' \dontrun{
#' # Retrieve by ID
#' getExperiment(experimentId)
#'
#' # Lookup by name
#' getExperiment(byName("my experiment"))
#' }
getExperiment = function(experimentId, params = list()) {
  checkDefined(experimentId)
  experimentId = lookupByName("experiments", experimentId)
  baseGet(paste("experiments", experimentId, sep = "/"), params)
}
