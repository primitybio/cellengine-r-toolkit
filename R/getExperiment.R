#' Get experiment
#'
#' Retrieves an experiment.
#'
#' @param experimentId ID of experiment.
#' @param params Optional query parameters.
#' @export
#' @examples
#' \dontrun{
#' getExperiment(experimentId)
#' getExperiment(experimentId, params = list("fields" = "+name"))
#' }
getExperiment = function(experimentId, params = list()) {
  checkDefined(experimentId)
  baseGet(paste("experiments", experimentId, sep = "/"), params)
}
