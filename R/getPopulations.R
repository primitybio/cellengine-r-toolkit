#' Get populations
#'
#' Retrieves the list of populations in an experiment.
#'
#' @param experimentId ID of experiment.
#' @param params Optional query parameters.
#' @export
#' @examples
#' \dontrun{
#' # List all populations in the experiment
#' getPopulations(experimentId)
#'
#' # List the names of the first five populations
#' getPopulations(experimentId, params = list("limit" = "5", "fields" = "+name"))
#' }
getPopulations = function(experimentId, params = list()) {
  checkDefined(experimentId)
  experimentId = lookupByName("experiments", experimentId)
  baseGet(paste("experiments", experimentId, "populations", sep = "/"), params)
}
