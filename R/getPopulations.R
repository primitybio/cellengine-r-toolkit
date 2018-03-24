#' Get populations
#'
#' Retrieves the list of populations in an experiment.
#'
#' @param experimentId ID of experiment.
#' @param params Optional query parameters.
#' @export
#' @examples
#' \dontrun{
#' getPopulations(experimentId)
#' getPopulations(experimentId, params = list("limit" = "5"))
#' }
getPopulations = function(experimentId, params = list()) {
  checkDefined(experimentId)
  experimentId = lookupByName("experiments", experimentId)
  baseGet(paste("experiments", experimentId, "populations", sep = "/"), params)
}
