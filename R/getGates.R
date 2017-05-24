#' Get gates
#'
#' Retrieves the list of gates in an experiment.
#'
#' @param experimentId ID of experiment.
#' @param params Optional query parameters.
#' @export
#' @examples
#' \dontrun{
#' getGates(experimentId)
#' getGates(experimentId, params = list("limit" = "5"))
#' }
getGates = function(experimentId, params = list()) {
  baseGet(paste("experiments", experimentId, "gates", sep = "/"), params)
}
