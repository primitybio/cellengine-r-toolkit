#' Update gate
#'
#' Updates a gate.
#'
#' @param experimentId ID of experiment.
#' @param gateId gate ID
#' @param properties Properties to set on the gate.
#' @param params Optional query parameters.
#' @export
#' @examples
#' \dontrun{
#' updateGate(experimentId, gateId, list("name" = "new gate name"))
#' }

updateGate = function(experimentId, gateId, properties = list(), params = list()) {
  checkDefined(experimentId)
  checkDefined(gateId)
  experimentId = lookupByName("experiments", experimentId)
  body = jsonlite::toJSON(properties, null = "null")
  basePatch(paste("experiments", experimentId, "gates", gateId, sep = "/"), body, params)
}
