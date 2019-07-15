#' Update gate family
#'
#' Updates a gate family.
#'
#' @param experimentId ID of experiment.
#' @param gid Gate family ID
#' @param properties Properties to set on the gate.
#' @param params Optional query parameters.
#' @export
#' @examples
#' \dontrun{
#' updateGateFamily(experimentId, gid, list("name" = "new gate name"))
#' }

updateGateFamily = function(experimentId, gid, properties = list(), params = list()) {
  checkDefined(experimentId)
  checkDefined(gid)
  experimentId = lookupByName("experiments", experimentId)
  body = jsonlite::toJSON(properties, null = "null")
  base = paste("experiments", experimentId, "gates", sep = "/")
  url = sprintf("%s?gid=%s", base, gid)
  print(url)
  basePatch(url, body, params)
}
