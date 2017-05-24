#' Get FCS files
#'
#' Retrieves the list of FCS files in an experiment. This does not download the
#' FCS files themselves; this only returns information about the FCS files.
#'
#' @param experimentId ID of experiment.
#' @param params Optional query parameters.
#' @export
#' @examples
#' \dontrun{
#' getFcsFiles(experimentId)
#' getFcsFiles(experimentId, params = list("limit" = "5"))
#' }
getFcsFiles = function(experimentId, params = list()) {
  baseGet(paste("experiments", experimentId, "fcsfiles", sep = "/"), params)
}
