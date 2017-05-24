#' Delete experiment
#'
#' Deletes an experiment.
#'
#' @param experimentId ID of experiment.
#' @export
#' @examples
#' \dontrun{
#' deleteExperiment(experimentId)
#' }
deleteExperiment = function(experimentId) {
  baseDelete(paste("experiments", experimentId, sep = "/"))
}
