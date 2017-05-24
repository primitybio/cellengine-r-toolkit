#' Auto Delete Population
#'
#' Deletes a population, its corresponding gates, and all descendant gates and
#' populations.
#'
#' @param experimentId ID of experiment.
#' @param populationId ID of population.
#' @export
#' @examples
#' \dontrun{
#' deletePopulationAuto(experimentId, populationId)
#' }
deletePopulationAuto = function(experimentId, populationId) {
  baseDelete(
    paste("experiments", experimentId, "populations", populationId, sep = "/"),
    params = list(deleteBranch = "true")
  )
}
