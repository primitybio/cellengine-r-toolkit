#' Get population
#'
#' Retrieves a population by ID
#'
#' @param experimentId ID of experiment.
#' @param populationId ID of the population.
#' @param params Optional query parameters.
#' @export
#' @examples
#' \dontrun{
#' getPopulation(experimentId, populationId)
#' getPopulation(experimentId, populationId, params = list("fields" = "+name"))
#' }
getPopulation = function(experimentId, populationId, params = list()) {
  checkDefined(experimentId)
  checkDefined(populationId)
  baseGet(paste("experiments", experimentId, "populations", populationId, sep = "/"), params)
}
