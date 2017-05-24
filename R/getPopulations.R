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
  baseGet(paste("experiments", experimentId, "populations", sep = "/"), params)
}

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
  baseGet(paste("experiments", experimentId, "populations", populationId, sep = "/"), params)
}
