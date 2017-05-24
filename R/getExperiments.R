#' Get experiments
#'
#' Retrieves the list of experiments to which you have access.
#'
#' @param params Optional query parameters.
#' @export
#' @examples
#' \dontrun{
#' getExperiments()
#' getExperiments(params = list("limit" = "5"))
#' }
getExperiments = function(params = list()) {
  baseGet(paste("experiments", sep = "/"), params)
}
