#' Delete FCS file
#'
#' Deletes an FCS file.
#'
#' @param experimentId ID of experiment.
#' @param fcsFileId ID of FCS file.
#' @export
#' @examples
#' \dontrun{
#' deleteFcsFile(experimentId, fcsFileId)
#' }
deleteFcsFile = function(experimentId, fcsFileId) {
  checkDefined(experimentId)
  checkDefined(fcsFileId)
  baseDelete(paste("experiments", experimentId, "fcsfiles", fcsFileId, sep = "/"))
}
