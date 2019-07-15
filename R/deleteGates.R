#' Delete gate
#'
#' Deletes a gate or a tailored gate family
#' Works for compound gates if you specify the top-level gid. Specifying the gid of a sector (i.e. one listed in model.gids) will result in no gates being deleted.
#' If gateID is specified, only that gate will be deleted, regardless of the other parameters specified.
#'
#' Currently, byName does not work for the gateId in this function (it does work for the experimentID).
#' @param experimentId ID of experiment.
#' @param gid ID of gate family.
#' @param gateId ID of gate.
#' @param exclude Gate ID to exclude from deletion.
#' @export
#' @examples
#' \dontrun{
#' deleteGate(experimentId, gid = [gate family ID])
#' deleteGate(experimentId, gateId = [gate ID])
#' }

deleteGates = function(experimentId, gid = NULL, gateId = NULL, exclude = NULL) {
  checkDefined(experimentId)
  experimentId = lookupByName("experiments", experimentId)
  #TODO:(ge) Add byName functionality for gateId
  
  if ((is.null(gid) & is.null(gateId)) | (!is.null(gid) & !is.null(gateId))){
      stop("Either the gid or the gateId must be specified")
  }

  if (!is.null(gateId)){
    checkDefined(gateId)
  } else if (!is.null(gid)) {
    base = paste("experiments", experimentId, "gates", sep = "/")
    url = sprintf("%s?gid=%s", base, gid)
    if (!is.null(exclude)) {
        url = sprintf("$s&exclude=%s", url, exclude)
    }
    baseDelete(url)
  }
}

