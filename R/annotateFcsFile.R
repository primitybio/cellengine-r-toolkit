#' Annotate FCS file
#'
#' Sets the annotations on an FCS file.
#'
#' This function overwrites any existing annotations. To add new annotations
#' while keeping the existing annotations, first use \code{getFcsFile} to get
#' the existing annotations.
#'
#' @param experimentId ID of experiment.
#' @param fcsFileId ID of FCS file.
#' @param annotations List of annotations
#' @export
#' @examples
#' \dontrun{
#' annotations = list("annotations 1" = 1, "annotation 2" = "value")
#' annotateFcsFile(experimentId, fcsFileId, annotations)
#' }
annotateFcsFile = function(experimentId, fcsFileId, annotations) {
  checkDefined(experimentId)
  experimentId = lookupByName("experiments", experimentId)
  checkDefined(fcsFileId)
  fcsFileId = lookupByName(paste("experiments", experimentId, "fcsfiles", sep = "/"), fcsFileId)
  body = jsonlite::toJSON(list("annotations" =
    mapply(function (k,v) {
      list("name" = jsonlite::unbox(k), "value" = jsonlite::unbox(v))
    }, names(annotations), annotations, SIMPLIFY = FALSE, USE.NAMES = FALSE)
  ))

  basePut(paste("experiments", experimentId, "fcsfiles", fcsFileId, sep = "/"), body)
}
