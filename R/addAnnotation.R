#' Append Annotations to FCS file
#'
#' Append new annotations to those existing on an FCS file.
#'
#' This function adds new annotations without overwriting the existing
#' annotations. To add new annotations without keeping the existing annotations,
#' use \code{annotateFcsFile}.
#'
#' @param experimentId ID of experiment.
#' @param fcsFileId ID of FCS file.
#' @param annotations List of annotations
#' @export
#' @examples
#' \dontrun{
#' annotations = list("annotations 1" = 1, "annotation 2" = "value")
#' addAnnotation(experimentId, fcsFileId, annotations)
#' }
addAnnotation = function(experimentId, fcsFileId, annotations) {
  checkDefined(experimentId)
  experimentId = lookupByName("experiments", experimentId)
  checkDefined(fcsFileId)
  fcsFileId = lookupByName(paste("experiments", experimentId, "fcsfiles", sep = "/"), fcsFileId, "filename")

  oldAnnotations = baseGet(paste("experiments", experimentId, "fcsfiles", fcsFileId, sep = "/"))$annotations

  annoList = as.list(with(data.frame(oldAnnotations), setNames(value, name)))
  annotations = append(annoList, annotations)

  body = jsonlite::toJSON(list("annotations" =
    mapply(function (k,v) {
      list("name" = jsonlite::unbox(k), "value" = jsonlite::unbox(v))
    }, names(annotations), annotations, SIMPLIFY = FALSE, USE.NAMES = FALSE)
  ))

  basePatch(paste("experiments", experimentId, "fcsfiles", fcsFileId, sep = "/"), body)
}
