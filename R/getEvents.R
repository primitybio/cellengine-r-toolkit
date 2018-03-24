#' Get FCS File Events
#'
#' Retrieves events from an FCS file or gated population as either an FCS or
#' TSV file.
#'
#' Tip: FCS files are more compact than TSV files. Use that format for faster
#' downloads. Use a library such as flowCore to parse FCS files.
#'
#' @param experimentId ID of experiment.
#' @param fcsFileId ID of FCS file.
#' @param populationId Optional ID of population, for gated events.
#' @param scaleSetId Optional ID of scale set. If omitted and if a single
#'   scale set exists in the experiment, that scale set will be used.
#' @param compensation ID of compensation or special compensation type
#'   (\code{UNCOMPENSATED} or \code{FILE_INTERNAL}).
#' @param compensatedQ If \code{TRUE}, applies the compensation specified in
#'   compensation to the exported events. For TSV format, the numerical values
#'   will be the compensated values. For FCS format, the numerical values will
#'   be unchanged, but the file header will contain the compensation as the
#'   spill string (file-internal compensation).
#' @param headerQ for TSV format only: if true, file will contain a header row.
#' @param format One of "TSV" or "FCS".
#' @param destination Optional, if specified, write the file to the specified
#'   path instead of returning it as a binary blob.
#' @param overwrite Optional, if a destination is specified, allows destination
#'   file to be overwritten.
#' @export
#' @examples
#' \dontrun{
#' getEvents(experimentId, fcsFileId)
#' getEvents(experimentId, fcsFileId, populationId, format = "TSV")
#' getEvents(experimentId, fcsFileId, destination = "/path/to/output.fcs")
#' }
getEvents = function(experimentId,
                     fcsFileId,
                     populationId = NULL,
                     scaleSetId = NULL,
                     compensation = UNCOMPENSATED,
                     compensatedQ = FALSE,
                     headerQ = FALSE,
                     format = "FCS",
                     destination = NULL,
                     overwrite = FALSE) {

  checkDefined(experimentId)
  checkDefined(fcsFileId)

  # scale set argument
  # TODO dedupe
  if (is.null(scaleSetId) && !is.null(populationId)) {
    serverScaleSets = getScaleSets(experimentId, params = list(fields = "+_id"))
    if (!is.data.frame(serverScaleSets)) { # zero-length results are not data.frames
      stop("No scalesets found in experiment.")
    }
    if (nrow(serverScaleSets) > 1) {
      stop("More than one scaleset exists in experiment. Please specify a scaleSetId to select one.")
    }
    scaleSetId = serverScaleSets$`_id`
  }

  ensureBaseUrl()

  fullURL = paste(paste(pkg.env$baseURL, "experiments", experimentId, "fcsfiles", fcsFileId, sep = "/"), format, sep = ".")

  params = list(
    populationId = populationId,
    scaleSetId = scaleSetId,
    compensationId = compensation,
    compensatedQ = compensatedQ,
    headers = headerQ,
    format = format
  )

  if (is.null(destination)) {
    response = httr::GET(fullURL, query = params, httr::user_agent(ua))
    httr::warn_for_status(response)
    content = httr::content(response, "raw")
  } else {
    response = httr::GET(fullURL, query = params, httr::user_agent(ua), httr::write_disk(destination, overwrite))
    httr::warn_for_status(response)
  }
}
