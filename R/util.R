pkg.env = new.env()

pkg.env$baseURL = "https://cellengine.com/api/v1"

handleResponse = function(response) {
  httr::warn_for_status(response)
  content = httr::content(response, "text", encoding = "UTF-8")
  return(jsonlite::fromJSON(content))
}

ua = (function (){
  versions <- c(
    `CellEngine API Toolkit` = "0.1.0", # TODO see if utils::packageVersion works
    libcurl = curl::curl_version()$version,
    `r-curl` = as.character(utils::packageVersion("curl")),
    httr = as.character(utils::packageVersion("httr")))
  paste0(names(versions), "/", versions, collapse = " ")
})()

ensureBaseUrl = function() {
  if (pkg.env$baseURL == "") stop("Please call setServer(host) first.")
}

baseGet = function(url, params = list()) {
  ensureBaseUrl()
  fullURL = paste(pkg.env$baseURL, url, sep = "/")
  handleResponse(httr::GET(fullURL, query = params, httr::user_agent(ua)))
}

basePut = function(url, body, params = list()) {
  ensureBaseUrl()
  fullURL = paste(pkg.env$baseURL, url, sep = "/")
  handleResponse(httr::PUT(fullURL, body = body, query = params, httr::content_type_json(), httr::user_agent(ua)))
}

basePost = function(url, body, params = list()) {
  ensureBaseUrl()
  fullURL = paste(pkg.env$baseURL, url, sep = "/")
  handleResponse(httr::POST(fullURL, body = body, query = params, httr::content_type_json(), httr::user_agent(ua)))
}

baseDelete = function(url, params = list()) {
  ensureBaseUrl()
  fullURL = paste(pkg.env$baseURL, url, sep = "/")
  response = httr::DELETE(fullURL, query = params, httr::user_agent(ua))
  httr::warn_for_status(response)
}

checkDefined = function(param) {
  if (is.null(param)) {
    stop(paste0("parameter '", deparse(substitute(param)), "' is NULL"))
  }
}

#' No compensation.
#'
#' A constant representing no compensation.
#'
#' @export
UNCOMPENSATED = 0

#' File-internal compensation.
#'
#' A constant representing file-internal compensation.
#'
#' @export
FILE_INTERNAL = -1

#' Ungated.
#'
#' A constant representing the ungated population.
#'
#' @export
UNGATED = ""

#' Returns more information about the last error.
#'
#' Some functions in this package will store additional error information so
#' that it is retrievable using \code{getErrorInfo()}.
#'
#' @export
getErrorInfo = function() {
  pkg.env$lastError
}

#' Resource By Name
#'
#' Allows specifying a resource by name instead of by id, anywhere that an id is
#' accepted.
#'
#' Internally, this looks up the resource's id by name before the function runs.
#' To improve performance, the resource's id is cached for the duration of the R
#' session. Resources such as gates that exist within an experiment are cached
#' within the experiment's scope. That is, the following is safe, even though
#' the FCS files have the same name:
#'
#' \code{
#' getFcsFile(byName("experiment 1"), byName("fcsfile1.fcs"))
#' getFcsFile(byName("experiment 2"), byName("fcsfile1.fcs"))
#' }
#'
#' @param name Name of resource.
#' @examples
#' \dontrun{
#' getGates(byName("my experiment"))
#' getGates(experimentId = byName("my experiment"))
#' }
#' @export
byName = function(name) {
  class(name) <- "_boxed_by_name"
  name
}

byNameHash = new.env(hash = TRUE, parent = emptyenv())

lookupByName = function(listpath, name) {
  if (class(name) != "_boxed_by_name") return(name)

  name = unclass(name)
  key = paste(listpath, name, sep="$$$")
  if (exists(key, envir = byNameHash)) return(get(key, envir = byNameHash))

  vals = baseGet(listpath, params = list(
    query = sprintf("eq(name, \"%s\")", name),
    limit = 2 # need >1 so we can detect ambiguous matches
  ))
  if (!is.data.frame(vals)) {
    stop(sprintf("Resource with the name '%s' does not exist in the experiment.", name))
  }
  if (nrow(vals) > 1) {
    stop(sprintf("More than one resource with the name '%s' exists in the experiment.", name))
  }

  val = vals$`_id`
  assign(key, val, envir = byNameHash)
  val
}
