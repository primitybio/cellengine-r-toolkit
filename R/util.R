pkg.env = new.env()

pkg.env$baseURL = ""

handleResponse = function(response) {
  httr::warn_for_status(response)
  content = httr::content(response, "text", encoding = "UTF-8")
  return(jsonlite::fromJSON(content))
}

ua = (function (){
  versions <- c(
    `CellEngine R API Toolkit` = "0.1.0", # TODO see if utils::packageVersion works
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
