#' Authenticate
#'
#' Authenticates with the server. Call \code{\link{setServer}} before this
#' function.
#'
#' For security reasons, you should not store your password in plain text, and
#' you should ensure that your R history does not store your password. Instead,
#' consider setting an environment variable and using something like
#' \code{authenticate("myusername", Sys.getenv("API_PASSWORD"))}
#'
#' @param username Your username.
#' @param password Your password.
#' @export
#' @examples
#' \dontrun{
#' authenticate("username", "password")
#' }
authenticate = function(username, password) {
  body = jsonlite::toJSON(list(
    username = jsonlite::unbox(username),
    password = jsonlite::unbox(password)
  ))
  return(invisible(basePost("signin", body, list())))
}
