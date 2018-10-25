#' authenticate to the Blizzard portal
#'
#' @param client_id a character, the application client id
#' @param client_secret a character, the application client secret id
#' @param region a character, the region to authenticate with
#'
#' @export
wow_auth <- function(client_id = NULL, client_secret = NULL, region = NULL) {
  if (is.null(client_id)) {
    client_id <- Sys.getenv("BLIZZARD_CLIENT_ID")
    if (client_id == "") {
      stop("Blizzard client id cannot be empty! Either specify it or set the BLIZZARD_CLIENT_ID environment variable.")
    }
  }

  if (is.null(client_secret)) {
    client_secret <- Sys.getenv("BLIZZARD_CLIENT_SECRET")
    if (client_secret == "") {
      stop("Blizzard client secret cannot be empty! Either specify it or set the BLIZZARD_CLIENT_SECRET environment variable.")
    }
  }

  if (is.null(region)) {
    region <- Sys.getenv("BLIZZARD_CLIENT_REGION")
    if (region == "") {
      stop("Blizzard client region cannot be empty! Either specify it or set the BLIZZARD_CLIENT_REGION environment variable.")
    }
  } else {
    validate_region(region)
    Sys.setenv(BLIZZARD_CLIENT_REGION = tolower(region))
  }

  endpoint <- oauth_endpoint(authorize = NULL, access = "https://us.battle.net/oauth/token", validate = "https://us.battle.net/oauth/check_token")
  app      <- oauth_app(appname = "wowr", key = client_id, secret = client_secret)
  token    <- oauth2.0_token(endpoint = endpoint, app = app, client_credentials = TRUE)

  if (!validate_token(token)) {
    cat("Cached token invalid, generating a new one ...\n")
    file.remove(token$cache_path)
    token    <- oauth2.0_token(endpoint = endpoint, app = app, client_credentials = TRUE)
  }

  token
}

#' validate a wowr token
#'
#' The `validate()` function in `httr` has a hard-coded *access_token* parameter to check for validity while the Blizzard API
#' uses *token*, hence this custom function.
#'
#' @param token an [httr::oauth2.0_token()]
validate_token <- function(token) {
  url <- modify_url(
    url = token$endpoint$validate,
    query = list(token = token$credentials$access_token)
  )

  status_code(GET(url)) == 200
}
