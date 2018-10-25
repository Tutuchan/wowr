#' build a GET request
#'
#' @param endpoint a character,
#' @param token a [httr::oauth2.0_token()]
#' @param region a character, one of the available regions
#' @param namespace a character (optional)
#' @param locale a character (optional)
#'
#' @keywords internal
make_get <- function(endpoint, token, region, namespace = NULL, locale = NULL) {
  base_url <- region_base_url(region)
  url <- add_endpoint(base_url, endpoint = endpoint) %>%
    add_locale(locale)

  args <- list(
    url = url,
    config = config(token = token)
  )

  args <- add_namespace(args, namespace)

  response <- do.call(what = GET, args = args)
  response
}

#' build a GET request without a namespace
#'
#' @param endpoint a character
#' @template dots
#'
#' @keywords internal
make_get_without_namespace <- function(endpoint, ...) {
  dots <- augment_query(...)

  response <- make_get(
    endpoint = endpoint,
    token = dots$token,
    region = dots$region,
    locale = dots$locale
  )
  content <- content(response)

  list(
    response = response,
    content = content,
    dots    = dots
  )
}

#' build a GET request with a namespace
#'
#' @param endpoint a character
#' @param namespace a character
#' @template dots
#'
#' @keywords internal
make_get_with_namespace <- function(endpoint, namespace, ...) {
  dots <- augment_query(...)
  namespace <- make_namespace(type = namespace, region = dots$region)

  response <- make_get(
    endpoint = endpoint,
    token = dots$token,
    region = dots$region,
    namespace = namespace,
    locale = dots$locale
  )
  content <- content(response)

  list(
    content = content,
    dots    = dots
  )
}

#' build a GET request with a static namespace
#'
#' @param endpoint a character
#' @template dots
#'
#' @keywords internal
make_get_with_static_namespace <- function(endpoint, ...) {
  make_get_with_namespace(endpoint, "static", ...)
}

#' build a GET request with a dynamic namespace
#'
#' @param endpoint a character
#' @template dots
#'
#' @keywords internal
make_get_with_dynamic_namespace <- function(endpoint, ...) {
  make_get_with_namespace(endpoint, "dynamic", ...)
}


#' build the base URL for a region
#'
#' @param region a character
#'
#' @keywords internal
region_base_url <- function(region) {

  region <- tolower(region)
  validate_region(region)

  base_url <- "api.blizzard.com"
  if (region == "cn") {
    url <- paste(base_url, region, sep = ".")
  } else {
    url <- paste(region, base_url, sep = ".")
  }

  modify_url(url = url, scheme = "https")
}

#' add an endpoint to an URL
#'
#' @param url a character
#' @param endpoint a character
#'
#' @keywords internal
add_endpoint <- function(url, endpoint) {
  if (is.null(endpoint)) {
    return(url)
  }

  parsed <- parse_url(url)
  parsed$path <- paste(parsed$path, endpoint, sep = "/")
  build_url(parsed)
}


#' add a locale to an URL
#'
#' @param url a character
#' @param locale a character
#'
#' @keywords internal
add_locale <- function(url, locale) {
  if (is.null(locale)) {
    return(url)
  }
  modify_url(url, query = list("locale" = locale))
}

#' add a namespace to an URL
#'
#' The Blizzard API uses namespaces for some endpoints, see
#' [the doc](https://develop.battle.net/documentation/guides/game-data-apis-wow-namespaces)
#' for more context.
#'
#' @param url a character
#' @param namespace a character
#'
#' @keywords internal
add_namespace <- function(args, namespace) {
  if (!is.null(namespace)) {
    args <- c(args, list(add_headers(`Battlenet-Namespace` = namespace)))
  }
  args
}

