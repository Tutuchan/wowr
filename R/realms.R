#' retrieve connected realms ids
#'
#' @template dots
#'
#' @export
wow_connected_realms <- function(...) {
  dots <- augment_query(...)
  endpoint <- "data/wow/connected-realm/"

  response <- make_get(
    endpoint = endpoint,
    token = dots$token,
    region = dots$region,
    namespace = make_dynamic_namespace(dots$region),
    locale = dots$locale
  )

  content <- content(response)

  # extract ids
  ids <- vapply(
    X   = content$connected_realms,
    FUN = function(x) {
      idx <- regexpr("[[:digit:]]{3,4}", x$href)
      as.numeric(substr(x$href, idx, idx + attr(idx, "match.length") - 1))
    },
    FUN.VALUE = numeric(1)
  )

  data.table(id = ids)

}

#' retrieve info on a specific connected realm
#'
#' @param realm_id an integer, see [wow_connected_realms()] for a list
#' @template dots
#'
#' @export
wow_connected_realm <- function(realm_id, ...) {

  dots <- augment_query(...)
  endpoint <- paste0("data/wow/connected-realm/", realm_id)

  response <- make_get(
    endpoint = endpoint,
    token = dots$token,
    region = dots$region,
    namespace = make_dynamic_namespace(dots$region),
    locale = dots$locale
  )

  content <- content(response)

  # status of the realms
  status_locale     <- infer_locale(content$status$name)
  population_locale <- infer_locale(content$population$name)
  status <- data.table(
    id = content$id,
    locale = status_locale$locale,
    status = status_locale$y,
    population = population_locale$y
  )

  # realms info
  n_realms <- length(content$realms)
  realms <- lapply(content$realms, function(realm) {
    data.table(
      region     = unname(unlist(realm$region$name)),
      name       = unname(unlist(realm$name)),
      category   = unname(unlist(realm$category)),
      type       = unname(unlist(realm$type$name)),
      slug       = realm$slug,
      tournament = realm$is_tournament
    )
  }) %>%
    Reduce(rbind, .)

  # duplicate status information for all realms
  duplicate <- rep(1:nrow(status), n_realms)
  cbind(
    status[duplicate,],
    realms
  ) %>%
    as.data.table()
}

#' retrieve realms ids
#'
#' @template dots
#'
#' @export
wow_realms <- function(...) {

  res <- make_get_with_dynamic_namespace(endpoint = "data/wow/realm/", ...)
  dots    <- res$dots
  content <- res$content

  lapply(content$realms, parse_realm, dots = dots) %>%
    Reduce(rbind, .) %>%
    as.data.table()

}

parse_realm <- function(realm, dots) {

  name <- infer_locale(realm$name, dots)

  data.table(
    id = realm$id,
    slug = realm$slug,
    locale = name$locale,
    name   = name$y
  )

}
