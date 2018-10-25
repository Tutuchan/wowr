#' get dungeons names and ids
#'
#' @template dots
#'
#' @return a data.table with the following columns:
#' + id,
#' + name,
#' + locale
#' @export
wow_dungeons <- function(...) {
  # TODO rework with make_get and infer_locale
  dots <- augment_query(...)

  endpoint <- "data/wow/connected-realm/509/mythic-leaderboard/"

  response <- make_get(
    endpoint = endpoint,
    token = dots$token,
    region = dots$region,
    namespace = make_dynamic_namespace(dots$region),
    locale = dots$locale
  )
  content <- content(response)

  multi_locale <- length(content$current_leaderboards[[1]]$name) > 1

  lapply(content$current_leaderboards, function(x) {
    if (multi_locale) {
      lc <- names(x$name)
      nm <- unlist(unname(x$name))
    } else {
      lc <- dots$locale
      nm <- x$name
    }
    data.table(
      id = x$id,
      name = nm,
      locale = lc
    )
  }) %>%
    Reduce(rbind, .)
}
