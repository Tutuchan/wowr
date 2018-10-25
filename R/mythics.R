#' get affixes for the current week
#'
#' @template dots
#'
#' @return a data.table with the following columns
#' + period: integer, the current period
#' + start: POSIXct, start of the current period
#' + end: POSIXct, end of the current period
#' + locale: character, the name of the locale
#' + affix: character, the name of the affix in the current locale
#' + level: integer, at which M+ level this affix starts being present
#' @export
wow_mythic_challenge <- function(...) {

  dots <- augment_query(...)

  response <- make_get(
    endpoint = "data/wow/mythic-challenge-mode/",
    token = dots$token,
    region = dots$region,
    namespace = make_dynamic_namespace(dots$region),
    locale = dots$locale
  )
  content <- content(response)

  affixes <- content$current_keystone_affixes %>%
    lapply(function(afx) {
      res <- infer_locale(afx$keystone_affix$name, dots)
      locale <- res$locale
      affix  <- res$y

      data.table(
        locale = locale,
        affix  = affix,
        level  = afx$starting_level
      )
    }) %>%
    Reduce(rbind, .)

  period <- data.table(
    period = rep(content$current_period, nrow(affixes)),
    start  = rep(to_posix(content$current_period_start_timestamp), nrow(affixes)),
    end    = rep(to_posix(content$current_period_end_timestamp), nrow(affixes))
  )

  cbind(period, affixes) %>%
    as.data.table()

}

#' get the mythic leaderboard for a realm, dungeon and period
#'
#' @param realm_id an integer, see [wow_realms()]
#' @param dungeon_id an integer, see [wow_dungeons()]
#' @param period an integer
#' @template dots
#'
#' @export
wow_mythic_leaderboard <- function(realm_id, dungeon_id, period, ...) {

  dots <- augment_query(...)

  endpoint <- glue("data/wow/connected-realm/{realm_id}/mythic-leaderboard/{dungeon_id}/period/{period}")

  response <- make_get(
    endpoint = endpoint,
    token = dots$token,
    region = dots$region,
    namespace = make_dynamic_namespace(dots$region),
    locale = dots$locale
  )
  content <- content(response)

  res <- infer_locale(content$map$name, dots)
  locale  <- res$locale
  dungeon <- res$y

  dungeon <- data.table(
    realm_id  = realm_id,
    mythic_id = content$map_challenge_mode_id,
    name      = dungeon,
    locale    = locale,
    period    = content$period,
    start     = to_posix(content$period_start_timestamp),
    end       = to_posix(content$period_end_timestamp)
  )

  groups <- lapply(content$leading_groups, function(group) {
    members <- lapply(group$members, parse_group_member) %>%
      Reduce(rbind, .)
    stats <- data.table(
      ranking   = group$ranking,
      key_level = group$keystone_level,
      duration  = group$duration / 1000,
      completed = to_posix(group$completed_timestamp)
    )

    cbind(stats, members)
  }) %>%
    Reduce(rbind, .) %>%
    as.data.table()

  list(
    dungeon = dungeon,
    groups  = groups
  )

}

mythic_leaderboard <- function(dungeon = dungeon, groups = groups) {
  structure(
    list(
      dungeon = dungeon,
      groups  = groups
    ),
    class = "mythic_leaderboard"
  )
}

parse_group_member <- function(member) {
  data.table(
    name = member$profile$name,
    id   = member$profile$id,
    realm_id = member$profile$realm$id,
    faction  = tolower(member$faction$type),
    spec_id  = member$specialization$id
  )
}
