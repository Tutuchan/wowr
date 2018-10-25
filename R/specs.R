#' get all playable specializations
#'
#' @template dots
#'
#' @export
wow_specializations <- function(...) {

  res <- make_get_with_static_namespace(endpoint = "data/wow/playable-specialization/")
  dots    <- res$dots
  content <- res$content

  characters <- lapply(content$character_specializations, function(x) {
    res <- infer_locale(x$name, dots)
    data.table(
      spec_id     = x$id,
      spec_type   = "character",
      spec_locale = res$locale,
      spec_name   = res$y
    )
  }) %>%
    Reduce(rbind, .)

  pets <- lapply(content$pet_specializations, function(x) {
    res <- infer_locale(x$name, dots)
    data.table(
      spec_id     = x$id,
      spec_type   = "pet",
      spec_locale = res$locale,
      spec_name   = res$y
    )
  }) %>%
    Reduce(rbind, .)

  rbind(
    characters,
    pets
  )
}

#' get detailed info on a specific specialization
#'
#' @param spec_id an integer, see [wow_specializations()] for a full list
#' @template dots
#'
#' @export
wow_specialization <- function(spec_id, ...) {
  res <- make_get_with_static_namespace(endpoint = glue("data/wow/playable-specialization/{spec_id}"))
  dots    <- res$dots
  content <- res$content

  class       <- infer_locale(content$playable_class$name)
  spec        <- infer_locale(content$name)
  male_desc   <- infer_locale(content$gender_description$male)
  female_desc <- infer_locale(content$gender_description$female)
  role        <- infer_locale(content$role$name)
  talents     <- parse_talents(content$talent_tiers)
  pvp_talents <- parse_pvp_talents(content$pvp_talents)

  desc <- data.table(
    id          = content$id,
    locale      = class$locale,
    class       = class$y,
    spec        = spec$y,
    male_desc   = male_desc$y,
    female_desc = female_desc$y,
    role        = role$y
  )

  structure(
    list(
      description = desc,
      talents     = talents,
      pvp_talents = pvp_talents
    ),
    class = "wow_specialization"
  )

}

parse_talents <- function(talents, dots) {
  lapply(talents, parse_talent_tier, dots = dots) %>%
    Reduce(rbind, .)
}

parse_pvp_talents <- function(talents, dots) {
  parse_talent_tier(
    talent_tier = list(talents = talents, level = NA),
    dots = dots
  )
}

parse_talent_tier <- function(talent_tier, dots) {

  talents <- lapply(talent_tier$talents, parse_talent, dots = dots) %>%
    Reduce(rbind, .) %>%
    transform(level = talent_tier$level)

  talents[c("level", "id", "locale", "name", "tooltip", "cast")] %>%
    as.data.table()
}

parse_talent <- function(talent, dots) {
  name    <- infer_locale(talent$talent$name, dots)
  tooltip <- infer_locale(talent$spell_tooltip$description, dots)
  cast    <- infer_locale(talent$spell_tooltip$cast_time, dots)

  data.table(
    locale  = name$locale,
    id      = talent$talent$id,
    name    = name$y,
    tooltip = tooltip$y,
    cast    = cast$y
  )

}
