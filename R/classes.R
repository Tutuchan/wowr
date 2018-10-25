#' get all playable classes
#'
#' @template dots
#'
#' @export
wow_classes <- function(...) {
  res <- make_get_with_static_namespace(endpoint = "data/wow/playable-class/", ...)
  dots    <- res$dots
  content <- res$content

  lapply(content$classes, function(x) {
    res <- infer_locale(x$name, dots)
    data.table(
      class_id     = x$id,
      class_locale = res$locale,
      class_name   = res$y
    )
  }) %>%
    Reduce(rbind, .)
}


#' get detailed info on a specific class
#'
#' @param class_id an integer, see [wow_classes()] for a full list
#' @template dots
#'
#' @export
wow_class <- function(class_id, ...) {
  res <- make_get_with_static_namespace(endpoint = glue("data/wow/playable-class/{class_id}"))
  dots    <- res$dots
  content <- res$content

  class       <- infer_locale(content$name, dots)
  male_name   <- infer_locale(content$gender_name$male, dots)
  female_name <- infer_locale(content$gender_name$female, dots)
  specs       <- parse_class_specializations(content$specializations, dots)

  desc <- data.table(
    id          = content$id,
    locale      = class$locale,
    name        = class$y,
    male_name   = male_name$y,
    female_name = female_name$y
  )

  structure(
    list(
      description     = desc,
      specializations = specs
    ),
    class = "wow_class"
  )

}

print.wow_class <- function(x, ...) {

  desc <- x$description
  spec <- x$specializations

  if (nrow(desc) > 1) {
    desc <- desc[desc$locale == "en_US",]
    spec <- spec[spec$locale == "en_US",]
  }

  if (desc$male_name != desc$female_name) {
    name <- paste(desc$male_name, desc$female_name, sep = " / ")
  } else {
    name <- desc$name
  }
  cat("<WoW class>\n")
  cat(glue(" - name (id): {name} ({desc$id})"), "\n")
  cat(" - specs:\n")
  for (i in seq_nrow(spec)) {
    cat(glue("    - {spec$spec_name[i]} ({spec$spec_id[i]})"), "\n")
  }


}

parse_class_specializations <- function(x, dots) {
  lapply(x, parse_class_specialization, dots = dots) %>%
    Reduce(rbind, .)
}

parse_class_specialization <- function(x, dots) {

  res <- infer_locale(x$name, dots)
  data.table(
    spec_id   = x$id,
    locale    = res$locale,
    spec_name = res$y
  )


}
