#' get all regions
#'
#' @template dots
#'
#' @export
wow_regions <- function(...) {
  res <- make_get_with_dynamic_namespace(endpoint = "data/wow/region/", ...)
  dots    <- res$dots
  content <- res$content

  lapply(content$regions, function(x) {
    res <- infer_locale(x$name, dots)
    data.table(
      class_id     = x$id,
      class_locale = res$locale,
      class_name   = res$y
    )
  }) %>%
    Reduce(rbind, .)
}


#' get detailed info on a specific region
#'
#' @param region_id an integer, see [wow_regions()] for a full list
#' @template dots
#'
#' @export
wow_region <- function(region_id, ...) {
  res <- make_get_with_dynamic_namespace(endpoint = glue("data/wow/region/{region_id}"))
  dots    <- res$dots
  content <- res$content

  locale <- infer_locale(content$name, dots)

  data.table(
    region_id = content$id,
    locale    = locale$locale,
    name      = locale$name
  )
}



get_region <- function() {
  get_env("BLIZZARD_CLIENT_REGION")
}

validate_region <- function(x) {
  tolower(x) %in% c("eu", "na", "cn", "sea", "kr", "tw")
}

get_default_locale <- function(region) {
  switch(
    cn  = "zh_CN",
    eu  = "en_GB",
    kr  = "ko_KR",
    na  = "en_US",
    sea = "en_US",
    tw  = "zh_TW"
  )
}

make_namespace <- function(type, region) {
  paste(type, region, sep = "-")
}

make_static_namespace <- function(region) {
  make_namespace("static", region)
}

make_dynamic_namespace <- function(region) {
  make_namespace("dynamic", region)
}
