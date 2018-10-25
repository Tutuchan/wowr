# requests -----------------------------------------------------------------

#' add default values to a request if needed
#'
#' @template dots
augment_query <- function(...) {
  dots <- list(...)

  if (is.null(dots$token)) {
    dots$token <- wow_auth()
  }

  if (is.null(dots$region)) {
    dots$region <- get_region()
  }

  dots
}

#' check if a quantity has multiple locales
#'
#' Return a consistent structure whether it is single- or multi-locale.
#'
#' @param x an object, either a vector or a list
#' @param l a list, additional parameters, usually the `dots` variable
infer_locale <- function(x, l) {
  if (length(x) > 1) {
    locale <- names(x)
    y      <- unname(unlist(x))
  } else {
    y      <- x
    if (!is.null(l$locale)) {
      locale <- l$locale
    } else {
      locale <- get_default_locale(l$region)
    }
  }

  list(locale = locale, y = y)
}

# general -----------------------------------------------------------------

to_posix <- function(x) {
  as.POSIXct(x / 1000, origin = "1970-01-01")
}

to_minutes <- function(x) {
  mins <- x %/% 60
  secs <- round(x %% 60)

  if (secs > 0) {
    paste0(mins, "min and ", secs, "s")
  } else {
    paste0(mins, "min")
  }
}

get_env <- function(x) {
  res <- Sys.getenv(x)
  if (res == "") {
    res <- NULL
  }

  res
}

seq_nrow <- function(df) {
  seq_len(nrow(df))
}
