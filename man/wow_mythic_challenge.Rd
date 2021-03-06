% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mythics.R
\name{wow_mythic_challenge}
\alias{wow_mythic_challenge}
\title{get affixes for the current week}
\usage{
wow_mythic_challenge(...)
}
\arguments{
\item{...}{additional parameters to be passed to the request: token, region and/or locale}
}
\value{
a data.table with the following columns
\itemize{
\item period: integer, the current period
\item start: POSIXct, start of the current period
\item end: POSIXct, end of the current period
\item locale: character, the name of the locale
\item affix: character, the name of the affix in the current locale
\item level: integer, at which M+ level this affix starts being present
}
}
\description{
get affixes for the current week
}
