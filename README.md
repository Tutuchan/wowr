
<!-- README.md is generated from README.Rmd. Please edit that file -->

# wowr

The goal of wowr is to provide functions to interact with the [World of
Warcraft
API](https://develop.battle.net/documentation/api-reference/world-of-warcraft-community-api).

## Installation

You can install wowr from [Github](https://www.github.com/tutuchan/wowr)
with:

``` r
remotes::install_github("tutuchan/wowr")
```

In order to use the functions in `wowr`, you need to:

  - own a Blizzard account (or create a new one),
  - add a Blizzard authenticator (if you don’t have one already),
  - create a client on the developer portal and retrieve your client id
    and secret. These steps are explained in the [Blizzard
    documentation](https://develop.battle.net/documentation/guides/getting-started).

These values can be passed to the `wow_auth()` function or, for
convenience, stored in your .Rprofile as the `BLIZZARD_CLIENT_ID` and
`BLIZZARD_CLIENT_SECRET` environment variables.

## Usage

### Authentication

The `wow_auth()` function returns an authentication token that can be
passed to subsequent functions.

``` r
library(wowr)
token <- wow_auth(region = "EU")

wow_realms(token = token)
#>         id      slug locale        name
#>    1:  500  aggramar  en_US    Aggramar
#>    2:  500  aggramar  es_MX    Aggramar
#>    3:  500  aggramar  pt_BR    Aggramar
#>    4:  500  aggramar  de_DE    Aggramar
#>    5:  500  aggramar  en_GB    Aggramar
#>   ---                                  
#> 3200: 1929 blackscar  it_IT Черный Шрам
#> 3201: 1929 blackscar  ru_RU Черный Шрам
#> 3202: 1929 blackscar  ko_KR   Blackscar
#> 3203: 1929 blackscar  zh_TW   Blackscar
#> 3204: 1929 blackscar  zh_CN        黑痕
```

The token is also cached using `httr` in a `.httr-oauth` file. If you
don’t provide a token, `wowr` will check if there is a cached token and
if there is, if it’s still valid. It is recommended to provide the token
as checking the validity requires a call to the Blizzard API every time.

``` r
wow_realms()
#>         id      slug locale        name
#>    1:  500  aggramar  en_US    Aggramar
#>    2:  500  aggramar  es_MX    Aggramar
#>    3:  500  aggramar  pt_BR    Aggramar
#>    4:  500  aggramar  de_DE    Aggramar
#>    5:  500  aggramar  en_GB    Aggramar
#>   ---                                  
#> 3200: 1929 blackscar  it_IT Черный Шрам
#> 3201: 1929 blackscar  ru_RU Черный Шрам
#> 3202: 1929 blackscar  ko_KR   Blackscar
#> 3203: 1929 blackscar  zh_TW   Blackscar
#> 3204: 1929 blackscar  zh_CN        黑痕
```

### Dot parameters

Many functions take `...` parameters. They can be one of the following:

  - *token*: the token returned from `wow_auth()`,
  - *region*: a character, one of the available
    [regions](https://develop.battle.net/documentation/guides/regionality-partitions-and-localization),
  - *locale*: a character, one of the available
    [locales](https://develop.battle.net/documentation/guides/regionality-partitions-and-localization)
