#' Query or set Tejapi API key
#' @param api_key Optionally passed parameter to set Tejapi \code{api_key}.
#' @return Returns invisibly the currently set \code{api_key}.
#' @examples \dontrun{
#' Tejapi.api_key('foobar')
#' }
#' @export
Tejapi.api_key <- function(api_key) {
  if (!missing(api_key)) {
    options(Tejapi.api_key = api_key)
  }
  invisible(getOption("Tejapi.api_key"))
}

Tejapi.api_version <- function(api_version) {
  if (!missing(api_version)) {
    options(Tejapi.api_version = api_version)
  }
  invisible(getOption("Tejapi.api_version"))
}

Tejapi.base_url <- function(base_url) {
  if (!missing(base_url)) {
    options(Tejapi.base_url = base_url)
  }
  invisible(getOption("Tejapi.base_url", "https://api.tej.com.tw/api"))
}