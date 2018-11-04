#' Executes Tejapi API calls
#'
#' @details Set your \code{api_key} with \code{Tejapi.api_key} function. For instructions on finding your api key go to \url{https://api.tej.com.tw}
#'
#' @param path Path to api resource.
#' @param http Type of http request sent.
#' @param postdata A character or raw vector that is sent in a body.
#' @param ... Named values that are interpretted as Tejapi API parameters. Please see \url{https://api.tej.com.tw/documents.html}.
#' @return Tejapi API response.
#' @seealso \code{\link{Tejapi.api_key}}
#' @examples \dontrun{
#' Tejapidata = Tejapi.rest(path="datasets/TWN/AIND", http="GET")
#' plot(Tejapidata[,1])
#' }
#' @importFrom httr VERB
#' @importFrom jsonlite fromJSON
#' @export
Tejapi.rest <- function(path, http = c("GET", "PUT", "POST", "DELETE"), postdata = NULL, ...) {
  http <- match.arg(http)
  request <- Tejapi.rest.build_request(path, ...)
  response <- httr::VERB(http, request$request_url,
                         config = do.call(httr::add_headers, request$headers),
                         body = postdata, query = request$params)

  Tejapi.rest.handl_errors(response)
  text_response <- httr::content(response, as = "text")

  json_response <- tryCatch(jsonlite::fromJSON(text_response, simplifyVector = TRUE), error = function(e) {
      stop(e, " Failed to parse response: ", text_response)
    })
  json_response
}

Tejapi.rest.build_request <- function(path, ...) {
  params <- list(...)
  # ensure vectors get converted into v3 api supported query params
  # e.g., opts.columns=c('ticker', 'rev') -> list('opts.columns[]'=ticker,'opts.columns[]'=rev)
  params <- Tejapi.rest.build_query_params(params)
  # ensure Dates convert to characters or else curl will convert the Dates to timestamp
  params <- Tejapi.rest.convert_dates_to_character(params)

  request_url <- paste(Tejapi.base_url(), path, sep = "/")
  accept_value <- "application/json"

  #Tejapi_version <- as.character(utils::packageVersion("Tejapi"))
  Tejapi_version <- "1.0"
  headers <- list(Accept = accept_value, `Request-Source` = "R", `Request-Source-Version` = Tejapi_version)

  if (!is.null(Tejapi.api_key())) {
    headers <- c(headers, list(`X-Api-Token` = Tejapi.api_key()))
  }

  # query param api_key takes precedence
  if (!is.null(params$api_key)) {
    headers <- c(headers, list(`X-Api-Token` = params$api_key))
    params$api_key <- NULL
  }

  list(request_url = request_url, headers = headers, params = params)
}

Tejapi.rest.handl_errors <- function(response) {
  if (!(httr::status_code(response) >= 200 && httr::status_code(response) < 300)) {
    stop(httr::content(response, as = "text"), call. = FALSE)
  }
}

Tejapi.rest.convert_dates_to_character <- function(params) {
  convert_date_to_character <- function(param) {
    if (class(param) == "Date") {
      param <- as.character(param)
    }
    param
  }
  lapply(params, convert_date_to_character)
}

Tejapi.rest.build_query_params <- function(params) {
  if (length(params) <= 0) {
    return(params)
  }
  mod_params <- list()
  for(i in 1:length(params)) {
    # keep the params the same if not a vector
    converted_params <- params[i]

    # check val to see if vector
    # if so, convert
    if (length(params[[i]]) > 1) {
      converted_params <- Tejapi.rest.convert_vector_params(names(params[i]), params[[i]])
    }
    mod_params <- c(mod_params, converted_params)
  }
  return(mod_params)
}

Tejapi.rest.convert_vector_params <- function(name, vector_values) {
  mod_query_name <- paste0(name, "[]")
  mod_query_list <- list()

  for(val in vector_values) {
    l <- list()
    l[[mod_query_name]] <- val
    mod_query_list <- c(mod_query_list, l)
  }
  return(mod_query_list)
}