#' Retrieves Data from the Tejapi Datatable endpoint
#'
#' @details Set your \code{api_key} with \code{Tejapi.api_key} function. For instructions on finding your api key go to \url{https://api.tej.com.tw}
#'
#' @param datatable_code Datatable code on Tejapi specified as a string.
#' @param paginate When set to TRUE, fetches up to 1,000,000 rows of data
#' @param ... Additional named values that are interpreted as Tejapi API parameters.
#' @return Returns a data.frame.
#' @seealso \code{\link{Tejapi.api_key}}
#' @examples \dontrun{
#' Tejapi.datatable('TWN/AIND', paginate=TRUE)
#' }
#' @export
Tejapi <- function(datatable_code, paginate = FALSE, ...) {
  path <- paste0("datatables/", datatable_code)
  params <- list(...)

  # make request for first page of data
  json <- do.call(Tejapi.rest, c(path = path, params))
  datatable <- json$datatable
  data <- datatable$data
  # contains a list of names and corresponding types
  columns <- datatable$columns
  next_cursor_id <- json$meta$next_cursor_id
  df <- as.data.frame(data, stringsAsFactors = FALSE)

  # continue to make requests for data if paginate=TRUE and there is data
  while (isTRUE(paginate) && !is.null(next_cursor_id)) {
    params["opts.cursor_id"] <- next_cursor_id
    json <- do.call(Tejapi.rest, c(path = path, params))
    df_page <- as.data.frame(json$datatable$data, stringsAsFactors = FALSE)
    df <- rbind(df, df_page)
    next_cursor_id <- json$meta$next_cursor_id

    # only fetch a maximum of 1,000,000 rows
    if (nrow(df) >= Tejapi.max_rows() && !is.null(next_cursor_id)) {
      warning(paste("This call returns a larger amount of data than Tejapi allows.",
                    "Please view our documentation on developer methods to request more data.",
                    "https://api.tej.com.tw/documents.html"), call. = FALSE)
      break
    }
  }

  if (!isTRUE(paginate) && !is.null(next_cursor_id)) {
    warning(paste("This call returns more data. To request more pages, please set paginate=TRUE",
                  "in your Tejapi call. For more information see our documentation:",
                  "https://api.tej.com.tw/documents.html"), call. = FALSE)
  }

  df <- Tejapi.set_df_columns(df, columns)

  return(df)
}

Tejapi.set_df_columns <- function(df, columns) {
  ncols <- length(columns[, 1])
  # if df is empty create an empty df with ncolumns set
  # or else we won't be able to set the column names
  if (nrow(df) <= 0 && ncols > 0) {
    df <- data.frame(matrix(ncol = ncols, nrow = 0))
  }

  # set column names
  names(df) <- columns[, 1]

  # set column types
  df <- Tejapi.convert_df_columns(df, columns[, 2])

  return(df)
}

Tejapi.convert_df_columns <- function(df, column_types) {
  if (length(column_types) <= 0) {
    return(df)
  }
  column_types <- tolower(column_types)
  for (i in 1:length(column_types)) {
    if (grepl("^float|^bigdecimal|^integer|^double|^decimal", column_types[i])) {
      df[, i] <- as.numeric(df[, i])
    } else if (grepl("^datetime", column_types[i])) { #2018-06-21T00:00:00.000Z
      df[, i] <- as.POSIXct(df[, i],format="%Y-%m-%dT%H:%M:%OSZ")
    } else if (grepl("^date", column_types[i])) {
      df[, i] <- as.Date(df[, i])
    } else {
      df[, i] <- as.character(df[, i])
    }
  }
  return(df)
}

Tejapi.max_rows <- function() {
  return(100000)
}