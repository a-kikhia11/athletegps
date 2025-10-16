#' Get All Available STATSports GPS Metrics
#'
#' Queries the GetAvailableMetrics API Endpoint to retrieve all available GPS Metrics.
#'
#' @return A data frame containing ALL GPS Metrics.
#' @export
get_metrics <- function() {
  config <- get_config(quiet = TRUE)
  # Get API Key and URL
  url <- paste0(config$endpoints$metrics)

  # Perform POST request with httr
  response <- tryCatch(
    httr::POST(
      url = url,
      .add_gps_headers(config$version)),
    error = function(e) {
      stop("Failed to connect to the 'GetAvailableMetrics' API Endpoint: ", e$message, call. = FALSE)
    })

  # Consistent response handling
  .handle_api_response(response)

  # Parse JSON body safely
  body_txt <- httr::content(req_D, as = "text", encoding = "UTF-8")
  metrics <- tryCatch(
    jsonlite::fromJSON(body_txt),
    error = function(e) {
      stop("Failed to parse JSON from API: ", e$message, call. = FALSE)
    }
  )

  # Transform Data
  if (!is.null(metrics) && length(metrics) > 0) {
    metrics <- data.frame(Metrics = metrics)
  } else {metrics <- data.frame()}

  # Validate and extract data
  if (!is.null(metrics) && length(metrics) > 0) {
    return(metrics)
  } else {
    stop("No data found in API response.", call. = FALSE)
  }

}
