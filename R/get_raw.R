#' Get STATSports Raw Data
#'
#' Queries the getSessionRawData API Endpoint to retrieve Raw GPS data
#'
#' @param rawDataId Raw Data ID of a Players GPS Unit (Recommended; run \code{get_session()} to get available rawDataId's of intended session)
#' @return A data frame containing Raw IMU GPS Data
#' @export
get_raw <- function(rawDataId) {
  config <- get_config(quiet = TRUE)
  # Get API Key and URL
  url <- paste0(config$endpoints$session_raw)
  api_key <- paste0(config$ThirdPartyApiKey)

  # Resolve start_date from argument or stored config
  if (is.null(rawDataId)) {
    stop("Raw Data ID not set. Please provide a valid ID first.")
  }

  page <- 0
  raw_data <- data.frame()

  repeat {

    # Build request body
    body <- list(
      thirdPartyApiId = api_key,
      rawDataId = rawDataId,
      page = page
    )

    # Perform POST request with httr
    response <- tryCatch(
      httr::POST(
        url = url,
        body = body,
        .add_gps_headers(config$version)),
      error = function(e) {
        stop("Failed to connect to the 'getSessionRawData' API Endpoint: ", e$message, call. = FALSE)
      })
    # Consistent response handling
    .handle_api_response(response)
    # Parse JSON body safely
    body_txt <- httr::content(response, as = "text", encoding = "UTF-8")
    raw <- tryCatch(
      jsonlite::fromJSON(body_txt),
      error = function(e) {
        stop("Failed to parse JSON from API: ", e$message, call. = FALSE)
      }
    )

    if (raw$nextPage < 0) {
      message("No more tests to retrieve. Stopping pagination.")
      break
    }

    if (!is.null(raw$data) && length(raw$data) > 0) {
      raw_data <- dplyr::bind_rows(raw_data,as.data.frame(raw$data))
      page <- raw$nextPage
      message("Continuing to page ", page)
    } else {
      message("No tests returned, stopping pagination.")
      break
    }

    Sys.sleep(0.2) # Pause to respect rate limits
  }

  # Validate and extract data
  if (!is.null(raw_data) && length(raw_data) > 0) {
    return(raw_data)
  } else {
    stop("No Raw data found in API response.", call. = FALSE)
  }

}
