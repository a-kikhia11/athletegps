#' Get STATSports IMU Data
#'
#' Queries the getSessionImuData API Endpoint to retrieve only Raw GPS IMU data
#'
#' @param rawDataId Raw Data ID of a Players GPS Unit (Recommended; run \code{get_session()} to get available rawDataId's of intended session)
#' @return A data frame containing Raw IMU GPS Data
#' @export
get_imu_only <- function(rawDataId) {
  config <- get_config(quiet = TRUE)
  # Get API Key and URL
  url <- paste0(config$endpoints$session_imu)
  api_key <- paste0(config$ThirdPartyApiKey)

  # Resolve start_date from argument or stored config
  if (is.null(rawDataId)) {
    stop("Raw Data ID not set. Please provide a valid ID first.")
  }

  page <- 0
  imu_data <- data.frame()

  repeat {

    # Perform POST request with httr
    response <- tryCatch(
      httr::POST(
        url = url,
        body = jsonlite::toJSON(list(thirdPartyApiId = api_key,rawDataId = rawDataId,nextPage = page),
                                auto_unbox = TRUE,null = "null"),
        .add_gps_headers(config$version)),
      error = function(e) {
        stop("Failed to connect to the 'getSessionImuData' API Endpoint: ", e$message, call. = FALSE)
      })
    # Consistent response handling
    .handle_api_response(response)
    # Parse JSON body safely
    body_txt <- httr::content(response, as = "text", encoding = "UTF-8")
    imu_raw <- tryCatch(
      jsonlite::fromJSON(body_txt),
      error = function(e) {
        stop("Failed to parse JSON from API: ", e$message, call. = FALSE)
      }
    )

    if (imu_raw$nextPage < 0) {
      message("No more tests to retrieve. Stopping pagination.")
      break
    }

    if (!is.null(imu_raw$data) && length(imu_raw$data) > 0) {
      imu_data <- dplyr::bind_rows(imu_data,as.data.frame(imu_raw$data))
      page <- imu_raw$nextPage
      message("Continuing to page ", page)
    } else {
      message("No tests returned, stopping pagination.")
      break
    }

    Sys.sleep(1.5) # Pause to respect rate limits
  }


  # Validate and extract data
  if (!is.null(imu_data) && length(imu_data) > 0) {
    return(imu_data)
  } else {
    stop("No 'IMU' data found in API response.", call. = FALSE)
  }

}
