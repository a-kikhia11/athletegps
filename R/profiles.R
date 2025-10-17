#' Get STATSports Athlete profiles
#'
#' Queries the GetPlayerDetails API Endpoint and returns a data frame of athlete profiles.
#'
#' @return A data frame containing STATSports athlete profile information for the stored `api_key`.
#' If no profiles are found, the function raises an error.
#' @export
get_profiles <- function() {
  config <- get_config(quiet = TRUE)
  # Get API Key and URL
  url <- paste0(config$endpoints$profile)
  api_key <- paste0(config$ThirdPartyApiKey)

  # Perform POST request with httr
  response <- tryCatch(
    httr::POST(url = url,
               body = .add_gps_body(api_key,date = NULL),
               .add_gps_headers(config$version)),
    error = function(e) {
      stop("Failed to connect to the Profiles API: ", e$message, call. = FALSE)
    }
  )

  # Consistent response handling
  .handle_api_response(response)

  # Parse JSON body safely
  body_txt <- httr::content(response, as = "text", encoding = "UTF-8")
  profiles <- tryCatch(
    jsonlite::fromJSON(body_txt),
    error = function(e) {
      stop("Failed to parse Profiles API response: ", e$message, call. = FALSE)
    }
  )

  # Validate and extract profiles
  if (!is.null(profiles) && length(profiles) > 0) {
    return(profiles)
  } else {
    stop("No 'profiles' data found in API response.", call. = FALSE)
  }
}
