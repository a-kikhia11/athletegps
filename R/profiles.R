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
               body = jsonlite::toJSON(
                 list(
                   ThirdPartyApiId = api_key,
                   sessionDate = NULL
                 ),
                 auto_unbox = TRUE,
                 null = "null"
               ),
               httr::add_headers(
                 "api-version" = config$version,
                 "Accept" = "application/json",
                 "Content-Type" = "application/json"
               )),
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
