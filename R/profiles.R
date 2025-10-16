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
  date <- NULL

  # Perform POST request with httr
  response <- tryCatch(
    httr::POST(url = url,
               body = .add_gps_body(api_key,date),
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
    jsonlite::fromJSON(body_txt, simplifyVector = FALSE),
    error = function(e) {
      stop("Failed to parse Profiles API response: ", e$message, call. = FALSE)
    }
  )

  # Validate and extract profiles
  if (!is.null(profiles) && length(profiles) > 0) {

    profiles_df <- data.frame(
      customPlayerId = .safe_extract(profiles, "customPlayerId"),
      displayName = .safe_extract(profiles, "displayName"),
      firstName = .safe_extract(profiles, "firstName"),
      lastName = .safe_extract(profiles, "lastName"),
      shortName = .safe_extract(profiles, "shortName"),
      dateOfBirth = .safe_extract(profiles, "dateOfBirth"),
      gender = .safe_extract(profiles, "gender"),
      activeSquadName = .safe_extract(profiles, "activeSquadName"),
      primaryPosition = .safe_extract(profiles, "primaryPosition"),
      secondaryPosition = .safe_extract(profiles, "secondaryPosition"),
      height = .safe_extract(profiles, "height"),
      weight = .safe_extract(profiles, "weight"),
      maxSpeed = .safe_extract(profiles, "maxSpeed"),
      maxHeartRate = .safe_extract(profiles, "maxHeartRate"),
      restingHeartRate = .safe_extract(profiles, "restingHeartRate"),
      maxAccel = .safe_extract(profiles, "maxAccel"),
      maxDecel = .safe_extract(profiles, "maxDecel"),
      runningSymmetry = .safe_extract(profiles, "runningSymmetry"),
      stringsAsFactors = FALSE
    )

    if (any(profiles_df$dateOfBirth == "")) {
      warning("You have profiles in your organisation which have NULL date of birth values. Please ensure you populate these values for all profiles.")
    }

    return(profiles_df)
  } else {
    stop("No 'profiles' data found in API response.", call. = FALSE)
  }
}
