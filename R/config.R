# Package-level environment to store global STATSports config
.gps_api_env <- new.env(parent = emptyenv())

# Helper for the config file path
.gps_config_path <- function() {
  file.path(Sys.getenv("HOME"), ".statsports_config.json")
}

#' Internal helper function to retry reading a key from the system keyring.
#' This handles potential intermittent delays after writing to the keyring.
#'
#' @param service Character. Keyring service name (e.g., "statsports_credentials").
#' @param username Character. Keyring username (e.g., "api_key").
#' @param retries Integer. Number of retry attempts before giving up (default 5).
#' @param delay Numeric. Delay in seconds between retry attempts (default 0.5).
#'
#' @return Character scalar. The retrieved credential value.
#' Will throw an error if unable to retrieve a valid, non-blank value after all retries.
.retry_key_get <- function(service, username, retries = 5, delay = 0.5) {
  for (i in seq_len(retries)) {
    value <- tryCatch(
      {
        val <- keyring::key_get(service = service, username = username)
        if (nzchar(trimws(val))) {
          if (i > 1) {
            message(sprintf("Retrieved '%s' from keyring on attempt %d.", username, i))
          }
          return(val)
        } else {
          message(sprintf("Retry %d/%d: '%s' value is blank. Retrying in %.1f seconds...", i, retries, username, delay))
          NULL
        }
      },
      error = function(e) {
        # Suppress error messages during retry
        NULL
      }
    )
    if (!is.null(value)) {
      return(value)
    }
    Sys.sleep(delay)
  }

  stop(sprintf(
    "Unable to retrieve a valid (non-blank) '%s' from keyring after %d attempts. Please call load_credentials() manually.",
    username, retries
  ), call. = FALSE)
}

#' Load Stored STATSports API Credentials and Configuration (with retry logic)
#'
#' Loads the saved STATSports API configuration from the user's config file
#' and retrieves sensitive credentials securely from the system keyring.
#'
#' @return Invisibly returns TRUE if credentials and configuration were loaded successfully, FALSE otherwise.
#' @param verbose Logical; if TRUE, prints messages on load status (default FALSE)
#' Internal function (not designed to be used directly by end users)
#' @keywords internal
load_credentials <- function(verbose = TRUE) {
  service_name <- "statsports_credentials"
  path <- .gps_config_path()

  if (!file.exists(path)) {
    if (verbose) message("STATSports config file not found. Please run set_credentials().")
    return(FALSE)
  }

  config <- tryCatch(
    jsonlite::read_json(path, simplifyVector = TRUE),
    error = function(e) {
      if (verbose) message("Failed to read config file: ", e$message)
      NULL
    }
  )
  if (is.null(config)) {
    return(FALSE)
  }

  config$ThirdPartyApiKey <- tryCatch(
    .retry_key_get(service_name, "api_key"),
    error = function(e) {
      if (verbose) message("Unable to retrieve ThirdPartyAPI Key from keyring on initial package load. Run set_credentials() again.")
      return(NULL)
    }
  )

  if (is.null(config$ThirdPartyApiKey)) {
    return(FALSE)
  }

  .gps_api_env$config <- config
  invisible(TRUE)
}

#' Set and Save STATSports API Credentials
#'
#' Securely stores STATSports API credentials in the system keyring and
#' saves non-sensitive configuration to a JSON config file in the user's home directory
#' for reuse across sessions.
#'
#' Sensitive values (API key) are never written to disk and
#' are retrieved securely from the keyring when needed.
#'
#' @param api_key Your STATSports Third-party API Key (stored securely in keyring)
#'
#' @return Invisibly returns TRUE if credentials and configuration were saved successfully.
#' @export
set_credentials <- function(api_key) {
  service_name <- "statsports_credentials"

  # Store sensitive credentials securely in the keyring
  keyring::key_set_with_value(service = service_name, username = "api_key", password = api_key)

  # Verify credentials were saved and are retrievable
  .retry_key_get(service_name, "api_key", retries = 5, delay = 0.5)

  # API version
  vers = "7"

  # Construct endpoint URLs based on region
  endpoints <- list(
    profile = "https://statsportsproseries.com/thirdpartyapi/api/ThirdPartyData/GetPlayerDetails",
    metrics = "https://prd-webapp-thirdpartyapi.azurewebsites.net/api/ThirdPartyData/GetAvailableMetrics",
    session = "https://statsportsproseries.com/thirdpartyapi/api/ThirdPartyData/GetFullSession",
    session_imu = "https://statsportsproseries.com/thirdpartyapi/api/ThirdPartyData/getSessionImuData",
    session_raw = "https://statsportsproseries.com/thirdpartyapi/api/ThirdPartyData/getSessionRawData"
  )

  # Create config list excluding sensitive info
  config <- list(
    endpoints = endpoints,
    version = vers
  )

  # Persist non-sensitive config to disk as JSON
  jsonlite::write_json(config, .gps_config_path(), auto_unbox = TRUE)

  # Load credentials into package environment to refresh state
  load_credentials()

  message("STATSports API configuration saved: sensitive values stored in keyring, non-sensitive data saved to disk.")
  invisible(TRUE)
}

#' Retrieve stored STATSports configuration
#'
#' Returns the configuration list previously set by \code{set_credentials()}.
#' If credentials haven't been set, this function will raise an error.
#'
#' @param safe Logical; if TRUE (default), sensitive values are redacted in printed output (only when not quiet).
#' @param quiet Logical; if TRUE, suppresses printed output (default FALSE).
#'
#' @return A named list containing the stored STATSports configuration values for the current user.
#' Sensitive values are redacted when printed with safe = TRUE.
#' Returned invisibly.
#' @export
get_config <- function(safe = TRUE, quiet = FALSE) {
  if (is.null(.gps_api_env$config)) {
    stop("STATSports API credentials not set. Run `set_credentials()` or `load_credentials()` first.")
  }

  config <- .gps_api_env$config

  # Retrieve sensitive credentials securely from keyring with retry and error handling
  config$ThirdPartyApiKey <- tryCatch(
    .retry_key_get("statsports_credentials", "api_key"),
    error = function(e) {
      stop("Unable to retrieve api_key from keyring: ", e$message)
    }
  )

  # Print config with sensitive data redacted
  if (!quiet && safe) {
    display_config <- config
    display_config$ThirdPartyApiKey <- "<hidden>"
    print(display_config)
  }

  invisible(config)
}

#' Add body to API requests for logging purposes
#'
#' Internal helper to standardise body on all API calls.
#' @keywords internal
#' @noRd
.add_gps_body <- function(key,date) {
  jsonlite::toJSON(list(ThirdPartyApiId = key,sessionDate = date),
                   auto_unbox = TRUE,null = "null")
}

#' Add headers to API requests for logging purposes
#'
#' Internal helper to standardise headers on all API calls.
#' @keywords internal
#' @noRd
.add_gps_headers <- function(vers) {
  httr::add_headers("api-version" = vers,"Accept" = "application/json",
                    "Content-Type" = "application/json")
}

#' Handle API response errors consistently across package functions
#' @keywords internal
#' @noRd
.handle_api_response <- function(response) {
  status <- httr::status_code(response)

  # Safely read the response body
  body_txt <- tryCatch(
    httr::content(response, as = "text", encoding = "UTF-8"),
    error = function(e) "<Failed to read response body>"
  )

  # 2xx: Success (including 204 No Content)
  if (status >= 200 && status < 300) {
    # Only check for empty body if it's NOT a 204
    if (status != 204 && !nzchar(body_txt)) {
      stop("Empty response body from the API. If you believe you should be seeing data, please check your query parameters and validate your request.", call. = FALSE)
    }
    return(invisible(body_txt)) # Always return the body (even if empty) invisibly
  }

  # Try to extract error message
  err_msg <- tryCatch(
    {
      err <- jsonlite::fromJSON(body_txt, simplifyVector = TRUE)
      err$message %||% err$error_description %||% err$error %||% substr(body_txt, 1, 200)
    },
    error = function(e) substr(body_txt, 1, 200)
  )

  # Friendly messages
  msg <- switch(as.character(status),
                "400" = "Bad Request (400): The request was invalid. Please verify your arguments, query parameters, and API credentials using get_config(), and compare against the function documentation.",
                "401" = "Unauthorized (401): Authentication failed. Please check your API credentials using get_config() and then set_credentials().",
                "403" = "Forbidden (403): You do not have permission to access this resource. Please check your API credentials using get_config().",
                "404" = "Not Found (404): The requested resource was not found. Please check your session_date value and API credentials using get_config().",
                "415" = "Incorrect Request Headers or Body (415): The request was invalid. Please verify your arguments, query parameters, and API credentials using get_config(), and compare against the function documentation.",
                "429" = "Too Many Requests (429): Rate limit exceeded. Please try again later.",
                NULL
  )

  if (!is.null(msg)) {
    stop(msg, call. = FALSE)
  } else if (status >= 500 && status < 600) {
    stop(paste0("Server Error (", status, "): A server-side error occurred. Please try again later."), call. = FALSE)
  } else {
    stop(paste0("Unexpected Error (", status, "): ", err_msg), call. = FALSE)
  }
}
