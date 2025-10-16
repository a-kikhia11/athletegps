utils::globalVariables(c(
  "req_D", "imu_raw", "start_date", "sessionDetails", "id", "sessionPlayers",
  "playerDetails", "startTime", "endTime", "maxSpeed", "squadId", "sessionType",
  "sessionId", "sessionName", "sessionDate", "sessionStartTime", "sessionEndTime",
  "playerId", "customPlayerId", "rawDataId", "displayName", "firstName", "lastName",
  "shortName", "dateOfBirth", "gender", "activeSquadName", "primaryPosition",
  "secondaryPosition", "height", "weight", "bestMaxSpeed", "shareDate", "map", "%>%"
))

#' @import keyring
#' @importFrom dplyr mutate select filter arrange group_by summarise ungroup distinct
#' @importFrom tidyr unnest pivot_wider
#' @importFrom purrr map
#' @importFrom httr GET POST content
#' @importFrom jsonlite fromJSON
#' @importFrom lubridate ymd
#' @importFrom magrittr %>%
NULL

#' Safely extract a field from a list of lists
#'
#' Internal helper function used to extract a specified field from a list of records.
#' If a field value is \code{NULL}, it is replaced with an empty string (\code{""}).
#'
#' @param x A list of lists (records), coming from parsed JSON API responses.
#' @param field A character string naming the field to extract from each record.
#'
#' @return A character vector containing the extracted values, with \code{NULL}s replaced by \code{""}.
#' Internal function (not designed to be used directly by end users)
#' @keywords internal
.safe_extract <- function(x, field) {
  v <- sapply(x, function(el) {
    val <- el[[field]]
    if (is.null(val)) "" else val
  }, USE.NAMES = FALSE)
  return(v)
}

#' Safely extract the first non-empty field from a list of lists
#'
#' Internal helper function used to extract the first non-empty value
#' from a list of fields, in order, from a list of records.
#' If all fields are \code{NULL} or empty strings, returns \code{""}.
#'
#' @param x A list of lists (records), coming from parsed JSON API responses.
#' @param fields A character vector naming the candidate fields to check in order.
#'
#' @return A character vector containing the extracted values, with \code{NULL}s replaced by \code{""}.
#' Internal function (not designed to be used directly by end users)
#' @keywords internal
.safe_extract_first <- function(x, fields) {
  v <- sapply(x, function(el) {
    for (f in fields) {
      val <- el[[f]]
      if (!is.null(val) && nzchar(trimws(val))) {
        return(val)
      }
    }
    ""
  }, USE.NAMES = FALSE)
  return(v)
}

#' Safely extract a single value
#'
#' Internal helper function used to safely retrieve a single value from an object.
#' If the value is \code{NULL}, it is replaced with an empty string (\code{""}).
#'
#' @param x A single value, possibly \code{NULL}.
#'
#' @return The original value, or \code{""} if \code{NULL}.
#' Internal function (not designed to be used directly by end users)
#' @keywords internal
.safe_value <- function(x) {
  if (is.null(x)) {
    return("")
  }
  return(x)
}
