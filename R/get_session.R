#' Get STATSports Session(s) and Drills
#'
#' Queries the GetFullSession API Endpoint to retrieve GPS Session and Drill data
#'
#' @param session_date ISO 8601 UTC date string (e.g., "2025-06-25").
#' @param drills Logical TRUE or FALSE to include drill breakdown.
#' @param metrics Optional character vector of available metrics to filter by (Recommended; run \code{get_metrics()} to get all available metrics)
#' @return A data frame containing GPS Session results matching the optional filters.
#' @export
get_session <- function(session_date, drills = TRUE, metrics = NULL) {
  config <- get_config(quiet = TRUE)
  # Get API Key and URL
  url <- paste0(config$endpoints$session)
  api_key <- paste0(config$ThirdPartyApiKey)

  # Resolve start_date from argument or stored config
  if (is.null(session_date)) {
    stop("Session date not set. Please provide a valid date in ISO 8601 UTC format (YYYY-MM-DD) first.")
  } else {
    # Validate format if supplied directly
    if (!grepl("^\\d{4}-\\d{2}-\\d{2}$", start_date)) {
      stop("`session_date` must be in ISO 8601 format: e.g., '2025-06-25'")
    }
  }

  # Format metric variable
  if (is.null(metrics)) {
    metrics <- NULL
  } else {
    metrics <- ifelse(grepl("^drillKpi\\.", metrics), metrics, paste0("drillKpi.", metrics))
  }

  # Perform POST request with httr
  response <- tryCatch(
    httr::POST(
      url = url,
      body = .add_gps_body(api_key,session_date),
      .add_gps_headers(config$version)),
    error = function(e) {
      stop("Failed to connect to the 'GetFullSession' API Endpoint: ", e$message, call. = FALSE)
    })

  # Consistent response handling
  .handle_api_response(response)

  # Parse JSON body safely
  body_txt <- httr::content(response, as = "text", encoding = "UTF-8")
  gps_data <- tryCatch(
    jsonlite::fromJSON(body_txt),
    error = function(e) {
      stop("Failed to parse JSON from API: ", e$message, call. = FALSE)
    }
  )

  # Transform Data
  if (!is.null(gps_data) && length(gps_data) > 0 && isFALSE(drills)) {
    gps_data <- gps_data %>% tidyr::unnest(sessionDetails) %>% dplyr::rename(sessionId = id) %>% tidyr::unnest(sessionPlayers) %>% tidyr::unnest(playerDetails) %>%
      dplyr::rename(playerId = id,sessionStartTime = startTime,sessionEndTime = endTime,bestMaxSpeed = maxSpeed) %>%
      dplyr::select(squadId,sessionType,sessionId,sessionName,sessionDate,sessionStartTime,sessionEndTime,
                    playerId,customPlayerId,rawDataId,displayName,firstName,lastName,shortName,dateOfBirth,gender,activeSquadName,primaryPosition,secondaryPosition,
                    height,weight,bestMaxSpeed,shareDate,drills)
  } else if (!is.null(gps_data) && length(gps_data) > 0 && isTRUE(drills) && is.null(metrics)) {
    gps_data <- gps_data %>% tidyr::unnest(sessionDetails) %>% dplyr::rename(sessionId = id) %>% tidyr::unnest(sessionPlayers) %>% tidyr::unnest(playerDetails) %>%
      dplyr::rename(playerId = id,sessionStartTime = startTime,sessionEndTime = endTime,bestMaxSpeed = maxSpeed) %>%
      dplyr::select(squadId,sessionType,sessionId,sessionName,sessionDate,sessionStartTime,sessionEndTime,
                    playerId,customPlayerId,rawDataId,displayName,firstName,lastName,shortName,dateOfBirth,gender,activeSquadName,primaryPosition,secondaryPosition,
                    height,weight,bestMaxSpeed,shareDate,drills) %>%
      dplyr::mutate(drills = purrr::map(drills, ~ {
        if (is.data.frame(.x)) {
          .x %>% dplyr::select(-sessionPlayerDataId, -sessionType)
        } else {
          .x
        }})) %>%
      dplyr::mutate(drills = map(drills, ~ {flat <- jsonlite::flatten(.x)
                    names(flat) <- ifelse(grepl("^drill", names(flat)),names(flat),paste0("drill",toupper(substring(names(flat), 1, 1)),substring(names(flat), 2)))
                    names(flat) <- sub("^drillKpi\\.totalTime$", "totalTime", names(flat))
                    flat})) %>%
      dplyr::mutate(drills = purrr::map(drills, ~ {flat <- jsonlite::flatten(.x)
      names(flat) <- gsub("^drillKpi\\.", "", names(flat))
      flat})) %>%
      tidyr::unnest(drills)
  } else if (!is.null(gps_data) && length(gps_data) > 0 && isTRUE(drills) && !is.null(metrics)) {
    gps_data <- gps_data %>% tidyr::unnest(sessionDetails) %>% dplyr::rename(sessionId = id) %>% tidyr::unnest(sessionPlayers) %>% tidyr::unnest(playerDetails) %>%
      dplyr::rename(playerId = id,sessionStartTime = startTime,sessionEndTime = endTime,bestMaxSpeed = maxSpeed) %>%
      dplyr::select(squadId,sessionType,sessionId,sessionName,sessionDate,sessionStartTime,sessionEndTime,
                    playerId,customPlayerId,rawDataId,displayName,firstName,lastName,shortName,dateOfBirth,gender,activeSquadName,primaryPosition,secondaryPosition,
                    height,weight,bestMaxSpeed,shareDate,drills) %>%
      dplyr::mutate(drills = purrr::map(drills, ~ {
        if (is.data.frame(.x)) {
          .x %>% dplyr::select(-sessionPlayerDataId, -sessionType)
          } else {
            .x
          }})) %>%
      dplyr::mutate(drills = map(drills, ~ {flat <- jsonlite::flatten(.x)
                    names(flat) <- ifelse(grepl("^drill", names(flat)),names(flat),paste0("drill",toupper(substring(names(flat), 1, 1)),substring(names(flat), 2)))
                    names(flat) <- sub("^drillKpi\\.totalTime$", "totalTime", names(flat))
                    flat})) %>%
      dplyr::mutate(drills = purrr::map(drills, ~ {
        if (is.data.frame(.x)) {
          drillkpi_cols <- grep("^drillKpi\\.", names(.x), value = TRUE)
          cols_to_keep <- c(lubridate::setdiff(names(.x), drillkpi_cols),
                            lubridate::intersect(drillkpi_cols, metrics))
          dplyr::select(.x, dplyr::all_of(cols_to_keep))
          } else {
            .x
          }})) %>%
      dplyr::mutate(drills = purrr::map(drills, ~ {flat <- jsonlite::flatten(.x)
                    names(flat) <- gsub("^drillKpi\\.", "", names(flat))
                    flat})) %>%
      tidyr::unnest(drills)
  } else {gps_data <- data.frame()}

  # Validate and extract data
  if (!is.null(gps_data) && length(gps_data) > 0) {
    return(gps_data)
  } else {
    stop("No 'session' data found in API response.", call. = FALSE)
  }

}
