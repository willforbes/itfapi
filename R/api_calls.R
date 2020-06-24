#' Get a list of tournaments from TournamentApi
#'
#' @param circuit The circuit to query on: WCT, MT, WT, JT
#' @param start_date Date to take tournaments from (can be string)
#' @param days Number of days to include in query (integer)
#'
#' @return A dataframe listing or the return parameters from API
#' @export
#'
#' @examples
#' get_tournaments("WCT", "2020-01-01", 31)
get_tournaments <- function(circuit, start_date, days) {
  start_date <- as.Date(start_date)
  end_date <- start_date + days

  addr_calendar <- paste0("https://www.itftennis.com/Umbraco/Api/TournamentApi/GetCalendar?",
                          "circuitCode=", circuit, "&searchString=&skip=0&take=100&nationCodes=&zoneCodes=&",
                          "dateFrom=", start_date, "&dateTo=", end_date,
                          "&indoorOutdoor=&categories=&isOrderAscending=true&orderField=startDate&surfaceCodes=")
  ret_object <- NULL
  server_response <- httr::GET(addr_calendar)
  api_output <- httr::content(server_response, "text")

  if(server_response$status_code < 400) {
    if (response_type(api_output) == "json") {
      ret_object <- jsonlite::fromJSON(addr_calendar, flatten = TRUE)[["items"]]
    } else if (response_type(api_output) == "xml") {
      ret_object <- tibble::as_tibble(as.list(xml2::read_xml(api_output))[[1]]) %>%
        tidyr::unnest_wider(Items) %>% tidyr::unnest(cols = names(.))
    }
  } else {
    stop(paste0("Server error code: ", server_response$status_code))
  }

  return(ret_object)
}


#' Get the event filters for a given tournament using GetEventFilters API
#'
#' @param key The tournament key as a string
#'
#' @return A nested list containing all allowed filters for a tournament
#' @export
#'
#' @examples
#' get_event_filters("WC-ITFFS-AUS-02A-2019")
get_event_filters <- function(key) {
  addr_event <- paste0("https://www.itftennis.com/Umbraco/Api/TournamentApi/GetEventFilters?",
                       "tournamentKey=", key)

  server_response <- httr::GET(addr_event)
  api_output <- httr::content(server_response, "text")
  ret_object <- NULL

  if(server_response$status_code < 400) {
    if (response_type(api_output) == "json") {
      ret_object <- jsonlite::fromJSON(api_output, flatten = TRUE)
    } else if (response_type(api_output) == "xml") {
      ret_object <- as.list(xml2::read_xml(api_output))
    }
  } else {
    stop(paste0("Server error code: ", server_response$status_code))
  }

  return(ret_object)
}


#' Get a draw sheet using filters and GetDrawSheet API, and append darw sheet object to input parameters
#'
#' @param parameters_list List of filters to search for
#'
#' @return The same parameter list as passed in but with draw sheet object appended
#' @export
#'
#' @examples
#' get_draw_sheet(list(
#'   tourType = "N",
#'   tournamentId = 1100044201,
#'   weekNumber = 0,
#'   drawsheetStructureCode: "KO",
#'   eventClassificationCode: "M",
#'   matchTypeCode: "S",
#'   playerTypeCode: "M"
#' ))
get_draw_sheet <- function(parameters_list) {
  server_response <- httr::POST(
    "https://www.itftennis.com/Umbraco/Api/TournamentApi/GetDrawsheet",
    body = parameters_list,
    encode = "json"
  )
  api_output <- httr::content(server_response, "text")

  ret_object <- NULL
  if(server_response$status_code < 400) {
    parameters_list[["draw_sheet"]] <- jsonlite::fromJSON(api_output, flatten = TRUE)
  }else {
    stop(paste0("Server error code: ", server_response$status_code))
  }

  return(parameters_list)
}


#' Get a list of tournaments from TournamentApi, append filters column, unnest and then
#' append draw sheet column
#'
#' @param circuit The circuit to query on: WCT, MT, WT, JT
#' @param start_date Date to take tournaments from (can be string)
#' @param days Number of days to include in query (integer)
#'
#' @return A dataframe of all tournaments, events, and matches
#' @export
#'
#' @examples
#' get_tournaments("WCT", "2020-01-01", 31)
get_tournaments_with_draw_sheet <- function(circuit, start_date, days) {
  get_tournaments(circuit, start_date, days) %>%
    dplyr::mutate(tourn_filter = purrr::map(tournamentKey, function(x)
      get_event_filters(x))) %>%
    dplyr::mutate(tourn_filter = purrr::map(tourn_filter, function(x) {
      if (length(x$filters) > 0) {
        filters_to_list((x$filters),
                        up_list = list(
                          tourType = x$tourType,
                          tournamentId = x$tournamentId,
                          weekNumber = 0
                        )
        )
      }else {
        NULL
      }
    })) %>% tidyr::unnest(tourn_filter) %>%
    dplyr::mutate(draw_sheet = purrr::map(tourn_filter, get_draw_sheet))

}
