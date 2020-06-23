#' Test if server response is XML or JSON
#'
#' @param resp The server response as a string
#'
#' @return Lowercase string of either json or xml, otherwise empty string
#'
#' @examples
#' response_type ("<xml></xml>")
response_type <- function(resp) {
  if(str_sub(resp, 1, 1) == "{") {
    return("json")
  }else if(str_sub(resp, 1, 1) == "<") {
    return("xml")
  }else {
    return("")
  }
}

#' Convert GetEventFilters API return object to list object ready for POST call to GetDrawSheet API
#'
#' @param filters The filters object inside the response
#' @param up_list Any additional parameters to pass in (e.g. tournamentId)
#' @param down_list Used in the recursive calling of the function
#'
#' @return A list of lists, each containing filters required to call a Draw Sheet
#' @export
#'
#' @examples
#' filters_to_list(x$filters, list(
#'  tourType = x$tourType,
#'  tournamentId = x$tournamentId,
#'  weekNumber = 0
#' ))
filters_to_list <- function(filters, up_list = list(), down_list = list()) {
  for (i in 1:nrow(filters)) {
    up_list[[filters[i, ]$dataName]] <- filters[i, ]$valueCode
    if (is.na(filters[i, ]$subFilter) == FALSE) {
      down_list <- filters_to_list(filters[i, ]$subFilter[[1]], up_list, down_list)
    } else {
      down_list[[length(down_list) + 1]] <- up_list
    }
  }
  return(down_list)
}
