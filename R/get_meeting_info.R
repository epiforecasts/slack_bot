#' Get info for this week's lab meeting
#'
#' @param gsheet_id The ID of the google sheet
#'
#' @importFrom googlesheets4 read_sheet
#' @importFrom dplyr mutate %>% select .data
#'
#' @export
#'
#' @examples
#' googlesheets4::gs4_deauth()
#' gsheet_id <- Sys.getenv("GSHEET_ID")
#' if (gsheet_id != "") {
#'   get_meeting_info(gsheet_id)
#' }
#'
get_meeting_info <- function(gsheet_id) {

  meeting_planning <- read_sheet(gsheet_id) %>%
    mutate(week = format(.data$Date, "%Y-W%W"))

  this_week <- format(lubridate::today(), "%Y-W%W")

  week_plan <- meeting_planning %>%
    dplyr::filter(.data$week == this_week) %>%
    select(.data$Speaker, .data$Topic,
           .data$Chair, .data$Notetaking, .data$Room) %>%
    mutate(Random = NA_character_) %>%
    unlist()

  week_plan[is.na(week_plan)] <- ""

  return(week_plan)

}
