#' Get info for this week's lab meeting
#'
#' @param gsheet_id The ID of the google sheet
#'
#' @importFrom googlesheets4 read_sheet
#' @importFrom dplyr mutate %>% select
#'
#' @export
#'
get_meeting_info <- function(gsheet_id) {

  meeting_planning <- read_sheet(gsheet_id) %>%
    mutate(week = lubridate::week(Date))

  this_week <- lubridate::week(lubridate::today())

  week_plan <- meeting_planning %>%
    dplyr::filter(week == this_week) %>%
    select(Speaker, Topic) %>%
    unlist()

  return(week_plan)

}
