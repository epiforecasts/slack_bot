#' Get info for this week's lab meeting
#'
#' @param gsheet_id The ID of the google sheet
#'
#' @importFrom googlesheets4 read_sheet
#' @importFrom dplyr mutate %>% select .data
#'
#' @export
#' @autoglobal
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
    mutate(
      week = format(Date, "%Y-W%W"),
      Time = format(Time, "%H:%M")
    )

  this_week <- format(lubridate::today(), "%Y-W%W")

  week_row <- meeting_planning %>%
    dplyr::filter(.data$week == this_week)

  ## Combine up to three speaker columns into a single comma-separated string
  ## so format_meeting_info can split and tag each.
  pick <- function(col) {
    if (col %in% names(week_row)) as.character(week_row[[col]]) else NA_character_
  }
  speakers <- c(pick("Speaker"), pick("Speaker2"), pick("Speaker3"))
  speakers <- speakers[!is.na(speakers) & nchar(speakers) > 0]
  speaker <- paste(speakers, collapse = ", ")

  field <- function(col) {
    val <- pick(col)
    if (length(val) == 0 || is.na(val)) "" else val
  }

  week_plan <- c(
    Speaker = speaker,
    Topic   = field("Topic"),
    Chair   = field("Chair"),
    Room    = field("Room"),
    Time    = field("Time"),
    Random  = ""
  )

  return(week_plan)

}
