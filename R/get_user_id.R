#' Get slack user ID from their name
#'
#' @param name A character string with the name ("Firstname Lastname")
#' @inheritParams get_meeting_info
#'
#' @importFrom googlesheets4 read_sheet
#' @importFrom dplyr filter %>% pull
#'
#' @export
#' @autoglobal
#'
#' @examples
#' googlesheets4::gs4_deauth()
#' gsheet_id <- Sys.getenv("GSHEET_ID")
#' if (gsheet_id != "") {
#'   get_user_id("Hugo Gruson", gsheet_id)
#' }
#'
get_user_id <- function(name, gsheet_id) {

  id <- read_sheet(gsheet_id, sheet = "Slack-IDs") %>%
    filter(name == Username) %>%
    pull(`User ID`)

  if (length(id) == 0) {
    stop("We don't know the slack ID of this user", call. = FALSE)
  }

  return(id)

}
