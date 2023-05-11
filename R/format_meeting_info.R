#' Format meeting info as a simple bullet list
#'
#' @importFrom glue glue
#'
#' @param assignee Who is presenting this week?
#' @param random Who is charge of talking about a random interesting thing?
#' @param chair Who will chair this meeting?
#' @param notes Who will take notes?
#' @param topic What will be the topic?
#' @param zoom_link What is the zoom link?
#' @param room What is the room?
#' @inheritParams get_user_id
#'
#' @examples
#' format_meeting_info("Sebastian Funk")
#' format_meeting_info("Seb")
#' format_meeting_info("Sebastian Funk", "someone")
#' format_meeting_info(random = "Sebastian Funk")
#'
#' @export
#'
format_meeting_info <- function(
  assignee = "", random = "", chair = "", notes = "", topic = "",
  zoom_link = "", room = "",
  gsheet_id = Sys.getenv("GSHEET_ID")
) {

  announcement <- ""

  if (nchar(assignee) > 0) {
    announcement <- tryCatch(
      glue(
        "{announcement}\n- presenting: <@{assignee_id}>",
        assignee_id = get_user_id(assignee, gsheet_id)),
      error = function(e) {
        glue("{announcement}\n- presenting: {assignee}")
      }
    )
  }
  if (nchar(random) > 0) {
    announcement <- tryCatch(
      glue(
        "{announcement}\n- talking about something interesting: <@{random_id}>",
        random_id = get_user_id(random, gsheet_id)
      ),
      error = function(e) {
        glue("{announcement}\n- talking about something interesting: {random}")
      }
    )
  }
  if (nchar(chair) > 0) {
    announcement <- tryCatch(
      glue(
        "{announcement}\n- chairing: <@{chair_id}>",
        chair_id = get_user_id(chair, gsheet_id)
      ),
      error = function(e) {
        glue("{announcement}\n- chairing: {chair}")
      }
    )
  }
  if (nchar(notes) > 0) {
    announcement <- tryCatch(
      glue(
        "{announcement}\n- note taking: <@{notes_id}>",
        notes_id = get_user_id(notes, gsheet_id)
      ),
      error = function(e) {
        glue("{announcement}\n- note taking: {notes}")
      }
    )
  }

  if (nchar(topic) > 0) {
    announcement <- glue("{announcement}\n- topic: {topic}")
  }

  if (nchar(zoom_link) > 0 || nchar(room) > 0) {
    loc_str <- "Join us"
    if (nchar(zoom_link) > 0) {
      loc_str <- paste(loc_str, "on", zoom_link)
      if (nchar(room) > 0) {
        loc_str <- paste(loc_str, "or")
      }
    }
    if (nchar(room) > 0) {
      loc_str <- paste(loc_str, "in room", room)
    }
    loc_str <- paste0(loc_str, ".")
    announcement <- glue("{announcement}\n{loc_str}")
  }

   return(announcement)
}
