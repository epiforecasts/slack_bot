#' Format meeting info as a simple bullet list
#'
#' @importFrom glue glue
#'
#' @param assignee Who is presenting this week?
#' @param random Who is charge of talking about a random interesting thing?
#' @param chair Who will chair this meeting?
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
  assignee = NULL, random = NULL, chair = NULL,
  gsheet_id = Sys.getenv("GSHEET_ID")
) {

  announcement <- ""

  if (!is.null(assignee) && !is.na(assignee)) {
    announcement <- tryCatch(
      glue(
        "{announcement}\n- presenting: @<{assignee_id}>",
        assignee_id = get_user_id(assignee, gsheet_id)),
      error = function(e) {
        glue("{announcement}\n- presenting: {assignee}")
      }
    )
  }
  if (!is.null(random) && !is.na(random)) {
    announcement <- tryCatch(
      glue(
        "{announcement}\n- talking about something interesting: @<{random_id}>",
        random_id = get_user_id(random, gsheet_id)
      ),
      error = function(e) {
        glue("{announcement}\n- talking about something interesting: {random}")
      }
    )
  }
  if (!is.null(chair) && !is.na(chair)) {
    announcement <- tryCatch(
      glue(
        "{announcement}\n- chairing: @<{chair_id}>",
        chair_id = get_user_id(chair, gsheet_id)
      ),
      error = function(e) {
        glue("{announcement}\n- chairing: {chair}")
      }
    )
  }

  return(announcement)
}
