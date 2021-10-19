#' Create announcement reminder for lab meeting
#'
#' @inheritParams create_announcement_msg
#' @param chair Who will chair this meeting?
#' @param zoom_link The link to the zoom room where the meeting is taking place
#'
#' @importFrom praise praise
#' @importFrom glue glue
#'
#' @export
#'
#' @examples
#' create_announcement_reminder()
#' create_announcement_reminder("Seb")
#' create_announcement_reminder("Seb", "the number pi")
#' create_announcement_reminder("Seb", "the number pi", "someone")
#' create_announcement_reminder(random = "Seb")
#' create_announcement_reminder(chair = "Seb", zoom_link = "https://example.com")
#'
create_announcement_reminder <- function(
  assignee = NULL, topic = NULL, random = NULL, chair = NULL, zoom_link = NULL
) {

  announcement <- glue(
    "<!channel>, lab meeting is happening now!"
  )

  if (!is.null(assignee) && !is.na(assignee)) {
    announcement <- glue("{announcement}\n- presenting: @{assignee}")
  }
  if (!is.null(random) && !is.na(random)) {
    announcement <- glue(
      "{announcement}\n- talking about something interesting: @{random}"
    )
  }
  if (!is.null(chair) && !is.na(chair)) {
    announcement <- glue(
      "{announcement}\n- chairing: @{chair}"
    )
  }

  if (is.null(zoom_link)) {
    return(announcement)
  }

  location <- glue("Join us on {zoom_link}!")

  return(paste(announcement, location, sep = "\n"))

}


