#' Create announcement reminder for lab meeting
#'
#' @inheritParams create_announcement_msg
#' @param advance How long in advance the message is sent
#'
#' @importFrom praise praise
#' @importFrom glue glue
#'
#' @export
#'
#' @examples
#' create_announcement_reminder()
#' create_announcement_reminder("Sebastian Funk")
#' create_announcement_reminder("Seb")
#' create_announcement_reminder("Sebastian Funk", "someone")
#' create_announcement_reminder(random = "Sebastian Funk")
#' create_announcement_reminder(chair = "Sebastian Funk", zoom_link = "https://example.com")
#'
create_announcement_reminder <- function(
  ...,
  topic = "",
  advance = "ca. 15 minutes"
) {

  meeting_info <- format_meeting_info(..., topic = topic)

  if (...length() == 0 || meeting_info == "") {
    return(NULL)
  }

  if (isTRUE(startsWith(topic, "[OFF]"))) {
    reason <- sub("\\[OFF\\] *", "", topic)
    announcement <- glue(
      "Reminder: no lab meeting today ({reason})."
    )
    conclusion <- "See you next time!"
  } else {
    announcement <- glue("<!channel>, lab meeting is happening in {advance}.")
    announcement <- paste0(announcement, meeting_info)
    conclusion <- "See you there!"
  }

  return(glue("{announcement}\n {conclusion}"))
}
