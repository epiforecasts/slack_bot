#' Create announcement message for lab meeting
#'
#' @param ... Arguments passed to [format_meeting_info()]
#' @param lab_meeting_day Day of the meeting
#' @param lab_meeting_time  Time of the meeting
#' @importFrom praise praise
#' @importFrom glue glue
#'
#' @inheritParams format_meeting_info
#
#' @export
#'
#' @examples
#' create_announcement_msg()
#' create_announcement_msg("Sebastian Funk")
#' create_announcement_msg("Seb")
#' create_announcement_msg("Sebastian Funk", "someone")
#'
create_announcement_msg <- function(
  ...,
  topic = "",
  lab_meeting_day = "Thursday",
  lab_meeting_time = "10:00 (UK time)"
) {
  meeting_info <- format_meeting_info(..., topic = topic)

  if (...length() == 0 || meeting_info == "") {
    return(NULL)
  }

  greeting <- "Hello ${adjective} Epiforecasts members"

  if (isTRUE(startsWith(topic, "[OFF]"))) {
    reason <- sub("\\[OFF\\] *", "", topic)
    announcement <- glue(
      "No lab meeting this week ({reason})."
    )
    conclusion <- "Hopefully you'll have a good week anyway!"
  } else {
    announcement <- glue(
      "For our meeting on {lab_meeting_day} at {lab_meeting_time}:"
    )

    announcement <- paste0(announcement, meeting_info)

    conclusion <- "This is going to be ${adjective}!"
  }

  praise(glue("{greeting}!\n {announcement}\n {conclusion}"))
}
