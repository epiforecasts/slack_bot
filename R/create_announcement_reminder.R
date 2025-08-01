#' Create announcement reminder for lab meeting
#'
#' @inheritParams create_announcement_msg
#'
#' @importFrom praise praise
#' @importFrom glue glue
#' @importFrom lubridate now ymd_hm today
#' @importFrom dplyr between
#'
#' @export
#'
#' @examples
#' time <- format(lubridate::now() + lubridate::minutes(15), "%H:%M")
#' create_announcement_reminder(time = time)
#' create_announcement_reminder("Sebastian Funk", time = time)
#' create_announcement_reminder("Seb", time = time)
#' create_announcement_reminder("Sebastian Funk", "someone", time = time)
#' create_announcement_reminder(random = "Sebastian Funk", time = time)
#' create_announcement_reminder(
#'   chair = "Sebastian Funk",
#'   zoom_link = "https://example.com",
#'   time = time
#' )
#'
create_announcement_reminder <- function(...,
                                         topic = "",
                                         time = "") {

  # Check if meeting time is within the next 30 minutes
  if (time == "") {
    return(NULL)
  }

  meeting_time <- ymd_hm(paste(today(), time), tz = "Europe/London")
  difference <- as.integer(difftime(
    meeting_time, now(tzone = "Europe/London"), units = "mins"
  ))
  if (!between(difference, 0, 30)) {
    return(NULL)
  }

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
    announcement <- glue(
      "<!channel>, lab meeting is happening in ca. {difference} minutes."
    )
    announcement <- paste0(announcement, meeting_info)
    conclusion <- "See you there!"
  }

  return(glue("{announcement}\n {conclusion}"))
}
