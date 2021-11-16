#' Create announcement reminder for lab meeting
#'
#' @inheritParams create_announcement_msg
#' @param zoom_link The link to the zoom room where the meeting is taking place
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
  zoom_link = NULL
) {

  announcement <- glue(
    "<!channel>, lab meeting is happening now!"
  )

  announcement <- paste0(announcement, format_meeting_info(...))

  if (is.null(zoom_link)) {
    return(announcement)
  }

  location <- glue("Join us on {zoom_link}!")

  return(paste(announcement, location, sep = "\n"))

}


