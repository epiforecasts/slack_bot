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
  zoom_link = NULL,
  room = NULL
) {

  announcement <- glue(
    "<!channel>, lab meeting is happening in 5 minutes!"
  )

  announcement <- paste0(announcement, format_meeting_info(...))

  if (is.null(zoom_link) && is.null(room)) {
    return(announcement)
  }

  loc_str <- "Join us"
  if (!is.null(zoom_link)) {
    loc_str <- paste(loc_str, "on {zoom_link}")
    if (!is.null(room)) {
      loc_str <- paste(loc_str, "or")
    }
  }
  if (!is.null(room)) {
    loc_str <- paste(loc_str, "in room", room)
  }
  loc_str <- paste0(loc_str, "!")
  location <- glue(loc_str)

  return(paste(announcement, location, sep = "\n"))

}


