#' Create announcement reminder for lab meeting
#'
#' @inheritParams create_announcement_msg
#' @param zoom_link The link to the zoom room where the meeting is taking place
#'
#' @import praise praise
#' @import glue glue
#'
#' @export
#'
#' @examples
#' create_announcement_reminder()
#' create_announcement_reminder("Seb")
#' create_announcement_reminder("Seb", "the number pi")
#'
create_announcement_reminder <- function(assignee = NULL, topic = NULL, zoom_link = NULL) {

  if (is.null(assignee) || is.na(assignee)) {
    return()
  }

  if (is.null(topic) || is.na(topic)) {
    prez <- "presentation"
  } else {
    prez <- glue("presentation about '{topic}'")
  }

  announcement <- glue(
    "@{assignee}'s {prez} is happening now!"
  )

  if (is.null(zoom_link)) {
    return(announcement)
  }

  location <- "Join us on {zoom_id}!"

  return(paste(announcement, location, sep = "\n"))

}


