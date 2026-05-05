#' Format meeting info as a simple bullet list
#'
#' @importFrom glue glue
#'
#' @param assignee Who is presenting this week? (one or more comma-separated
#'   full names; "Everyone" maps to a channel-wide ping)
#' @param random Who is in charge of talking about a random interesting thing?
#' @param chair Who will chair this meeting?
#' @param topic Topic / kind of meeting (e.g. "Project updates", "Journal Club")
#' @param zoom_link What is the zoom link?
#' @param room What is the room?
#' @inheritParams get_user_id
#'
#' @examples
#' format_meeting_info("Sebastian Funk", topic = "Project updates")
#' format_meeting_info("Sebastian Funk, Liza Hadley", topic = "Project updates")
#' format_meeting_info(topic = "Coffee walk")
#'
#' @export
#'
format_meeting_info <- function(
  assignee = "", random = "", chair = "", topic = "",
  zoom_link = "", room = "",
  gsheet_id = Sys.getenv("GSHEET_ID")
) {

  ## one or more comma-separated names; tag each, erroring loudly if any
  ## are unknown so the announcement isn't sent with a misspelled name.
  ## "Everyone" is a special token that maps to a channel-wide ping.
  ## When no Slack-IDs sheet is configured (e.g. in examples / R CMD check)
  ## fall back to the plain name rather than failing.
  tag_people <- function(names_str, gsheet_id) {
    parts <- trimws(strsplit(names_str, ",")[[1]])
    parts <- parts[nchar(parts) > 0]
    vapply(parts, function(name) {
      if (tolower(name) == "everyone") return("<!channel>")
      if (nchar(gsheet_id) == 0) return(name)
      paste0("<@", get_user_id(name, gsheet_id), ">")
    }, character(1))
  }

  announcement <- ""

  ## Topic line: combine topic and speakers as "TOPIC (@speakers)" or "TOPIC".
  has_assignee <- nchar(assignee) > 0
  has_topic <- nchar(topic) > 0
  topic_line <- if (has_topic) topic else ""
  if (has_assignee) {
    tags <- tag_people(assignee, gsheet_id)
    if (length(tags) > 0) {
      tag_str <- paste(tags, collapse = ", ")
      topic_line <- if (nchar(topic_line) > 0) {
        paste0(topic_line, " (", tag_str, ")")
      } else {
        tag_str
      }
    }
  }
  if (nchar(topic_line) > 0) {
    announcement <- glue("{announcement}\n- {topic_line}")
  }

  if (nchar(random) > 0) {
    random_tag <- tag_people(random, gsheet_id)
    announcement <- glue(
      "{announcement}\n- talking about something interesting: ",
      "{paste(random_tag, collapse = ', ')}"
    )
  }
  if (nchar(chair) > 0) {
    chair_tag <- tag_people(chair, gsheet_id)
    announcement <- glue(
      "{announcement}\n- chairing: {paste(chair_tag, collapse = ', ')}"
    )
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
