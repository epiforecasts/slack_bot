#' Create announcement message for lab meeting
#'
#' @param ... Arguments passed to [format_meeting_info()]
#'
#' @importFrom praise praise
#' @importFrom glue glue
#'
#' @export
#'
#' @examples
#' create_announcement_msg()
#' create_announcement_msg("Sebastian Funk")
#' create_announcement_msg("Seb")
#' create_announcement_msg("Sebastian Funk", "someone")
#'
create_announcement_msg <- function(
  ...
) {
  greeting <- "Hello ${adjective} Epiforecasts members"

  meeting_info <- format_meeting_info(...)

  if (...length() == 0 || meeting_info == "") {
    return(NULL)
  }

  lab_meeting_day <- "Thursday"

  announcement <- glue(
    "For our meeting on {lab_meeting_day}:"
  )

  announcement <- paste0(announcement, meeting_info)

  conclusion <- "This is going to be ${adjective}!"

  praise(glue("{greeting}!\n {announcement}\n {conclusion}"))

}


