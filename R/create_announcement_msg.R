#' Create announcement message for lab meeting
#'
#' @param assignee Who is presenting this week?
#'
#' @import praise praise
#' @import glue glue
#'
#' @export
#'
#' @examples
#' create_announcement_msg("Seb")
#'
create_announcement_msg <- function(assignee) {
  greeting <- "Hello ${adjective} Epiforecasts members"

  question <- "What are your plans for this ${adjective} week"

  lab_meeting_day <- "Thursday"
  announcement <- glue(praise(
    "@{assignee} is ${creating} a presentation for our meeting on {lab_meeting_day}"
  ))

  conclusion <- "This is going to be ${adjective}!"

  praise(glue("{greeting}! {question}?\n {announcement}. {conclusion}"))

}


