#' Create announcement message for lab meeting
#'
#' @param assignee Who is presenting this week?
#' @param topic What is the topic of this week's presentation?
#' @param random Who is charge of talking about a random interesting thing?
#'
#' @importFrom praise praise
#' @importFrom glue glue
#'
#' @export
#'
#' @examples
#' create_announcement_msg()
#' create_announcement_msg("Seb")
#' create_announcement_msg("Seb", "the number pi")
#' create_announcement_msg("Seb", "the number pi", "someone")
#'
create_announcement_msg <- function(
  assignee = NULL, topic = NULL, random = NULL
) {
  greeting <- "Hello ${adjective} Epiforecasts members"

  question <- "What are your plans for this ${adjective} week"

  if (is.null(assignee) || is.na(assignee)) {
    return(praise(glue("{greeting}! {question}?")))
  }

  lab_meeting_day <- "Thursday"

  if (is.null(topic) || is.na(topic)) {
    prez <- "a presentation"
  } else {
    prez <- glue("a presentation about '{topic}'")
  }

  announcement <- glue(praise(
    "@{assignee} is ${creating} {prez} for our meeting on {lab_meeting_day}"
  ))

  if (!is.null(random)) {
    announcement <- glue(
    "{announcement}, and @{random} will tell us about something interesting"
    )
  }

  conclusion <- "This is going to be ${adjective}!"

  praise(glue("{greeting}! {question}?\n {announcement}. {conclusion}"))

}


