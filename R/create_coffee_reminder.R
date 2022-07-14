#' Create a reminder for the weekly coffee
#'
#' @importFrom glue glue
#'
#' @inheritParams create_announcement_reminder
#'
#' @examples
#' create_coffee_reminder(zoom_link = "https://example.com")
#'
#' @export
#'
create_coffee_reminder <- function(zoom_link = NULL) {

  drinks <- c(
    "🥛",
    "☕",
    "🫖",
    "🍵",
    "🥤",
    "🧋",
    "🧃",
    "🧉"
  )

  intro <- glue(
    "Hey there, it's time for our weekly drink & chat {sample(drinks, 1)}."
  )

  location <- glue("Join us on {zoom_link}!")

  return(paste(intro, location))
}
