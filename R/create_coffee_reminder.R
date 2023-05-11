#' Create a reminder for the weekly coffee
#'
#' @importFrom glue glue
#'
#' @inheritParams format_meeting_info
#'
#' @examples
#' create_coffee_reminder(zoom_link = "https://example.com")
#'
#' @export
#'
create_coffee_reminder <- function(zoom_link = NULL) {

  drinks <- c(
    ":coffee:",
    ":tea:",
    ":teapot:",
    ":mate_drink:",
    ":glass_of_milk:",
    ":cup_with_straw:",
    ":bubble_tea:",
    ":beverage_box:"
  )

  intro <- glue(
    "Hey there, it's time for our weekly drink & chat {sample(drinks, 1)}."
  )

  location <- glue("Join us on {zoom_link}!")

  return(paste(intro, location))
}
