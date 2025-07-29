#' Get a random prompt
#'
#' @importFrom glue glue
#'
#' @export
#' @autoglobal
#'
#' @examples
#' get_prompt()
#'
get_prompt <- function() {
  prompts <- readLines(file.path("data-raw", "prompts.txt"))
  prompt <- sample(prompts, 1)
  glue("**Now**: {prompt}")
}
