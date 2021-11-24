#' Update list of slack user IDs
#'
#' @param oauth_token The OAuth token to access
#' [slack users.list API endpoint](https://api.slack.com/methods/users.list)
#'
#' @importFrom httr2 request req_auth_bearer_token req_perform resp_body_json
#' @importFrom dplyr %>% bind_rows
#' @importFrom purrr pluck map
#' @importFrom stats setNames
#'
#' @export
#'
update_user_ids <- function(oauth_token = Sys.getenv("SLACKBOT_OAUTH")) {
  browser()

  all_users <- request("https://slack.com/api/users.list") %>%
    req_auth_bearer_token(oauth_token) %>%
    req_perform() %>%
    resp_body_json()

  all_ids <- all_users %>%
    pluck("members") %>%
    map(~ c("id" = .x[["id"]], "email" = .x$profile$email)) %>%
    bind_rows()

  target_email <- name %>%
    strsplit(" ") %>%
    pluck(1) %>%
    setNames(c("firstname", "lastname")) %>%
    glue::glue_data("{firstname}.{lastname}@lshtm.ac.uk") %>%
    tolower()

  target_id <- all_ids$id[which(tolower(all_ids$email) == target_email)]

  if (nchar(target_id) == 0) {
    stop("Could not find an id for this person", call. = FALSE)
  }

  return(target_id)

}
