#' Get slack user ID from their name
#'
#' @param name A character string with the name ("Firstname Lastname")
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
get_user_id <- function(name, oauth_token = Sys.getenv("SLACKBOT_OAUTH")) {

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

  return(target_id)

}
