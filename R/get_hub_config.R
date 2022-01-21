#' Get the value of setting from config file
#'
#' @param setting Setting for which to return the value. All values are returned
#' in a list if missing.
#' @param config_file Path to the config file
#'
#' @export
#'
#' @examples
#' # Get config for European COVID-19 forecast hub
#' get_hub_config(config_file = "https://raw.githubusercontent.com/epiforecasts/covid19-forecast-hub-europe/main/forecasthub.yml")
#'
get_hub_config <- function(setting, config_file = here::here("project-config.json")) {

  if (missing(setting)) {
    jsonlite::read_json(config_file, simplifyVector = TRUE)
  }

  else {
    jsonlite::read_json(config_file, simplifyVector = TRUE)[[setting]]
  }
}

