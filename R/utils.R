weighted_mean <- function(x, weights) {
  return(sum(x * weights) / sum(weights))
}

weighted_median <- function(x, weights) {
  return(cNORM::weighted.quantile(x, probs = 0.5, weights = weights))
}

#' Compute weighted mean/median
#'
#' @param ... values to average
#' @param average Method to average. One of `"mean"` or `"median"`
#'
weighted_average <- function(..., average = c("mean", "median")) {

  average <- match.arg(average)

  if (average == "mean") {
    return(weighted_mean(...))
  } else if (average == "median") {
    return(weighted_median(...))
  }

}

##' Convert a date to the last date of the week it's in
##'
##' The day on which the week ends is given by the config variable
##' \code{week_end_day}
##' @param x date to convert
##  @inheritParams get_hub_config
##'
##' @importFrom here here
##' @importFrom lubridate wday ceiling_date
##' @return converte date
##' @author Sebastian Funk
##' @export
date_to_week_end <- function(x,
                             type = c("target", "forecast"),
                             config_file = here::here("project-config.json")) {
  type <- match.arg(type)
  if (type == "target") {
    config_var <- "week_end_day"
  } else if (type == "forecast") {
    config_var <- "forecast_week_day"
  }
  week_end_day <- get_hub_config(config_var, config_file = config_file)
  week_end_wday <- which(
    levels(lubridate::wday(1, label = TRUE, abbr = FALSE, week_start = 1)) ==
      week_end_day
  )

  start_week_day <- (week_end_wday %% 7) + 1L

  week_end_date <- lubridate::ceiling_date(
    x, unit = "week", week_start = start_week_day
  ) - 1

  return(week_end_date)
}
