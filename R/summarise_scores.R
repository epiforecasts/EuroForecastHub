#' Summarise scores
#'
#' @param scores table of scores, e.g. as created with \code{\link{score_models}}
#' @param report_date Date at which the scoring takes place
#' @param restrict_weeks Integer number of continuous weeks continuous weeks
#' leading up to the `report_date` the forecasts need to include to be
#' considered in the scoring.
#'
#' @importFrom dplyr group_by mutate ungroup filter select bind_rows count summarise left_join select across n_distinct full_join
#' @importFrom tidyr complete replace_na
#' @importFrom lubridate weeks
#'
#' @export
summarise_scores <- function(scores, report_date, restrict_weeks = 0L) {

  last_forecast_date <- report_date - 7

  ## extract data to be scored and set number of locations to one as default (see next command)
  score_data <- scores %>%
    filter(forecast_date <= last_forecast_date,
           target_end_date <= report_date)

  ## duplicate country data as overall data
  score_df <- score_data %>%
    mutate(location = "Overall") %>%
    bind_rows(score_data)

  num_loc <- score_df %>%
    group_by(model, location, target_variable, horizon) %>%
    summarise(n_loc = n_distinct(location_name), .groups = "drop")

  ## for overall, if more than 1 location exists, filter to have at least half
  ## of them
  score_df <- score_df %>%
    group_by(model, target_variable, location, horizon) %>%
    mutate(n = n_distinct(location_name)) %>%
    ungroup() %>%
    mutate(nall = n_distinct(location_name)) %>%
    filter(location != "Overall" | n >= nall / 2) %>%
    select(-n, -nall)

  ## continuous weeks of submission
  if (restrict_weeks > 0) {
    cont_weeks <- score_df %>%
      group_by(model, location, target_variable, target_end_date, horizon) %>%
      summarise(present = 1, .groups = "drop") %>%
      complete(model, location, target_variable, target_end_date, horizon) %>%
      filter(forecast_date <= report_date - weeks(horizon)) %>%
      group_by(model, location, target_variable, horizon) %>%
      mutate(continuous_weeks = cumsum(rev(present))) %>%
      filter(!is.na(continuous_weeks)) %>%
      summarise(continuous_weeks = max(continuous_weeks), .groups = "drop")

    score_df <- score_df %>%
      left_join(cont_weeks, by = c(
                              "model", "target_variable", "horizon",
                              "location"
                            )) %>%
      replace_na(list(continuous_weeks = 0)) %>%
      filter(continuous_weeks >= restrict_weeks) %>%
      select(-continuous_weeks)
  }

  ## number of forecasts
  num_fc <- score_df %>%
    count(model, target_variable, horizon, location)

  locations <- score_df %>%
    select(location, location_name) %>%
    distinct()

  rel_ae <- score_df %>%
    select(model, target_variable, horizon, location, aem) %>%
    pairwise_comparison(
      metric = "aem",
      baseline = "EuroCOVIDhub-baseline",
      summarise_by = c("model", "target_variable", "horizon", "location")
    ) %>%
    select(model, target_variable, horizon, location,
           rel_ae = scaled_rel_skill) %>%
    distinct()

  rel_wis <- score_df %>%
    filter(n_quantiles == 23) %>%
    select(model, target_variable, horizon, location, interval_score = wis) %>%
    pairwise_comparison(
      metric = "interval_score",
      baseline = "EuroCOVIDhub-baseline",
      summarise_by = c("model", "target_variable", "horizon", "location")
    ) %>%
    select(model, target_variable, horizon, location, rel_wis = scaled_rel_skill) %>%
    distinct()

  ## calibration metrics (50 and 95 percent coverage and bias)
  coverage <- score_df %>%
    group_by(model, target_variable, horizon, location) %>%
    summarise(across(starts_with("cov_"), mean), .groups = "drop")
  bias <- score_df %>%
    filter(n_quantiles == 23) %>%
    group_by(model, target_variable, horizon, location) %>%
    summarise(bias = mean(bias), .groups = "drop")

  table <- rel_ae %>%
    full_join(rel_wis, by = c("model", "target_variable", "horizon", "location")) %>%
    full_join(coverage, by = c("model", "target_variable", "horizon", "location")) %>%
    full_join(bias, by = c("model", "target_variable", "horizon", "location")) %>%
    inner_join(locations, by = "location") %>%
    mutate(across(c("bias", "rel_wis", "rel_ae", "cov_50", "cov_95"), round, 2))

  return(table)
}
