#' Summarise scores
#'
#' @param scores table of scores, e.g. as created with [score_forecasts()]
#' @param report_date Date at which the scoring takes place
#' @param restrict_weeks Integer number of continuous weeks continuous weeks
#' leading up to the `report_date` the forecasts need to include to be
#' considered in the scoring.
#' @importFrom dplyr group_by mutate ungroup filter select bind_rows count summarise left_join select across n_distinct full_join distinct starts_with
#' @importFrom tidyr complete replace_na
#' @importFrom lubridate weeks
#' @importFrom scoringutils pairwise_comparison
#'
#' @autoglobal
#'
#' @export
summarise_scores <- function(scores, report_date, restrict_weeks = 0L) {

  last_forecast_date <- report_date - 7

  locations <- scores %>%
    select(location, location_name) %>%
    distinct()

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
    group_by(target_variable) %>%
    mutate(nall = n_distinct(location_name)) %>%
    ungroup() %>%
    filter(location != "Overall" | n >= nall / 2) %>%
    select(-n, -nall)

  ## continuous weeks of submission leading up to the present
  if (restrict_weeks > 0) {
    cont_weeks <- score_df |>
      group_by(forecast_date, model, location, target_variable, horizon) |>
      summarise(present = 1, .groups = "drop") |>
      complete(model, location, target_variable, horizon, forecast_date) |>
      group_by(forecast_date, location, target_variable, horizon) |>
      mutate(all_na = all(is.na(present))) |>
      ungroup() |>
      filter(!all_na) |>
      group_by(model, location, target_variable, horizon) |>
      mutate(continuous_weeks = cumsum(rev(present))) |>
      filter(!is.na(continuous_weeks)) |>
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

  if (nrow(score_df) == 0) {
    return(tibble(
      model = character(0),
      target_variable = character(0),
      horizon = integer(0),
      location = character(0),
      location_name = character(0)
    ))
  }

  ## number of forecasts
  num_fc <- score_df %>%
    count(model, target_variable, horizon, location)

  rel_ae <- score_df %>%
    select(model, target_variable, horizon, location, location_name,
           forecast_date, ae_median) %>%
    pairwise_comparison(
      metric = "ae_median",
      baseline = "EuroCOVIDhub-baseline",
      by = c("model", "target_variable", "horizon", "location"),
    ) %>%
    select(model, target_variable, horizon, location, rel_ae = scaled_rel_skill) %>%
    distinct()

  pairwise_interval_scores <- score_df %>%
    filter(n_quantiles == 23) %>%
    select(model, target_variable, horizon, location, location_name,
           forecast_date, interval_score = wis) %>%
    pairwise_comparison(
      metric = "interval_score",
      baseline = "EuroCOVIDhub-baseline",
      by = c("model", "target_variable", "horizon", "location"),
    )

  rel_wis <- pairwise_interval_scores %>%
    select(model, target_variable, horizon, location,
           rel_wis = scaled_rel_skill) %>%
    distinct()

  msr_baseline <- pairwise_interval_scores %>%
    filter(compare_against == "EuroCOVIDhub-baseline") %>%
    select(model, target_variable, horizon, location,
           msr_baseline = mean_scores_ratio)

  ## calibration metrics (50 and 95 percent coverage and bias)
  coverage <- score_df %>%
    group_by(model, target_variable, horizon, location) %>%
    summarise(
      across(starts_with("cov_"), ~ mean(.x, na.rm = TRUE)), .groups = "drop"
    )
  bias <- score_df %>%
    filter(n_quantiles == 23) %>%
    group_by(model, target_variable, horizon, location) %>%
    summarise(bias = mean(bias), .groups = "drop")

  table <- rel_ae %>%
    full_join(rel_wis, by = c("model", "target_variable", "horizon", "location")) %>%
    full_join(msr_baseline, by = c("model", "target_variable", "horizon", "location")) %>%
    full_join(coverage, by = c("model", "target_variable", "horizon", "location")) %>%
    full_join(bias, by = c("model", "target_variable", "horizon", "location")) %>%
    left_join(num_fc, by = c("model", "target_variable", "horizon", "location")) %>%
    left_join(num_loc, by = c("model", "target_variable", "horizon", "location")) %>%
    left_join(locations, by = "location") %>%
    mutate(across(any_of(c("bias", "rel_wis", "msr_baseline", "rel_ae",
                           "cov_50", "cov_95")),
                      round, 2))

  return(table)
}
