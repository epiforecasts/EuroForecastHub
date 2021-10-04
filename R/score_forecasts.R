#' Score forecasts
#'
#' @inheritParams create_ensemble_average
#' @param quantiles Numeric vector of quantiles to be considered in the scoring.
#'
#' @importFrom dplyr group_by mutate ungroup filter select summarise left_join select across if_else n_distinct rename relocate
#' @importFrom tidyr pivot_wider
#' @importFrom scoringutils eval_forecasts
#'
#' @export
score_forecasts <- function(forecasts, quantiles = NULL) {

  ## Extract unique locations contained in forecasts
  locations <- forecasts %>%
    select(location, location_name) %>%
    distinct()

  ## For AE we use the median if given, otherwise the point forecast.
  ## Here we first convert the point forecast to the median for any
  ## models that don't provide the median.
  point_forecasts <- forecasts %>%
    filter(type == "point") %>%
    mutate(type = "quantile", quantile = 0.5) %>%
    rename(point_prediction = prediction)
  join_cols <- setdiff(colnames(point_forecasts), "point_prediction")
  fc_df <- forecasts %>%
    filter(type == "quantile") %>%
    full_join(point_forecasts, by = join_cols) %>%
    mutate(prediction =
             if_else(is.na(prediction), point_prediction, prediction)) %>%
    select(-point_prediction) %>%
    filter(!is.na(true_value)) ## remove forecasts without truth (yet)

  ## Filter for permitted quantiles
  if (!is.null(quantiles)) {
    fc_df <- fc_df %>%
      filter(quantile %in% quantiles)
  }
   ## Calibration metrics (50 and 95 percent coverage and bias);
  ## 1 if truth within prediction interval, 0 otherwise
  coverage <- fc_df %>%
    eval_forecasts(
      summarise_by = c("model", "target_variable", "forecast_date",
                       "target_end_date", "range", "horizon", "location"),
      metrics = "coverage",
    ) %>%
    filter(range %in% c(50, 95)) %>%
    select(model, target_variable, forecast_date, target_end_date, horizon,
           location, coverage, range) %>%
    pivot_wider(
      names_from = range, values_from = coverage,
      names_prefix = "cov_"
    )

  ## bias: Bias (1 - 2 * (lowest quantile below or equal to the data))
  ## ae: absolute error of the median (or point forecast if not give)
  ## wis: weighted interval score
  scores <- fc_df %>%
    eval_forecasts(
      summarise_by = c("model", "target_variable", "forecast_date",
                       "target_end_date", "horizon", "location"),
      metrics = c("interval_score", "aem", "bias"),
    ) %>%
    select(model, target_variable, forecast_date, target_end_date,
           horizon, location, wis = interval_score,
           sharpness, underprediction, overprediction,
           aem, bias)

  ## record number of quantiles that are provided
  nq <- fc_df %>%
    group_by(location, target_variable, forecast_date, target_end_date,
             model, horizon) %>%
    summarise(n_quantiles = n_distinct(quantile), .groups = "drop")

  ## create table
  table <- scores %>%
    left_join(coverage, by = c(
                          "model", "target_variable", "forecast_date",
                          "target_end_date", "horizon", "location"
                        )) %>%
    left_join(nq, by = c(
                    "model", "target_variable", "forecast_date",
                    "target_end_date", "horizon", "location"
                  )) %>%
    left_join(locations, by = "location") %>%
    relocate(location_name, .after = location) %>%
    mutate(bias = round(bias, 1)) %>%
    mutate(across(c("wis", "sharpness", "underprediction",
                    "overprediction"), round))

  return(table)
}
