#' Load models according to inclusion criteria before ensembling
#'
#' @inheritParams create_ensemble_average
#' @param exclude_models optional character vector to exclude over all dates,
#'   or data.frame with cols model and forecast_date, to exclude for specific
#'    dates
#' @param exclude_designated_other logical: whether to exclude models designated
#' as "other" in their metadata file (default `TRUE`)
#' @param return_criteria logical : whether to return a model/inclusion criteria
#' grid as well as the ensemble forecast (default `TRUE`)
#' @param eval_dir character: the path in which to look for evaluation csv files
#' @param rel_wis_cutoff numeric: any model with relative WIS greater than this
#' value will be excluded
#' @param horizons numeric: which horizons to require for inclusion in the
#' ensemble; if NULL (default), all horizons specified in the config file
#' will be required
#' @param config_file character: location of the config file; a URL can be given. By 
#' default it will be assumed this command is run inside a clone of a forecast
#' hub directory with a "project-config.json" file at its root
#'
#' @return
#' - if `return_criteria = TRUE`, a list with the following elements
#'   * "ensemble" : tibble : a single ensemble forecast
#'   * "criteria": tibble : all candidate models against criteria
#'     for inclusion in ensemble (all locations and horizons)
#'   * "forecast_date" : date : latest date
#' - if `return_criteria = FALSE`, a tibble of a single ensemble forecast
#'
#' @details
#' Steps:
#' Currently, models included based on having:
#' 1. All quantiles
#' 2. 4 horizons
#' 3. Not manually specified for exclusion
#' 4. Not the hub ensemble
#'
#' @importFrom dplyr filter %>% group_by summarise mutate left_join select inner_join
#' @importFrom tidyr replace_na
#' @importFrom here here
#' @inheritParams get_hub_config
#'
#' @autoglobal
#'
#' @export
use_ensemble_criteria <- function(forecasts,
                                  exclude_models = NULL,
                                  exclude_designated_other = TRUE,
                                  return_criteria = TRUE,
                                  eval_dir = here::here("evaluation", "weekly-summary"),
                                  rel_wis_cutoff = Inf,
                                  horizons = NULL,
                                  config_file = here::here("project-config.json")) {

  # Remove point forecasts
  forecasts <- filter(forecasts, type == "quantile")

  # 1. Identify models with all quantiles
  quantiles <- get_hub_config(
    "forecast_type", config_file = config_file
  )$quantiles
  all_quantiles <- forecasts %>%
    # Check all quantiles per target/location
    group_by(model, target_variable, location, target_end_date) %>%
    summarise(all_quantiles_present =
                (length(setdiff(quantiles, quantile)) == 0),
              .groups = "drop") %>%
    # Check all quantiles at all horizons
    group_by(model, target_variable, location) %>%
    summarise(all_quantiles_all_horizons = all(all_quantiles_present),
              .groups = "drop")

  # 2. Identify models with all horizons
  if (is.null(horizons)) {
    horizons <- get_hub_config("horizon", config_file = config_file)$values
  }
  all_horizons <- forecasts %>%
    group_by(model, target_variable, location) %>%
    summarise(all_horizons =
                (length(setdiff(horizons, horizon)) == 0),
              .groups = "drop")
  forecasts <- forecasts %>%
    filter(horizon %in% horizons)

  # 3. Manually excluded forecasts
  criteria <- all_quantiles %>%
    left_join(all_horizons,
              by = c("model", "target_variable", "location")) %>%
    mutate(not_excluded_manually = !(model %in% exclude_models)) %>%
  # 4. Drop hub models
    filter(!grepl("^EuroCOVIDhub-", model))

  # 5. Drop "other" designated models
  if (exclude_designated_other) {
    not_other <- get_model_designations(here()) %>%
      filter(designation != "other")
    criteria <- criteria %>%
      filter(model %in% not_other$model)
  }

  # 6. Cut-off by relative WIS
  if (is.finite(rel_wis_cutoff)) {
    evaluation_date <- max(forecasts$forecast_date)
    evaluation_file <- file.path(eval_dir, paste0("evaluation-", evaluation_date, ".csv"))
    if (file.exists(evaluation_file)) {
      evaluation <- vroom(evaluation_file) %>%
        filter(!is.na(rel_wis)) %>%
        group_by(model, target_variable, location) %>%
        summarise(rel_wis = all(rel_wis < rel_wis_cutoff), .groups = "drop")
      criteria <- criteria %>%
        left_join(evaluation, by = c("model", "target_variable", "location")) %>%
        replace_na(list(rel_wis = FALSE))
    } else {
      warning("Evaluation file ", evaluation_file, " does not exist. ",
              "Not excluding any model by relative WIS.")
      criteria$rel_wis <- TRUE
    }
  } else {
    criteria$rel_wis <- TRUE
  }

  # Clarify inclusion and exclusion for all models by location/variable
  include <- filter(criteria,
                    all_quantiles_all_horizons &
                      all_horizons &
                      not_excluded_manually &
                      rel_wis) %>%
    select(model, target_variable, location) %>%
    mutate(included_in_ensemble = TRUE)

  if (nrow(include) == 0) {
    warning("No models left after applying inclusion criteria.")
  }

  criteria <- left_join(criteria, include,
                        by = c("model", "target_variable", "location")) %>%
    mutate(included_in_ensemble = ifelse(is.na(included_in_ensemble),
                                         FALSE,
                                         included_in_ensemble))


  # Return
  forecasts <- inner_join(forecasts, include,
                          by = c("model", "target_variable", "location"))

  if (return_criteria) {
    return(list("forecasts" = forecasts,
                "criteria" = criteria))
  }

  return(forecasts)
}
