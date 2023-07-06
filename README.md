## EuroForecastHub package

Functions to support the European COVID-19 [Forecast](https://github.com/covid19-forecast-hub-europe/covid19-forecast-hub-europe) and [Scenario](https://github.com/covid19-forecast-hub-europe/covid19-scenario-hub-europe) Hubs. For similar functionality, see [covidHubUtils](https://github.com/reichlab/covidHubUtils).

Install the package with:

```
remotes::install_github("covid19-forecast-hub-europe/EuroForecastHub")
```

### Functionality

Use `devtools::build_manual()` to see complete function documentation.

Hub metadata
- `get_hub_config()`
- `get_model_designations()`
- `link_preview()`

Observed data handling
- `add_status()`
- `date_to_week_end()`
- `convert_to_weekly()`

Creating ensembles
- `weighted_average()`
- `create_ensemble_average()`
- `create_ensemble_relative_skill()`
- `use_ensemble_criteria()`
- `add_point_forecasts()`
- `format_ensemble()`
- `run_ensemble()`
- `run_multiple_ensembles()`

Forecast scoring
- `score_forecasts()`
- `summarise_scores()`

