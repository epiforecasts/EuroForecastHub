##' Adds hospitalisation status to a data frame obtained using
##' `covidHubUtils::load_truth`
##' @param x A data frame to add hospitalisation status to
##' @return an updated data frame
##' @importFrom dplyr mutate group_by ungroup summarise left_join
##' @importFrom tidyr replace_na
##' @author Sebastian Funk
##' @export
add_hosp_status <- function(x) {
  hosp_status <- readr::read_csv(here::here(
    "data-truth", "OWID", "truth_OWID-Incident hospitalizations.csv"
  ), show_col_types = FALSE) |>
    dplyr::mutate(
      sat_date = ceiling_date(x = date, unit = "week", week_start = 7) -
        days(1),
      target_variable = "inc hosp"
    ) |>
    dplyr::group_by(target_variable, location, sat_date) |>
    dplyr::mutate(n = n()) |> ## count observations per Saturday date
    dplyr::group_by(target_variable, location) |>
    ## check if data is weekly or daily
    dplyr::mutate(frequency = dplyr::if_else(all(n == 1), "weekly", "daily")) |>
    dplyr::ungroup() |>
    ## if weekly and end date is previous Sunday, make end date the Saturday
    ## instead, i.e. interpret Mon-Sun as Sun-Sat
    dplyr::mutate(
      target_end_date = dplyr::if_else(
        frequency == "weekly" & date + 6 == sat_date,
        date + 6, date
      )
    ) |>
    dplyr::group_by(target_variable, location, target_end_date = sat_date) |>
    dplyr::summarise(status = dplyr::if_else(
      any(status == "expecting revisions"), "expecting revisions",
      dplyr::if_else(
        any(status == "near final"), "near final", "final"
      )
    ), .groups = "drop")
  x <- x |>
    dplyr::left_join(
      hosp_status, by = c("target_variable", "location", "target_end_date")
    ) |>
    tidyr::replace_na(list(status = "final"))
  return(x)
}
