##' Adds hospitalisation status to a data frame obtained using
##' `covidHubUtils::load_truth`
##' @param x A data frame to add hospitalisation status to
##' @return an updated data frame
##' @importFrom dplyr mutate group_by ungroup summarise left_join bind_rows
##' @importFrom tidyr replace_na
##' @importFrom readr read_csv
##' @author Sebastian Funk
##' @export
add_status <- function(x) {
  hosp_status <- readr::read_csv(
    paste0(
      "https://raw.githubusercontent.com/covid19-forecast-hub-europe/",
      "covid19-forecast-hub-europe/main/data-truth/OWID/",
      "truth_OWID-Incident%20Hospitalizations.csv"
  ), show_col_types = FALSE) |>
    dplyr::mutate(target_variable = "inc hosp") |>
    dplyr::select(target_variable, location, target_end_date = date, status)
  case_status <- readr::read_csv(
    paste0(
      "https://raw.githubusercontent.com/covid19-forecast-hub-europe/",
      "covid19-forecast-hub-europe/main/data-truth/ECDC/",
      "truth_ECDC-Incident%20Cases.csv"
  ), show_col_types = FALSE) |>
    dplyr::mutate(target_variable = "inc case") |>
    dplyr::select(target_variable, location, target_end_date = date, status)
  death_status <- readr::read_csv(
    paste0(
      "https://raw.githubusercontent.com/covid19-forecast-hub-europe/",
      "covid19-forecast-hub-europe/main/data-truth/ECDC/",
      "truth_ECDC-Incident%20Cases.csv"
  ), show_col_types = FALSE) |>
    dplyr::mutate(target_variable = "inc death") |>
    dplyr::select(target_variable, location, target_end_date = date, status)

  status <- dplyr::bind_rows(hosp_status, case_stauts, death_status)

  x <- x |>
    dplyr::left_join(
      status, by = c("target_variable", "location", "target_end_date")
    ) |>
    tidyr::replace_na(list(status = "final"))
  return(x)
}
