##' Adds hospitalisation status to a data frame obtained using
##' `covidHubUtils::load_truth`
##' @param x A data frame to add hospitalisation status to
##' @return an updated data frame
##' @importFrom dplyr mutate group_by ungroup summarise left_join
##' @importFrom tidyr replace_na
##' @importFrom readr read_csv
##' @author Sebastian Funk
##' @export
add_hosp_status <- function(x) {
  hosp_status <- readr::read_csv(here::here(
    "data-truth", "OWID", "truth_OWID-Weekly Incident hospitalizations.csv"
  ), show_col_types = FALSE)
  x <- x |>
    dplyr::left_join(
      hosp_status, by = c("target_variable", "location", "target_end_date")
    ) |>
    tidyr::replace_na(list(status = "final"))
  return(x)
}
