#' Generate link to the preview shiny app for forecast submissions
#'
#' @param gh_repo GitHub repository address in the format `username/repo`
#' @param pr_number Number of the pull request to validate
#' @param data_folder Path to the folder containing forecasts in this `gh_repo`
#'
#' @return
#' A character vector containing one link for each of the forecast file
#' uploading in the PR #`pr_number`.
#'
#' @examples
#' link_preview(
#'   "epiforecasts/covid19-forecast-hub-europe",
#'   956
#' )
#'
#' @import gh gh
#'
#' @export
#'
link_preview <- function(gh_repo, pr_number, data_folder = "data-processed") {

  pr_files <- purrr::map_chr(
    gh(
      "/repos/{gh_repo}/pulls/{pr_number}/files",
      gh_repo = gh_repo,
      pr_number = pr_number
    ),
    "raw_url"
  )

  forecast_file_regex <- paste0(
    file.path(
      data_folder,
      "([a-zA-Z0-9_+]+-[a-zA-Z0-9_+]+)",
      "\\d{4}-\\d{2}-\\d{2}-\\1.csv"
    ),
    "$"
  )

  forecast_files <- grep(
    forecast_file_regex,
    pr_files,
    value = TRUE
  )

  preview_links <- paste0(
    "https://epiforecasts.shinyapps.io/ecdc_submission/?file=",
    forecast_files
  )

  return(preview_links)

}
