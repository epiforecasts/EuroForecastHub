#' Get the model designation of each model in a forecast hub
#'
#' Reads the metadata files to extract model_designation field
#'
#' @param hub_repo_path path to the local clone of the forecast repository
#'
#' @return a data frame of each "model", and the corresponding "designation"
#'
#' @importFrom yaml read_yaml
#'
#' @export
get_model_designations <- function(hub_repo_path) {

  metadata_files <- list.files(
    file.path("metadata"),
    full.names = TRUE
  )
  metadata <- purrr::map(metadata_files, yaml::read_yaml)

  models <- purrr::map_chr(metadata, "model_abbr")
  model_designations <- purrr::map_chr(metadata, "team_model_designation")

  return(data.frame(model = models, designation = model_designations))
}
