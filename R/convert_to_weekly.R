##' Convert truth data from daily to weekly
##'
##' @param x data to convert
##' @param ... any parameters for [date_to_week_end()]
##' @return converted data frame
##' @importFrom dplyr bind_rows filter mutate group_by ungroup select if_else across
##' @importFrom lubridate days
##' @author Sebastian Funk
##' @export
convert_to_weekly <- function(x, ...) {
  x |>
    dplyr::mutate(sat_date = date_to_week_end(date, ...)) |>
    dplyr::group_by(
      dplyr::across(c(-date, -value, -status, -snapshot_date))
    )  |>
    dplyr::mutate(n = dplyr::n())  |> ## count observations per Saturday date
    dplyr::group_by(
      dplyr::across(c(-date, -value, -status, -snapshot_date, -sat_date, -n))
    )  |>
    ## check if data is weekly or daily
    dplyr::mutate(
      frequency = dplyr::if_else(all(n == 1), "weekly", "daily")
    ) |>
    dplyr::ungroup() |>
    ## if weekly and end date is previous Sunday, make end date the Saturday
    ## instead, i.e. interpret Mon-Sun as Sun-Sat
    dplyr::group_by(location) |>
    dplyr::filter(n == max(tail(n, 7)) | frequency == "weekly") |>
    dplyr::mutate(date = dplyr::if_else(
      frequency == "weekly" & date + 6 == sat_date, date + 6, date
    )) |>
    dplyr::group_by(
      dplyr::across(c(-date, -value, -status, -snapshot_date))
    ) |>
    dplyr::filter(date == max(date)) |>
    dplyr::ungroup() |>
    dplyr::select(-sat_date, -n, -frequency)
}
