#' Summarise station locations and catch
#'
#' Joins station data to length distributions to produce per-station biomass
#' estimates, used to visualise survey coverage and zero-catch stations.
#'
#' @param tbl A dplyr query from the station table
#' @param ldist A dplyr query from the ldist table, pre-processed with
#'   [pax_ldist_scale_abund()] and [pax_ldist_add_weight()]
#' @return A dplyr query with columns ``sample_id``, ``begin_lat``,
#'   ``begin_lon``, ``year``, ``sampling_type``, ``species``, ``bio``
#'   (biomass index per station), and ``zero_station`` (``"Zero catch"`` or
#'   ``"Non zero"``)
# Was: tidypax::survey_locations
pax_station_location_summary <- function(
  tbl,
  ldist = dplyr::tbl(dbplyr::remote_con(tbl), "ldist") |>
    pax_ldist_scale_abund() |>
    pax_ldist_add_weight()
) {
  pcon <- dbplyr::remote_con(tbl)
  species_dummies <-
    tibble::tibble(species = 1:100, dummy = 1)

  # NSE variables
  sample_id <- NULL
  begin_lat <- NULL
  begin_lon <- NULL
  year <- NULL
  sampling_type <- NULL
  species <- NULL
  count <- NULL
  a <- NULL
  b <- NULL
  tow_length <- NULL
  bio <- NULL

  tbl |>
    dplyr::left_join(ldist, by = c("sample_id", "species")) |>
    dplyr::mutate(dummy = 1) |>
    dplyr::left_join(
      pax_temptbl(pcon, species_dummies),
      by = c('species', 'dummy')
    ) |>
    dplyr::group_by(
      sample_id,
      begin_lat,
      begin_lon,
      year,
      sampling_type,
      species
    ) |>
    dplyr::summarise(
      bio = sum(
        abs(
          coalesce(count, 0) *
            coalesce(a, 0.01) *
            abs(coalesce(length, 0))^coalesce(b, 3)
        ) /
          abs(coalesce(pmax(tow_length, 0.1), 4)),
        na.rm = TRUE
      ) /
        1e3
    ) |>
    dplyr::mutate(zero_station = ifelse(bio == 0, 'Zero catch', 'Non zero')) |>
    dplyr::ungroup()
}
