# TODO: Generic pax_add_group(lgroup / regions / gear_group / ...)
pax_add_lgroups <- function(tbl, lgroups, dl = 1) {
  UseMethod("pax_add_lgroups", as_pax(tbl))
}
pax_add_regions <- function(tbl, regions = list(all = 101:115)) {
  UseMethod("pax_add_regions", as_pax(tbl))
}
pax_add_depth_labels <- function(tbl, breaks = c(0, 100, 200, 300)) {
  UseMethod("pax_add_depth_labels", as_pax(tbl))
}
pax_add_gear_group <- function(tbl, gear_group = NULL) {
  UseMethod("pax_add_gear_group", as_pax(tbl))
}
pax_add_temporal_grouping <- function(tbl, tgroup = list(t1 = 1:6, t2 = 7:12)) {
  UseMethod("pax_add_gear_group", as_pax(tbl))
}
pax_add_yearly_grouping <- function(tbl, ygroup = NULL) {
  UseMethod("pax_add_gear_group", as_pax(tbl))
}
pax_add_strata <- function(
  tbl,
  use_total_area = 0,
  strata_stations = pax_dat_strata_stations(as_pax(tbl)),
  strata_attributes = pax_dat_strata_attributes(as_pax(tbl))
) {
  UseMethod("pax_add_strata", as_pax(tbl))
}

# Was: tidypax::add_lgroups
pax_add_lgroups.pax <- function(tbl, lgroups, dl = 1) {
  lgroup_tbl <-
    tidyr::expand_grid(length = seq(min(lgroups), max(lgroups), by = dl)) |>
    # TODO: consider making below:lgroups[cut(length,lgroups-1,include.lowest = TRUE, labels =FALSE)]
    dplyr::mutate(
      lgroup = lgroups[cut(
        length,
        lgroups,
        include.lowest = TRUE,
        labels = FALSE
      )]
    ) |>
    dplyr::group_by(lgroup) |>
    dplyr::mutate(mean_length = mean(length))

  tbl |>
    dplyr::left_join(pax_temptbl(as_pax(tbl), lgroup_tbl), by = 'length')
}

# Was: tidypax::add_regions
pax_add_regions.pax <- function(tbl, regions = list(all = 101:115)) {
  pcon <- as_pax(tbl)

  regions_tbl <- regions |>
    purrr::map(~ tibble::tibble(division = .)) |>
    dplyr::bind_rows(.id = 'region')

  tbl |>
    dplyr::left_join(
      pax_dat_reitmapping(pcon) |>
        dplyr::select(gridcell, division, subdivision) |>
        dplyr::left_join(pax_temptbl(pcon, regions_tbl))
    )
}

# TODO: Rename to depth_group?
# Was: tidypax::add_depth_labels
pax_add_depth_labels.pax <- function(tbl, breaks = c(0, 100, 200, 300)) {
  breaks <- sort(breaks)
  b_labs <- paste(breaks[-length(breaks)], breaks[-1], sep = '-')

  tbl |>
    dplyr::left_join(
      pax_dat_noaa_bathymetry(as_pax(tbl)) |>
        dplyr::group_by(gridcell) |>
        dplyr::summarise(noaa_depth = mean(noaa_depth)),
      by = 'gridcell'
    ) |>
    dplyr::mutate(
      depth = nvl(depth, -round(noaa_depth)),
      depth_class = case_when(
        is.na(depth) ~ 'Unknown',
        depth > local(max(breaks)) ~ paste0(local(max(breaks)), '+'),
        TRUE ~ cut(depth, breaks, labels = b_labs, include.lowest = TRUE)
      ),
      depth_class = nvl(depth_class, paste0(local(max(breaks)), '+'))
    )
}

# Was: tidypax::add_gear_group
pax_add_gear_group.pax <- function(tbl, gear_group = NULL) {
  if (is.null(gear_group)) {
    return(tbl |> dplyr::mutate(gear_name = 'all'))
  }

  gear_tbl <- gear_group |>
    purrr::map(~ tibble::tibble(mfdb_gear_code = .)) |>
    dplyr::bind_rows(.id = 'gear_name')
  return(
    tbl |>
      dplyr::left_join(pax_temptbl(as_pax(tbl), gear_tbl)) |>
      dplyr::mutate(gear_name = nvl(gear_name, 'Other'))
  )
}

# Was: tidypax::add_temporal_grouping
pax_add_temporal_grouping.pax <- function(
  tbl,
  tgroup = list(t1 = 1:6, t2 = 7:12)
) {
  if (is.null(tgroup)) {
    return(tbl |> dplyr::mutate(tgroup = 'year'))
  }

  tgroup_tbl <- tgroup |>
    purrr::map(~ tibble::tibble(month = .)) |>
    dplyr::bind_rows(.id = 'tgroup')
  return(
    tbl |>
      dplyr::left_join(pax_temptbl(as_pax(tbl), tgroup_tbl), by = "month") |>
      dplyr::mutate(tgroup = nvl(tgroup, 'Other'))
  )
}

# Was: tidypax::add_yearly_grouping
pax_add_yearly_grouping.pax <- function(tbl, ygroup = NULL) {
  if (is.null(ygroup)) {
    return(tbl |> dplyr::mutate(ygroup = year))
  }

  ygroup_tbl <- ygroup |>
    purrr::map(~ tibble::tibble(year = .)) |>
    dplyr::bind_rows(.id = 'ygroup')
  return(
    tbl |>
      dplyr::left_join(pax_temptbl(as_pax(tbl), ygroup_tbl), by = 'year') |>
      dplyr::mutate(ygroup = nvl(ygroup, year))
  )
}

# Was: tidypax::si_add_strata
pax_add_strata.pax <- function(
  tbl,
  use_total_area = 0,
  strata_stations = pax_dat_strata_stations(as_pax(tbl)),
  strata_attributes = pax_dat_strata_attributes(as_pax(tbl))
) {
  tbl |>
    dplyr::left_join(strata_stations) |>
    dplyr::left_join(strata_attributes) |>
    dplyr::mutate(
      area = ifelse(
        local(use_total_area) == 1,
        nvl(area, 1),
        nvl(rall_area, 0)
      ) /
        1.852^2
    ) |>
    dplyr::group_by(
      sample_id,
      station,
      gridcell,
      species,
      year,
      length,
      depth,
      stratification,
      stratum,
      sampling_type,
      area,
      rectangle,
      smareitur
    ) |>
    dplyr::summarise(N = sum(N, na.rm = TRUE), B = sum(B, na.rm = TRUE)) |>
    dplyr::group_by(
      species,
      year,
      stratification,
      stratum,
      sampling_type,
      area
    ) |>
    dplyr::mutate(
      unadjusted_N = N,
      unadjusted_B = B,
      N = area * N / dplyr::n_distinct(sample_id),
      B = area * B / dplyr::n_distinct(sample_id)
    )
}
