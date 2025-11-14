# Was: tidypax::add_lgroups
pax_add_lgroups <- function(tbl, lgroups, dl = 1) {
  pcon <- dbplyr::remote_con(tbl)

  lgroup_tbl <-
    tidyr::expand_grid(length_cm = seq(min(lgroups), max(lgroups), by = dl)) |>
    # TODO: consider making below:lgroups[cut(length_cm,lgroups-1,include.lowest = TRUE, labels =FALSE)]
    dplyr::mutate(
      lgroup = lgroups[cut(
        length_cm,
        lgroups,
        include.lowest = TRUE,
        labels = FALSE
      )]
    ) |>
    dplyr::group_by(lgroup) |>
    dplyr::mutate(mean_length_cm = mean(length_cm))
  print(lgroup_tbl)

  tbl |>
    dplyr::left_join(pax_temptbl(pcon, lgroup_tbl), by = 'length_cm')
}

# Was: tidypax::add_regions
pax_add_regions <- function(tbl, regions = list(all = 101:115), default = NULL) {
  pcon <- dbplyr::remote_con(tbl)

  regions_tbl <- data.frame(
    # Repeat the region name from the regions list, one per entry
    region = rep(names(regions), sapply(regions, length)),
    division = unlist(regions) )

  out <- tbl |>
    dplyr::left_join(
      dplyr::tbl(pcon, "paxdat_gridcell") |>
        dplyr::select(gridcell, division, subdivision) |>
        dplyr::left_join(pax_temptbl(pcon, regions_tbl), by = c("division")),
      by = c("gridcell"),
      suffix = c("", ".gridcell")
    )
  if (!is.null(default)) out <- dplyr::mutate(out, region = ifelse(is.na(region), local(default), region))
  return(out)
}

# Was: tidypax::add_depth_labels
pax_add_ocean_depth_class <- function(tbl, breaks = c(0, 100, 200, 300)) {
  pcon <- dbplyr::remote_con(tbl)

  # TODO: fallback logic, do right thing based in incoming table columns (only gridcell, select that, etc.)

  breaks <- sort(breaks)
  b_max <- max(breaks)
  b_labs <- paste(breaks[-length(breaks)], breaks[-1], sep = '-')
  b_labs_plusgroup <- sprintf("%d+", b_max)

  tbl |>
    dplyr::left_join(
      dplyr::tbl(pcon, "paxdat_gridcell"),
      by = 'gridcell',
      suffix = c("", ".gridcell")
    ) |>
    dplyr::mutate(
      # TODO: This assumes there's both incoming ocean_depth & ocean_depth.gridcell
      ocean_depth = ifelse(is.na(ocean_depth), ocean_depth.gridcell, ocean_depth),
      ocean_depth_class = case_when(
        is.na(ocean_depth) ~ 'Unknown',
        ocean_depth > local(b_max) ~ b_labs_plusgroup,
        TRUE ~ cut(ocean_depth, breaks, labels = b_labs, include.lowest = TRUE)
      ),
      ocean_depth_class = ifelse(is.na(ocean_depth_class), b_labs_plusgroup, ocean_depth_class)
    )
}

# Was: tidypax::add_gear_group
pax_add_gear_group <- function(tbl, gear_group = NULL) {
  pcon <- dbplyr::remote_con(tbl)

  if (is.null(gear_group)) {
    return(tbl |> dplyr::mutate(gear_name = 'all'))
  }

  gear_tbl <- gear_group |>
    purrr::map(~ tibble::tibble(mfdb_gear_code = .)) |>
    dplyr::bind_rows(.id = 'gear_name')

  tbl |>
    dplyr::left_join(pax_temptbl(pcon, gear_tbl)) |>
    dplyr::mutate(gear_name = nvl(gear_name, 'Other'))
}

# Was: tidypax::add_temporal_grouping
pax_add_temporal_grouping <- function(
  tbl,
  tgroup = list(t1 = 1:6, t2 = 7:12)
) {
  pcon <- dbplyr::remote_con(tbl)

  if (is.null(tgroup)) {
    return(tbl |> dplyr::mutate(tgroup = 'year'))
  }

  tgroup_tbl <- tgroup |>
    purrr::map(~ tibble::tibble(month = .)) |>
    dplyr::bind_rows(.id = 'tgroup')

  tbl |>
    dplyr::left_join(pax_temptbl(pcon, tgroup_tbl), by = "month") |>
    dplyr::mutate(tgroup = nvl(tgroup, 'Other'))
}

# Was: tidypax::add_yearly_grouping
pax_add_yearly_grouping <- function(tbl, ygroup = NULL) {
  pcon <- dbplyr::remote_con(tbl)

  if (is.null(ygroup)) {
    return(tbl |> dplyr::mutate(ygroup = year))
  }

  ygroup_tbl <- ygroup |>
    purrr::map(~ tibble::tibble(year = .)) |>
    dplyr::bind_rows(.id = 'ygroup')

  tbl |>
    dplyr::left_join(pax_temptbl(pcon, ygroup_tbl), by = 'year') |>
    dplyr::mutate(ygroup = nvl(ygroup, year))
}

# Was: tidypax::si_add_strata
pax_add_strata <- function(
  tbl,
  use_total_area = 0,
  strata_stations = pax_dat_strata_stations(dbplyr::remote_con(tbl)),
  strata_attributes = pax_dat_strata_attributes(dbplyr::remote_con(tbl))
) {
  con <- dbplyr::remote_con(tbl)

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

pax_add_sampling_type_desc <- function(
  tbl,
  lang = getOption('pax.lang')
) {
  pcon <- dbplyr::remote_con(tbl)
  
  st_tbl <- dplyr::tbl(pcon, "paxdat_sampling_type_desc")
  if (lang == 'is') {
    st_tbl <- st_tbl |>
      dplyr::select(sampling_type, sampling_type_desc = sampling_type_desc_is)
  } else {
    st_tbl <- st_tbl |>
      dplyr::select(sampling_type, sampling_type_desc = sampling_type_desc_en)
  }

  tbl |>
    dplyr::left_join(st_tbl, by = c('sampling_type'))
}

pax_add_mfdb_gear_code_desc <- function(
  tbl,
  lang = getOption('pax.lang')
) {
  pcon <- dbplyr::remote_con(tbl)
  
  st_tbl <- dplyr::tbl(pcon, "paxdat_mfdb_gear_code_desc")

  tbl |>
    dplyr::left_join(st_tbl, by = c('mfdb_gear_code'))
}
