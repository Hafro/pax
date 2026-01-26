# Was: tidypax::add_lgroups
pax_add_lgroups <- function(tbl, lgroups, dl = 1) {
  pcon <- dbplyr::remote_con(tbl)

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
    dplyr::left_join(pax_temptbl(pcon, lgroup_tbl), by = 'length')
}

# Was: tidypax::add_regions
pax_add_regions <- function(
  tbl,
  regions = list(all = 101:115),
  default = NULL
) {
  pcon <- dbplyr::remote_con(tbl)

  regions_tbl <- data.frame(
    # Repeat the region name from the regions list, one per entry
    region = rep(names(regions), sapply(regions, length)),
    division = unlist(regions)
  )

  out <- tbl |>
    dplyr::left_join(
      dplyr::tbl(pcon, "paxdat_gridcell") |>
        dplyr::select(gridcell, division, subdivision) |>
        dplyr::left_join(pax_temptbl(pcon, regions_tbl), by = c("division")),
      by = c("gridcell"),
      suffix = c("", ".gridcell")
    )
  if (!is.null(default)) {
    out <- dplyr::mutate(
      out,
      region = ifelse(is.na(region), local(default), region)
    )
  }
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
    dplyr::mutate(
      # If depth missing, use mean depth from paxdat_ocean_depth
      ocean_depth = case_when(
        is.na(ocean_depth) ~
          dplyr::sql(
            "(
        SELECT mean(od.ocean_depth)
        FROM paxdat_ocean_depth od
        WHERE h3_cell IN h3_cells)"
          ),
        TRUE ~ ocean_depth
      ),
      ocean_depth_class = case_when(
        is.na(ocean_depth) ~ 'Unknown',
        ocean_depth > local(b_max) ~ b_labs_plusgroup,
        TRUE ~ cut(ocean_depth, breaks, labels = b_labs, include.lowest = TRUE)
      ),
      ocean_depth_class = ifelse(
        is.na(ocean_depth_class),
        b_labs_plusgroup,
        ocean_depth_class
      )
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
    dplyr::mutate(gear_name = coalesce(gear_name, 'Other'))
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
    dplyr::mutate(tgroup = coalesce(tgroup, 'Other'))
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
    dplyr::mutate(ygroup = coalesce(ygroup, year))
}

# TODO: This interface is a bit of an anacronysm now
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
