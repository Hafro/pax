pax_def_groupings <- function(
  lgroups = seq(0, 200, 5),
  regions = list(all = 101:115),
  tgroup = NULL,
  ygroup = NULL,
  gear_group = NULL,
  ocean_depth = NULL,
  ...
) {
  list(
    lgroups = lgroups,
    regions = regions,
    tgroup = tgroup,
    ygroup = ygroup,
    gear_group = gear_group,
    ocean_depth = ocean_depth,
    ...
  )
}

pax_add_groupings <- function(
  tbl,
  groupings = pax_def_groupings(),
  ignore_missing_col = TRUE
) {
  tbl <- tbl |>
    pax_add_regions(
      groupings$regions,
      ignore_missing_col = ignore_missing_col
    ) |>
    pax_add_gear_group(
      groupings$gear_group,
      ignore_missing_col = ignore_missing_col
    ) |>
    pax_add_lgroups(
      groupings$lgroups,
      ignore_missing_col = ignore_missing_col
    ) |>
    pax_add_temporal_grouping(
      groupings$tgroup,
      ignore_missing_col = ignore_missing_col
    ) |>
    pax_add_yearly_grouping(
      groupings$ygroup,
      ignore_missing_col = ignore_missing_col
    ) |>
    pax_add_ocean_depth_class(
      groupings$ocean_depth,
      ignore_missing_col = ignore_missing_col
    )
}

# Was: tidypax::add_lgroups
pax_add_lgroups <- function(tbl, lgroups, ignore_missing_col = FALSE) {
  pcon <- dbplyr::remote_con(tbl)

  if (isTRUE(ignore_missing_col) && !("length" %in% colnames(tbl))) {
    # Column not present in this table, do nothing
    return(tbl)
  }

  if (FALSE && all(diff(lgroups) == diff(lgroups)[[1]])) {
    # TODO: Disabled until #11 sorted
    # Even length-groups, we can use modulo arithmetic
    lgroup_min <- min(lgroups)
    lgroup_max <- max(lgroups)
    lgroup_dl <- diff(lgroups)[[1]]

    tbl <- tbl |>
      dplyr::mutate(
        lgroup = least(
          greatest(
            (length - local(lgroup_min)) -
              ((length - local(lgroup_min)) %% local(lgroup_dl)),
            local(lgroup_min)
          ) +
            local(lgroup_min),
          local(lgroup_max)
        )
      )
  } else {
    # Use list_filter to pick off first item to meet condition
    tbl <- tbl |>
      dplyr::mutate(
        # TODO: tidypax::add_lgroups' behaviour was including the lower bound in the previous group,
        #       use < instead of <= here to emulate this, but seems broken.
        #       https://github.com/Hafro/pax/issues/11
        lgroup = sql(paste0(
          "list_last(list_filter([",
          paste(as.numeric(lgroups), collapse = ", "),
          "], lambda x : x < length))"
        ))
      )
  }
  return(tbl)
}

# Was: tidypax::add_regions
pax_add_regions <- function(
  tbl,
  regions = list(all = 101:115),
  default = NULL,
  ignore_missing_col = FALSE
) {
  pcon <- dbplyr::remote_con(tbl)

  if (isTRUE(ignore_missing_col) && !("gridcell" %in% colnames(tbl))) {
    # Column not present in this table, do nothing
    return(tbl)
  }

  if (is.null(regions)) {
    # No regions given, do nothing
    return(tbl)
  }

  regions_tbl <- data.frame(
    # Repeat the region name from the regions list, one per entry
    region = rep(names(regions), sapply(regions, length)),
    division = unlist(regions)
  )

  out <- tbl |>
    dplyr::left_join(
      pax_temptbl(pcon, "paxdat_gridcell") |>
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
pax_add_ocean_depth_class <- function(
  tbl,
  ocean_depth_tbl = dplyr::tbl(dbplyr::remote_con(tbl), "ocean_depth"),
  breaks = c(0, 100, 200, 300),
  ignore_missing_col = FALSE
) {
  pcon <- dbplyr::remote_con(tbl)

  if (isTRUE(ignore_missing_col) && !("ocean_depth" %in% colnames(tbl))) {
    # Column not present in this table, do nothing
    return(tbl)
  }

  breaks <- sort(breaks)
  b_max <- max(breaks)
  b_labs <- paste(breaks[-length(breaks)], breaks[-1], sep = '-')
  b_labs_plusgroup <- sprintf("%d+", b_max)

  tbl |>
    dplyr::mutate(
      # If depth missing, use mean depth from ocean_depth_tbl
      ocean_depth = case_when(
        is.na(ocean_depth) ~
          dplyr::sql(paste0(
            "(
        SELECT mean(od.ocean_depth)
        FROM ",
            dbplyr::remote_name(ocean_depth_tbl),
            " od
        WHERE h3_cell IN h3_cells)"
          )),
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
pax_add_gear_group <- function(
  tbl,
  gear_group = NULL,
  default = "Other",
  ignore_missing_col = FALSE
) {
  pcon <- dbplyr::remote_con(tbl)

  if (isTRUE(ignore_missing_col) && !("mfdb_gear_code" %in% colnames(tbl))) {
    # Column not present in this table, do nothing
    return(tbl)
  }

  if (is.null(gear_group)) {
    return(tbl |> dplyr::mutate(gear_name = 'all'))
  }

  gear_tbl <- gear_group |>
    purrr::map(~ tibble::tibble(mfdb_gear_code = .)) |>
    dplyr::bind_rows(.id = 'gear_name')

  out <- dplyr::left_join(tbl, pax_temptbl(pcon, gear_tbl))

  # Set any groups containing NA manually, as these won't join
  na_groups <- names(gear_group)[sapply(gear_group, function(x) any(is.na(x)))]
  if (length(na_groups) > 1) {
    stop("Only one gear_group can contain NA")
  } else if (length(na_groups) > 0) {
    out <- dplyr::mutate(
      out,
      gear_name = ifelse(is.na(mfdb_gear_code), local(na_groups), gear_name)
    )
  }

  # Set default for any still-unassigned groups
  if (!is.null(default)) {
    out <- dplyr::mutate(
      out,
      gear_name = coalesce(gear_name, local(default))
    )
  }
}

# Was: tidypax::add_temporal_grouping
pax_add_temporal_grouping <- function(
  tbl,
  tgroup = list(t1 = 1:6, t2 = 7:12),
  ignore_missing_col = FALSE
) {
  pcon <- dbplyr::remote_con(tbl)

  if (isTRUE(ignore_missing_col) && !("month" %in% colnames(tbl))) {
    # Column not present in this table, do nothing
    return(tbl)
  }

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
pax_add_yearly_grouping <- function(
  tbl,
  ygroup = NULL,
  ignore_missing_col = FALSE
) {
  pcon <- dbplyr::remote_con(tbl)

  if (isTRUE(ignore_missing_col) && !("year" %in% colnames(tbl))) {
    # Column not present in this table, do nothing
    return(tbl)
  }

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
