decorate_mar <- function(tbl) {
  # Figure out the pax_mar_* call in the stack
  mar_call <- NULL
  for (parent_call in rev(sys.calls())) {
    parent_fn <- parent_call[[1]]
    if (is.function(parent_fn)) {
      # Ignore function definitions (the next one up is probably do.call)
      next
    }
    if (identical(parent_fn, as.symbol("do.call"))) {
      # Strip off do.call wrapper
      parent_fn <- parent_call[[2]]
      # TODO: This doesn't actually work, the function arguments are lost, which is kinda the point
      parent_call <- call(deparse1(parent_call[[2]]), "TODO")
    }
    if (!is.symbol(parent_fn)) {
      if (
        identical(parent_fn[[1]], as.symbol("::")) &&
          identical(parent_fn[[2]], as.symbol("pax"))
      ) {
        # This is a pax::x call, strip outer
        parent_fn <- parent_fn[[3]]
      } else {
        # Not part of our namespace
        next
      }
    }
    if (
      is.symbol(parent_fn) && startsWith(as.character(parent_fn), "pax_mar_")
    ) {
      mar_call <- parent_call
      break
    }
  }
  if (is.null(mar_call)) {
    stop("No pax_mar_* call found in call stack")
  }

  # Citation is mar citation but with the pax call inserted
  cite <- citation("mar")[[1]]
  cite$title <- deparse1(mar_call)

  # Default table name is the name of the function
  name <- gsub("^pax_mar_", "", parent_fn)

  return(pax_decorate(tbl, cite = cite, name = name))
}

# Was tidypax::catch_data
pax_mar_catch <- function(
  mar,
  species,
  year_start = NULL,
  year_end = NULL
) {
  if (!requireNamespace("mar", quietly = TRUE)) {
    stop("mar package not available, cannot import from DB")
  }

  # Produced with https://gitlab.hafogvatn.is/dag/00-setup/-/blob/master/logbooks/catch.R
  out <- mar::tbl_mar(mar, 'ops$bthe."logbooks_compiled"') |>
    dplyr::filter(
      species == local(species),
    ) |>
    dplyr::rename(
      mfdb_gear_code = gear,
      ocean_depth = depth,
    )

  if (!is.null(year_start)) {
    out <- dplyr::filter(out, year >= local(year_start))
  }
  if (!is.null(year_end)) {
    out <- dplyr::filter(out, year <= local(year_end))
  }
  return(out |> decorate_mar())
}

# Was: tidypax::landings_by_gear
pax_mar_landings <- function(mar) {
  if (!requireNamespace("mar", quietly = TRUE)) {
    stop("mar package not available, cannot import from DB")
  }

  mar::landadur_afli(mar) |>
    dplyr::select(
      species = tegund_nr,
      mfdb_gear_code = mfdb_gear_code,
      ices_area = ices_svaedi,
      year = ar,
      month = man,
      ices_division = ices_svaedi,
      country = land,
      boat_id = skip_nr,
      landings = magn_oslaegt
    ) |>
    decorate_mar()
}

pax_mar_ldist <- function(
  mar,
  species
) {
  if (!requireNamespace("mar", quietly = TRUE)) {
    stop("mar package not available, cannot import from DB")
  }

  out <- mar::les_lengd(mar) |>
    mar::skala_med_taldir() |>
    dplyr::select(
      sample_id = synis_id,
      species = tegund_nr,
      length = lengd,
      sex = kyn_nr,
      count = fjoldi
    )

  if (!is.null(species)) {
    out <- dplyr::filter(out, species %in% local(species))
  }
  # NB: Would need to join to mar::les_syni -> mar::les_stod to filter by year, worth it?
  return(
    out |>
      # Re-group by columns we selected, ignoring maturity stage e.g.
      # TODO: Previously this didn't happen. Why?
      dplyr::group_by(sample_id, species, length, sex) |>
      dplyr::summarize(count = sum(count)) |>
      decorate_mar()
  )
}

pax_mar_aldist <- function(
  mar,
  species
) {
  if (!requireNamespace("mar", quietly = TRUE)) {
    stop("mar package not available, cannot import from DB")
  }

  out <- mar::les_aldur(mar) |>
    dplyr::select(
      sample_id = synis_id,
      species = tegund_nr,
      length = lengd,
      age = aldur,
      sex = kyn_nr,
      count = fjoldi
    )
  if (!is.null(species)) {
    out <- dplyr::filter(out, species %in% local(species))
  }
  # NB: Would need to join to mar::les_syni -> mar::les_stod to filter by year, worth it?
  return(
    out |>
      # Re-group by columns we selected, ignoring maturity stage e.g.
      dplyr::group_by(sample_id, species, length, age, sex) |>
      dplyr::summarize(count = sum(count)) |>
      decorate_mar()
  )
}

pax_mar_lw_coeffs <- function(
  mar,
  species
) {
  if (!requireNamespace("mar", quietly = TRUE)) {
    stop("mar package not available, cannot import from DB")
  }
  out <- mar::lw_coeffs(mar)

  if (!is.null(species)) {
    out <- dplyr::filter(out, species %in% local(species))
  }
  return(out |> decorate_mar())
}

# Was: tidypax::sampling_tables, tidypax::age_reading_status
pax_mar_measurement <- function(
  mar,
  species,
  year_start = NULL,
  year_end = NULL,
  measurement_type = NULL
) {
  if (!requireNamespace("mar", quietly = TRUE)) {
    stop("mar package not available, cannot import from DB")
  }

  kvarna_nr <- NULL # Mask NSE variable

  out <- mar::les_maelingu(mar) |>
    dplyr::mutate(
      maeling_teg = ifelse(
        maeling_teg == 'OTOL' & is.na(kvarna_nr),
        'LEN',
        maeling_teg
      )
    ) |>
    dplyr::select(
      individual_id = maeling_id,
      sample_id = synis_id,
      species = tegund_nr,
      measurement_type = maeling_teg,
      length = lengd,
      age = aldur,
      sex = kyn_nr,
      maturity_stage = kynthroski_nr,
      weight_g = thyngd,
      gonad_weight = kynfaeri,
      gut_weight = magi,
      liver_weight = lifur,
      count = fjoldi
    )
  if (!is.null(species)) {
    out <- dplyr::filter(out, species %in% local(species))
  }
  if (!is.null(measurement_type)) {
    out <- dplyr::filter(out, measurement_type %in% local(measurement_type))
  }

  if (!is.null(year_start) || !is.null(year_end)) {
    station_tbl <- mar::les_stod(mar) |>
      dplyr::left_join(mar::les_syni(mar), by = 'stod_id') |>
      dplyr::select(
        sample_id = synis_id,
        year = ar
      )
    if (!is.null(year_start)) {
      station_tbl <- dplyr::filter(station_tbl, year >= local(year_start))
    }
    if (!is.null(year_end)) {
      station_tbl <- dplyr::filter(station_tbl, year <= local(year_end))
    }
    out <- dplyr::semi_join(out, station_tbl, by = "sample_id") # Filter by extant rows
  }
  return(out |> decorate_mar())
}

# Was: tidypax::quota_transfer_table & tidypax::quota_transfer_plot (common section at start)
pax_mar_quotatransfer <- function(mar, species) {
  if (!requireNamespace("mar", quietly = TRUE)) {
    stop("mar package not available, cannot import from DB")
  }

  mar:::kvoti_stada_summarised(mar) |>
    dplyr::filter(fteg == local(species)) |>
    dplyr::collect(n = Inf) |>
    dplyr::mutate(
      timabil = ifelse(
        stringr::str_sub(timabil, 1, 1) %in% "9",
        paste0(
          1900 + as.integer(stringr::str_sub(timabil, 1, 2)),
          "/",
          stringr::str_sub(timabil, 3)
        ),
        paste0(
          2000 + as.integer(stringr::str_sub(timabil, 1, 2)),
          "/",
          stringr::str_sub(timabil, 3)
        )
      )
    ) |>
    dplyr::arrange(fteg, timabil) |>
    dplyr::ungroup() |>
    decorate_mar()
}

# Was: tidypax::sampling_position
pax_mar_sampling <- function(
  mar,
  species,
  year_start = NULL,
  year_end = NULL,
  mfdb_gear_code = c('BMT', 'LLN', 'DSE'),
  sampling_type = c(1, 2, 3, 4, 8)
) {
  if (!requireNamespace("mar", quietly = TRUE)) {
    stop("mar package not available, cannot import from DB")
  }

  mar::les_stod(mar) |>
    dplyr::left_join(mar::les_syni(mar), by = 'stod_id') |>
    dplyr::left_join(
      mar::tbl_mar(mar, 'biota.gear_mapping'),
      by = 'veidarfaeri'
    ) |>
    dplyr::select(
      sample_id = synis_id,
      lat = kastad_breidd,
      lon = kastad_lengd,
      year = ar,
      month = man,
      sampling_type = synaflokkur_nr,
      mfdb_gear_code = gear,
      trip = leidangur
    ) |>
    dplyr::filter(
      ## skip MAGEI and MOGUN, these are stomach samples and should be a seperate sampling type
      !(trip %like% 'MAG%'),
      !(trip %like% 'MO%'),
      sampling_type %in% local(sampling_type),
      mfdb_gear_code %in% local(mfdb_gear_code)
    ) -> out
  if (!is.null(year_start)) {
    out <- dplyr::filter(out, year >= local(year_start))
  }
  if (!is.null(year_end)) {
    out <- dplyr::filter(out, year <= local(year_end))
  }

  out |>
    dplyr::semi_join(
      mar::les_lengd(mar) |>
        dplyr::select(sample_id = synis_id, species = tegund_nr) |>
        dplyr::filter(species %in% local(species)),
      by = 'sample_id'
    ) |>
    decorate_mar()
}

# Was: tidypax::si_stations
pax_mar_si <- function(
  mar,
  species,
  year_start = NULL,
  year_end = NULL
) {
  if (!requireNamespace("mar", quietly = TRUE)) {
    stop("mar package not available, cannot import from DB")
  }

  out <- mar::les_stod(mar) |>
    ## skip MAGEI and MOGUN, these are stomach samples and should be a seperate sampling type
    dplyr::filter(
      !(leidangur %like% 'MAG%'),
      !(leidangur %like% 'MO%')
    ) |>
    dplyr::mutate(gridcell = 10 * reitur + smareitur) |> ## change to nautical miles^2
    dplyr::left_join(mar::les_syni(mar), by = 'stod_id') |>
    dplyr::mutate(
      station = reitur * 10000 + nvl(tog_nr, 0) * 100 + veidarfaeri
    ) |> ## change to nautical miles^2
    dplyr::left_join(
      mar::tbl_mar(mar, 'biota.gear_mapping'),
      by = 'veidarfaeri'
    ) |>
    dplyr::select(
      sample_id = synis_id,
      year = ar,
      month = man,
      station,
      trip = leidangur,
      gridcell,
      begin_lat = kastad_breidd,
      begin_lon = kastad_lengd,
      end_lat = hift_breidd,
      end_lon = hift_lengd,
      depth = botndypi_kastad,
      # NB: Extracted rename from si_by_age
      mfdb_gear_code = gear,
      rectangle = reitur,
      tow_number = tog_nr,
      gear_id = veidarfaeri,
      tow_length = toglengd,
      sampling_type = synaflokkur_nr,
      smareitur,
      tow_start = togbyrjun,
      tow_end = togendir,
      skiki,
      fjardarreitur
    ) |>
    dplyr::mutate(
      fixed = case_when(
        sampling_type == 30 &
          (rectangle * 100 + nvl(tow_number, 0)) %in%
            c(
              27401,
              37212,
              37302,
              41214,
              41412,
              46211,
              46212,
              46214,
              46216,
              46311,
              46312,
              46313,
              51301,
              52413,
              56214,
              57412,
              62311,
              71912,
              72314
            ) ~
          0,
        sampling_type == 30 & tow_number %in% 1:19 ~ 1,
        sampling_type == 30 &
          tow_number %in% c(31, 32) &
          rectangle %in%
            c(319, 321, 367, 370, 371, 372, 414, 415, 422, 474, 523) ~
          1,
        TRUE ~ 0
      )
    )

  if (!is.null(species)) {
    out <- dplyr::filter(out, species %in% local(species))
  }
  if (!is.null(year_start)) {
    out <- dplyr::filter(out, year >= local(year_start))
  }
  if (!is.null(year_end)) {
    out <- dplyr::filter(out, year <= local(year_end))
  }
  return(out |> decorate_mar())
}
