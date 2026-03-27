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

  # Default table name is the name of the function
  name <- gsub("^pax_mar_", "", parent_fn)

  return(pax_decorate(tbl, name = name))
}

#' Import tables from the MAR database
#'
#' Functions to extract and standardise individual tables from the Hafro MAR
#' Oracle database into pax-compatible data.frames. The returned tables carry
#' ``pax_name`` and ``pax_cite`` attributes set by [pax_decorate()] and can be
#' passed directly to [pax_import()].
#'
#' @param mar A MAR database connection, as returned by ``mar::connect_mar()``
#' @param species Integer vector of species codes to filter by
#' @param year_start Optional integer, earliest year to include
#' @param year_end Optional integer, latest year to include
#' @name pax_mar
NULL

#' @return \subsection{pax_mar_logbook}{A dplyr query with columns
#'   ``logbook_id``, ``species``, ``year``, ``month``, ``vessel_nr``,
#'   ``mfdb_gear_code``, ``gear_size``, ``gridcell``, ``lat``, ``lon``,
#'   ``tow_area``, ``tow_time``, ``tow_hooks``, ``tow_num_nets``,
#'   ``tow_num_traps``, ``ocean_depth``, ``catch``, and ``catch_total``}
#' @rdname pax_mar
# Was tidypax::catch_data
pax_mar_logbook <- function(
  mar,
  species,
  year_start = NULL,
  year_end = NULL
) {
  if (!requireNamespace("mar", quietly = TRUE)) {
    stop("mar package not available, cannot import from DB")
  }

  # NSE variables
  id <- NULL
  year <- NULL
  month <- NULL
  vessel_nr <- NULL
  gear <- NULL
  gear_size <- NULL
  gridcell <- NULL
  lat <- NULL
  lon <- NULL
  area <- NULL
  towtime <- NULL
  hooks <- NULL
  nr_net <- NULL
  num_traps <- NULL
  depth <- NULL
  catch <- NULL
  total <- NULL

  # NB: Produced with https://gitlab.hafogvatn.is/dag/00-setup/-/blob/master/logbooks/catch.R
  # * sq / x / y / dx / dy / area produced by mar::encode_zchords(), rounding lon/lat
  out <- mar::tbl_mar(mar, 'ops$bthe."logbooks_compiled"') |>
    dplyr::filter(
      species %in% local(species),
    ) |>
    dplyr::select(
      logbook_id = id,
      species,
      year,
      month,
      vessel_nr,
      mfdb_gear_code = gear,
      gear_size = gear_size,
      gridcell,
      lat,
      lon,
      tow_area = area,
      tow_time = towtime,
      tow_hooks = hooks,
      tow_num_nets = nr_net,
      tow_num_traps = num_traps,
      ocean_depth = depth,
      catch = catch,
      catch_total = total
    )

  if (!is.null(year_start)) {
    out <- dplyr::filter(out, year >= local(year_start))
  }
  if (!is.null(year_end)) {
    out <- dplyr::filter(out, year <= local(year_end))
  }
  return(out |> decorate_mar())
}

#' @param ices_area_like Character vector of SQL LIKE patterns for filtering
#'   ICES areas, e.g. ``"5a%"``
#' @return \subsection{pax_mar_landings}{A dplyr query with columns
#'   ``year``, ``month``, ``species``, ``ices_area``, ``country``,
#'   ``mfdb_gear_code``, ``boat_id``, and ``catch``}
#' @rdname pax_mar
# Was: tidypax::landings_by_gear
pax_mar_landings <- function(
  mar,
  species,
  ices_area_like = '5a%',
  year_start = NULL,
  year_end = NULL
) {
  if (!requireNamespace("mar", quietly = TRUE)) {
    stop("mar package not available, cannot import from DB")
  }

  # NSE variables
  tegund_nr <- NULL
  ar <- NULL
  man <- NULL
  ices_svaedi <- NULL
  land <- NULL
  mfdb_gear_code <- NULL
  skip_nr <- NULL
  magn_oslaegt <- NULL
  year <- NULL

  # Make an expression or'ing all parts of ices_area_like together
  ices_area_c <- quote(1 == 0)
  for (l in ices_area_like) {
    ices_area_c <- substitute(
      ices_area_c | str_like(ices_svaedi, l),
      list(ices_area_c = ices_area_c, l = l)
    )
  }

  out <- mar::landadur_afli(mar) |> # i.e. combined reported landings, including historical landings & foreign
    dplyr::filter(
      ices_area_c,
      tegund_nr %in% local(species)
    ) |>
    dplyr::select(
      year = ar,
      month = man,
      species = tegund_nr,
      ices_area = ices_svaedi,
      country = land,
      mfdb_gear_code = mfdb_gear_code,
      boat_id = skip_nr,
      catch = magn_oslaegt
    )

  if (!is.null(year_start)) {
    out <- dplyr::filter(out, year >= local(year_start))
  }
  if (!is.null(year_end)) {
    out <- dplyr::filter(out, year <= local(year_end))
  }
  return(decorate_mar(out))
}

#' @return \subsection{pax_mar_ldist}{A dplyr query with columns
#'   ``sample_id``, ``species``, ``length``, ``sex``, and ``count``}
#' @rdname pax_mar
pax_mar_ldist <- function(
  mar,
  species
) {
  if (!requireNamespace("mar", quietly = TRUE)) {
    stop("mar package not available, cannot import from DB")
  }

  # NSE variables
  synis_id <- NULL
  tegund_nr <- NULL
  lengd <- NULL
  kyn_nr <- NULL
  fjoldi <- NULL
  sample_id <- NULL
  sex <- NULL
  count <- NULL

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
      dplyr::group_by(sample_id, species, length, sex) |>
      dplyr::summarize(count = sum(count, na.rm = TRUE)) |>
      decorate_mar()
  )
}

#' @return \subsection{pax_mar_aldist}{A dplyr query with columns
#'   ``sample_id``, ``species``, ``length``, ``weight``, ``age``, and
#'   ``count``}
#' @rdname pax_mar
pax_mar_aldist <- function(
  mar,
  species
) {
  if (!requireNamespace("mar", quietly = TRUE)) {
    stop("mar package not available, cannot import from DB")
  }

  # NSE variables
  synis_id <- NULL
  tegund_nr <- NULL
  lengd <- NULL
  thyngd <- NULL
  aldur <- NULL
  fjoldi <- NULL

  out <- mar::les_aldur(mar) |>
    dplyr::select(
      sample_id = synis_id,
      species = tegund_nr,
      length = lengd,
      weight = thyngd,
      age = aldur,
      count = fjoldi
    )
  if (!is.null(species)) {
    out <- dplyr::filter(out, species %in% local(species))
  }
  # NB: Would need to join to mar::les_syni -> mar::les_stod to filter by year, worth it?
  return(
    out |>
      # Re-group by columns we selected, ignoring maturity stage e.g.
      # NB: We used to do the below, but lw_pred uses aldist as an ~unaggregated source of length-weight data.
      #     https://github.com/Hafro/pax/issues/17
      #      dplyr::group_by(sample_id, species, length, age) |>
      #      dplyr::summarize(
      #        count = sum(count, na.rm = TRUE),
      #        weight = sum(weight * count, na.rm = TRUE) / sum(count, na.rm = TRUE)
      #      ) |>
      decorate_mar()
  )
}

#' @return \subsection{pax_mar_lw_coeffs}{A dplyr query of length-weight
#'   coefficients, filtered to the requested species}
#' @rdname pax_mar
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

#' @param measurement_type Character vector of measurement types to include,
#'   e.g. ``c("LEN", "OTOL")``
#' @return \subsection{pax_mar_measurement}{A dplyr query with columns
#'   ``individual_id``, ``sample_id``, ``species``, ``measurement_type``,
#'   ``length``, ``age``, ``sex``, ``maturity_stage``, ``weight_g``,
#'   ``gonad_weight``, ``gut_weight``, ``liver_weight``, and ``count``}
#' @rdname pax_mar
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

  # NSE variables
  maeling_teg <- NULL
  maeling_id <- NULL
  synis_id <- NULL
  tegund_nr <- NULL
  lengd <- NULL
  aldur <- NULL
  kyn_nr <- NULL
  kynthroski_nr <- NULL
  thyngd <- NULL
  kynfaeri <- NULL
  magi <- NULL
  lifur <- NULL
  fjoldi <- NULL
  ar <- NULL
  year <- NULL
  kvarna_nr <- NULL

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

#' @return \subsection{pax_mar_quotatransfer}{A data.frame of quota transfer
#'   records for the requested species, arranged by species and period}
#' @rdname pax_mar
# Was: tidypax::quota_transfer_table & tidypax::quota_transfer_plot (common section at start)
pax_mar_quotatransfer <- function(mar, species) {
  if (!requireNamespace("mar", quietly = TRUE)) {
    stop("mar package not available, cannot import from DB")
  }

  # NSE variables
  fteg <- NULL
  synis_id <- NULL
  kastad_breidd <- NULL
  timabil <- NULL
  fishing_year <- NULL

  mar::kvoti_stada_summarised(mar) |>
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
    dplyr::rename(
      species = fteg,
      fishing_year = timabil
    ) |>
    dplyr::arrange(species, fishing_year) |>
    dplyr::ungroup() |>
    decorate_mar()
}

#' @param mfdb_gear_code Character vector of gear codes to include
#' @param sampling_type Integer vector of sampling type codes to include
#' @return \subsection{pax_mar_sampling}{A dplyr query with columns
#'   ``sample_id``, ``lat``, ``lon``, ``year``, ``month``,
#'   ``sampling_type``, ``mfdb_gear_code``, and ``trip``, filtered to samples
#'   with length measurements for the requested species}
#' @rdname pax_mar
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

  # NSE variables
  synis_id <- NULL
  kastad_breidd <- NULL
  kastad_lengd <- NULL
  ar <- NULL
  man <- NULL
  synaflokkur_nr <- NULL
  gear <- NULL
  leidangur <- NULL
  trip <- NULL
  year <- NULL
  tegund_nr <- NULL

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

#' @return \subsection{pax_mar_station}{A dplyr query with columns
#'   ``sample_id``, ``year``, ``month``, ``station``, ``trip``,
#'   ``sampling_type``, ``gridcell``, ``begin_lat``, ``begin_lon``,
#'   ``end_lat``, ``end_lon``, ``mfdb_gear_code``, ``gear_id``,
#'   ``tow_depth``, ``tow_number``, ``tow_length``, ``tow_start``,
#'   ``tow_end``, and ``fixed``}
#' @rdname pax_mar
# Was: tidypax::si_stations
pax_mar_station <- function(
  mar,
  species = NULL, # NB: Ignored
  sampling_type = NULL,
  year_start = NULL,
  year_end = NULL
) {
  if (!requireNamespace("mar", quietly = TRUE)) {
    stop("mar package not available, cannot import from DB")
  }

  # NSE variables
  leidangur <- NULL
  reitur <- NULL
  smareitur <- NULL
  tog_nr <- NULL
  veidarfaeri <- NULL
  synis_id <- NULL
  ar <- NULL
  man <- NULL
  station <- NULL
  synaflokkur_nr <- NULL
  gridcell <- NULL
  kastad_breidd <- NULL
  kastad_lengd <- NULL
  hift_breidd <- NULL
  hift_lengd <- NULL
  gear <- NULL
  botndypi_kastad <- NULL
  toglengd <- NULL
  togbyrjun <- NULL
  togendir <- NULL
  fixed <- NULL
  year <- NULL

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
    dplyr::mutate(
      fixed = case_when(
        synaflokkur_nr == 30 &
          (reitur * 100 + nvl(tog_nr, 0)) %in%
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
        synaflokkur_nr == 30 & tog_nr %in% 1:19 ~ 1,
        synaflokkur_nr == 30 &
          tog_nr %in% c(31, 32) &
          reitur %in%
            c(319, 321, 367, 370, 371, 372, 414, 415, 422, 474, 523) ~
          1,
        TRUE ~ 0
      )
    ) |>
    dplyr::select(
      sample_id = synis_id,
      year = ar,
      month = man,
      station,
      trip = leidangur,
      sampling_type = synaflokkur_nr,
      gridcell,
      begin_lat = kastad_breidd,
      begin_lon = kastad_lengd,
      end_lat = hift_breidd,
      end_lon = hift_lengd,
      # NB: Extracted rename from si_by_age
      mfdb_gear_code = gear,
      gear_id = veidarfaeri,
      tow_depth = botndypi_kastad,
      tow_number = tog_nr,
      tow_length = toglengd,
      tow_start = togbyrjun,
      tow_end = togendir,
      fixed
    )

  if (!is.null(year_start)) {
    out <- dplyr::filter(out, year >= local(year_start))
  }
  if (!is.null(year_end)) {
    out <- dplyr::filter(out, year <= local(year_end))
  }
  if (!is.null(sampling_type)) {
    out <- dplyr::filter(out, sampling_type %in% local(sampling_type))
  }
  return(out |> decorate_mar())
}
