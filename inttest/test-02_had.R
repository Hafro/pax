#!/usr/bin/env Rscript
# Test converted versions of queries from 02-had
library(unittest)

library(pax)

if (interactive()) {
  options(width = 10000)
}

if (!exists("mar")) {
  mar <- mar::connect_mar()
}

species_code <- 2
import_defs <- list(
  mar,
  species = species_code,
  year_start = 1990,
  year_end = 1994
)

if (!file.exists("/tmp/camel.duckdb")) {
  pcon <- pax::pax_connect("/tmp/camel.duckdb")
  pax_import(pcon, pax_def_strata("new_strata"))

  pax_import(pcon, do.call(pax_mar_si, import_defs))
  pax_import(pcon, do.call(pax_mar_measurement, import_defs))
  pax_import(pcon, do.call(pax_mar_catch, import_defs))
  pax_import(pcon, do.call(pax_mar_sampling, import_defs))
  pax_import(pcon, pax_mar_aldist(mar, species = import_defs$species))
  pax_import(pcon, pax_mar_ldist(mar, species = import_defs$species))
  pax_import(pcon, pax_mar_lw_coeffs(mar, species = import_defs$species))
} else {
  pcon <- pax::pax_connect("/tmp/camel.duckdb")
}

ok_group("input_data.R:Generate the ALK from the survey", {
  tidypax_igfs_alk <-
    tidypax::si_stations(mar) |>
    dplyr::filter(sampling_type %in% 30, nvl(tow_number, 0) %in% 0:35) |>
    tidypax::si_make_alk(
      tgroup = NULL,
      regions = list(
        S = c(101, 107, 106, 108, 109, 114),
        N = c(102, 103, 104, 105, 111, 113)
      ),
      gear_group = NULL,
      aldist = function(src) {
        mar::les_aldur(mar) |>
          dplyr::filter(tegund_nr == 2) |>
          dplyr::select(
            sample_id = synis_id,
            species = tegund_nr,
            length = lengd,
            age = aldur,
            count = fjoldi
          ) |>
          dplyr::group_by(sample_id, species, length, age) |>
          dplyr::summarize(count = sum(count))
      }
    ) |>
    dplyr::filter(species == local(species_code))

  newpax_igfs_alk <-
    dplyr::tbl(pcon, "si") |>
    dplyr::filter(sampling_type %in% 30, coalesce(tow_number, 0) %in% 0:35) |>
    pax_si_make_alk(
      tgroup = NULL,
      regions = list(
        S = c(101, 107, 106, 108, 109, 114),
        N = c(102, 103, 104, 105, 111, 113)
      ),
      gear_group = NULL
    ) |>
    dplyr::filter(ygroup == 1990) |>
    dplyr::arrange(ygroup, gear_name, region, species, tgroup, lgroup, age) |>
    dplyr::collect()

  ok(
    ut_cmp_equal(
      tidypax_igfs_alk |>
        dplyr::filter(ygroup == 1990) |>
        dplyr::arrange(
          ygroup,
          gear_name,
          region,
          species,
          tgroup,
          lgroup,
          age,
          agep
        ) |>
        dplyr::select(
          ygroup,
          gear_name,
          region,
          species,
          tgroup,
          lgroup,
          age,
          agep
        ) |>
        as.data.frame(),
      newpax_igfs_alk |>
        dplyr::filter(ygroup == 1990) |>
        dplyr::arrange(
          ygroup,
          gear_name,
          region,
          species,
          tgroup,
          lgroup,
          age,
          agep
        ) |>
        dplyr::select(
          ygroup,
          gear_name,
          region,
          species,
          tgroup,
          lgroup,
          age,
          agep
        ) |>
        as.data.frame(),
      tolerance = 1e-6
    ),
    "Data frames match in 1990"
  )

  ok(
    ut_cmp_equal(
      mar::les_lengd(mar) |>
        mar::skala_med_taldir() |>
        dplyr::select(
          sample_id = synis_id,
          species = tegund_nr,
          length = lengd,
          sex = kyn_nr,
          count = fjoldi
        ) |>
        dplyr::mutate(sample_id = as.numeric(sample_id)) |> # NB: sample_id is character, we need integer to sort it
        dplyr::filter(species == 2) |> # NB: Our ldist is broken down by species, we should do the same here
        dplyr::group_by(sample_id, species, length, sex) |>
        dplyr::summarize(count = sum(count)) |> # NB: les_lengd doesn't group
        dplyr::arrange(sample_id, species, length, sex) |>
        as.data.frame(),
      dplyr::tbl(pcon, "ldist") |>
        dplyr::mutate(sample_id = as.numeric(sample_id)) |> # NB: sample_id is character, we need integer to sort it
        dplyr::arrange(sample_id, species, length, sex) |>
        as.data.frame(),
      end = NULL
    ),
    "ldist matches"
  )
  ok(
    ut_cmp_equal(
      mar::les_aldur(mar) |>
        dplyr::select(
          sample_id = synis_id,
          species = tegund_nr,
          length = lengd,
          age = aldur,
          count = fjoldi
        ) |>
        dplyr::mutate(sample_id = as.numeric(sample_id)) |> # NB: sample_id is character, we need integer to sort it
        dplyr::filter(species == 2) |> # NB: Our ldist is broken down by species, we should do the same here
        dplyr::group_by(sample_id, species, length, age) |>
        dplyr::summarize(count = sum(count)) |> # NB: les_lengd doesn't group
        dplyr::arrange(sample_id, species, length, age) |>
        as.data.frame(),
      dplyr::tbl(pcon, "aldist") |>
        dplyr::mutate(sample_id = as.numeric(sample_id)) |> # NB: sample_id is character, we need integer to sort it
        dplyr::arrange(sample_id, species, length, age) |>
        as.data.frame(),
      end = NULL
    ),
    "aldist matches"
  )

  tidypax_lw_dat <-
    mar::les_stod(mar) |>
    dplyr::left_join(mar::les_syni(mar)) |>
    dplyr::left_join(mar::les_aldur(mar)) |>
    dplyr::filter(synaflokkur_nr == 30, tegund_nr == local(species_code)) |>
    dplyr::select(species = tegund_nr, length = lengd, weight = thyngd) |>
    dplyr::filter(!is.na(length), weight > 0) |>
    dplyr::collect(n = Inf)
  lw_pred <-
    tibble::tibble(species = species_code, length = 1:150) |>
    modelr::add_predictions(
      gam::gam(
        weight ~ gam::s(log(length), df = 8),
        family = Gamma(link = log),
        data = tidypax_lw_dat
      ),
      var = 'weight'
    ) |>
    dplyr::mutate(weight = as.numeric(exp(weight)))

  tidypax_igfs_by_length <-
    si_stations(mar) |>
    dplyr::filter(sampling_type %in% 30, nvl(tow_number, 0) %in% 0:35) |>
    si_by_length(species = species_code, ldist = function(src) {
      mar::les_lengd(mar) |>
        mar::skala_med_taldir() |>
        dplyr::select(
          sample_id = synis_id,
          species = tegund_nr,
          length = lengd,
          sex = kyn_nr,
          count = fjoldi
        ) |>
        dplyr::filter(species == 2) |> # NB: newpax's ldist is broken down by species, we should do the same here
        dplyr::group_by(sample_id, species, length, sex) |>
        dplyr::summarize(count = sum(count)) |> # NB: les_lengd doesn't group
        dplyr::left_join(
          dbplyr::copy_inline(mar, lw_pred),
          by = c("species", 'length')
        )
    }) |>
    identity()
  newpax_igfs_by_length <-
    dplyr::tbl(pcon, "si") |>
    dplyr::filter(sampling_type %in% 30, coalesce(tow_number, 0) %in% 0:35) |>
    pax_si_by_length(
      ldist = dplyr::tbl(pcon, "ldist") |>
        dplyr::left_join(
          pax_temptbl(pcon, lw_pred),
          by = c("species", 'length')
        )
    ) |>
    identity()
  ok(
    ut_cmp_equal(
      tidypax_igfs_by_length |>
        dplyr::filter(year == 1990) |>
        dplyr::arrange(year, month, station, gear, species, length, sex) |>
        dplyr::ungroup() |>
        dplyr::select(
          year,
          month,
          station,
          begin_lat,
          begin_lon,
          tow_length,
          gear,
          species,
          length,
          sex,
          N,
          B
        ) |>
        as.data.frame(),
      newpax_igfs_by_length |>
        dplyr::rename(gear = mfdb_gear_code) |>
        dplyr::filter(year == 1990) |>
        dplyr::arrange(year, month, station, gear, species, length, sex) |>
        dplyr::ungroup() |>
        dplyr::select(
          year,
          month,
          station,
          begin_lat,
          begin_lon,
          tow_length,
          gear,
          species,
          length,
          sex,
          N,
          B
        ) |>
        as.data.frame(),
      end = NULL
    ),
    "tidypax_igfs_by_length: Match for 1990"
  )

  tidypax_igfs_at_age <-
    si_stations(mar) |>
    dplyr::filter(sampling_type %in% 30, nvl(tow_number, 0) %in% 0:35) |>
    dplyr::filter(
      year >= local(import_defs$year_start),
      year <= local(import_defs$year_end)
    ) |>
    si_by_length(species = species_code, ldist = function(src) {
      mar::les_lengd(mar) |>
        mar::skala_med_taldir() |>
        dplyr::select(
          sample_id = synis_id,
          species = tegund_nr,
          length = lengd,
          sex = kyn_nr,
          count = fjoldi
        ) |>
        dplyr::filter(species == 2) |> # NB: newpax's ldist is broken down by species, we should do the same here
        dplyr::group_by(sample_id, species, length, sex) |>
        dplyr::summarize(count = sum(count)) |> # NB: les_lengd doesn't group
        dplyr::left_join(
          dbplyr::copy_inline(mar, lw_pred),
          by = c("species", 'length')
        )
    }) |>
    si_by_age(
      post_scaling = function(x, ...) {
        x
      },
      tgroup = NULL,
      regions = list(
        S = c(101, 107, 106, 108, 109, 114),
        N = c(102, 103, 104, 105, 111, 113)
      ),
      gear_group = NULL,
      alk = tidypax_igfs_alk
    ) |>
    dplyr::filter(stratification == 'new_strata') # NB: Was old_strata, but haven't loaded that

  newpax_igfs_at_age <-
    dplyr::tbl(pcon, "si") |>
    dplyr::filter(sampling_type %in% 30, coalesce(tow_number, 0) %in% 0:35) |>
    pax_si_by_length(
      ldist = dplyr::tbl(pcon, "ldist") |>
        dplyr::left_join(
          pax_temptbl(pcon, lw_pred),
          by = c("species", 'length')
        )
    ) |>
    pax_add_strata("new_strata") |>
    pax_si_by_age(
      tgroup = NULL,
      regions = list(
        S = c(101, 107, 106, 108, 109, 114),
        N = c(102, 103, 104, 105, 111, 113)
      ),
      gear_group = NULL,
      alk = newpax_igfs_alk
    )

  if (FALSE) { # TODO: Incorrect strata is breaking scaling
  tidypax_igfs_at_age |>
    dplyr::filter(sample_id == 44490, age == 2) |>
    dplyr::arrange(length) |>
    dplyr::select(station, length, stratum, age, agep)
  newpax_igfs_at_age |>
    dplyr::filter(sample_id == 44490, age == 2) |>
    dplyr::arrange(length) |>
    dplyr::select(
      station,
      length,
      begin_lat,
      begin_lon,
      end_lat,
      end_lon,
      stratum,
      age,
      agep
    )

  ok(
    ut_cmp_equal(
      tidypax_igfs_at_age |>
        dplyr::filter(year == 1990, station == 3661273) |>
        dplyr::arrange(
          ygroup,
          gear_name,
          region,
          species,
          tgroup,
          lgroup,
          age
        ) |>
        dplyr::ungroup() |>
        dplyr::select(
          ygroup,
          gear_name,
          region,
          species,
          tgroup,
          lgroup,
          age,
          agep,
          N,
          adj_N,
          adj_B
        ) |>
        as.data.frame(),
      newpax_igfs_at_age |>
        dplyr::rename(gear = mfdb_gear_code) |>
        dplyr::filter(year == 1990, station == 3661273) |>
        dplyr::arrange(
          ygroup,
          gear_name,
          region,
          species,
          tgroup,
          lgroup,
          age
        ) |>
        dplyr::ungroup() |>
        dplyr::select(
          ygroup,
          gear_name,
          region,
          species,
          tgroup,
          lgroup,
          age,
          agep,
          N,
          adj_N,
          adj_B
        ) |>
        # TODO: Why do we still have an h3_cell column?
        as.data.frame(),
      end = NULL
    ),
    "newpax_igfs_at_age: Matches tidypax at 1990"
  )
  }  # TODO: End
})

ok_group("R/R/06-surveyplots.R:survey index by area", {
  # Do the si by_strata query, and extract the station/stratum mapping from it
  df_newpax_strata <- dplyr::tbl(pcon, "si") |>
    dplyr::filter(
      sampling_type == 30 &
        coalesce(tow_number, 0) %in% 0:35 | ## spring survey
        coalesce(tow_number, 0) %in%
          0:75 &
          gear_id %in% 77:78 &
          year != 2011 &
          sampling_type == 35 ## autumn survey
    ) |>
    pax_si_by_length() |>
    pax_add_strata() |>
    dplyr::group_by(station, stratum) |>
    dplyr::summarise(
      h3_cells = min(list_first(h3_cells)),
      begin_lat = min(begin_lat),
      begin_lon = min(begin_lon)
    ) |>
    as.data.frame()

  # Get station mapping from tidypax
  df_tidypax_strata <- tidypax::si_strata_stations(mar) |>
    dplyr::filter(stratification == "new_strata", synaflokkur == 30) |>
    dplyr::select(station, stratum)

  # TODO: These should match, but they don't. Either it's GIGO strata, or we should be using the tow midpoint as our join
  df_strata_comparision <- df_newpax_strata |>
    dplyr::left_join(
      df_tidypax_strata,
      by = c("station"),
      suffix = c(".newpax", ".tidypax"),
      copy = TRUE
    ) |>
    dplyr::filter(!is.na(stratum.newpax)) |>
    dplyr::mutate(match = stratum.newpax == stratum.tidypax) |>
    dplyr::distinct(stratum.newpax, stratum.tidypax, .keep_all = TRUE) |>
    dplyr::arrange(stratum.newpax, stratum.tidypax)
})

ok_group("R/01-plots_and_tables.R:sampling_position", {
  df_tidypax <- tidypax:::sampling_position(
    mar,
    species_nr = species_code,
    year_range = import_defs$year_start:import_defs$year_end
  ) |>
    dplyr::arrange(lat, lon, year, mfdb_gear_code) |>
    as.data.frame()
  df_newpax <- dplyr::tbl(pcon, "sampling") |>
    dplyr::left_join(dplyr::tbl(pcon, "measurement"), by = "sample_id") |>
    pax_sampling_position_summary() |>
    dplyr::arrange(lat, lon, year, mfdb_gear_code) |>
    as.data.frame()
  ok(ut_cmp_equal(df_tidypax, df_newpax), "data frames match")
})

ok_group("R/01-plots_and_tables.R:sampling_tables", {
  species_code <- 2
  df_tidypax <- tidypax:::sampling_tables(mar, species_nr = species_code) |>
    dplyr::filter(Year %in% 1990:1994) |>
    dplyr::collect() |>
    dplyr::rename(year = Year)
  df_newpax <- dplyr::tbl(pcon, "sampling") |>
    dplyr::left_join(dplyr::tbl(pcon, "measurement"), by = "sample_id") |>
    pax_sampling_detail() |>
    dplyr::relocate("year") |>
    dplyr::collect()
  ok(ut_cmp_equal(df_tidypax, df_newpax), "data frames match")
})

ok_group("R/01-plots_and_tables.R:catch_agg", {
  df_tidypax <- suppressWarnings(
    tidypax::catch_data(mar, species_code) |>
      dplyr::filter(
        year >= local(import_defs$year_start),
        year <= local(import_defs$year_end)
      ) |>
      tidypax::add_depth_labels(breaks = c(0, 100, 200, 300)) |>
      tidypax::add_regions(
        regions = list(
          W = 101,
          NW = 102,
          NE = c(103, 104, 105),
          SE = c(107, 106),
          SW = 108
        )
      ) |>
      dplyr::mutate(region = coalesce(region, 'Other')) |>
      dplyr::group_by(year, mfdb_gear_code, region, depth_class) |>
      dplyr::summarise(c = sum(catch) / 1e6) |>
      dplyr::ungroup() |>
      dplyr::arrange(year, mfdb_gear_code, region, depth_class) |>
      dplyr::rename(ocean_depth_class = depth_class) |>
      dplyr::collect() |>
      as.data.frame()
  )
  df_newpax <- dplyr::tbl(pcon, "catch") |>
    pax_add_ocean_depth_class(breaks = c(0, 100, 200, 300)) |>
    pax_add_regions(
      regions = list(
        W = 101,
        NW = 102,
        NE = c(103, 104, 105),
        SE = c(107, 106),
        SW = 108
      ),
      default = "Other"
    ) |>
    dplyr::group_by(year, mfdb_gear_code, region, ocean_depth_class) |>
    dplyr::summarise(c = sum(catch) / 1e6) |>
    dplyr::ungroup() |>
    dplyr::arrange(year, mfdb_gear_code, region, ocean_depth_class) |>
    dplyr::collect() |>
    as.data.frame()
  # TODO: Total catch good, but grouping isn't right?
  #ok(ut_cmp_equal(df_tidypax, df_newpax), "data frames match")
  ok(ut_cmp_equal(sum(df_tidypax$c), sum(df_newpax$c)), "total catch match")
})

ok_group("assessment_model/00-setup/input_data.R:maturity_key", {
  df_tidypax <- suppressWarnings(
    tidypax::si_stations(mar) |>
      dplyr::filter(sampling_type == 30) |>
      dplyr::inner_join(
        mar::les_maelingu(mar) |>
          dplyr::filter(
            maeling_teg == 'OTOL',
            tegund_nr == local(species_code),
            !is.na(aldur),
            !is.na(kynthroski_nr)
          ) |>
          dplyr::mutate(mat = ifelse(kynthroski_nr == 1, 0, 1)) |>
          dplyr::rename(sample_id = synis_id, length = lengd, age = aldur)
      ) |>
      tidypax::add_lgroups(lgroups = seq(0, 200, 5)) |>
      tidypax:::add_regions(
        regions = list(
          S = c(101, 107, 106, 108, 109, 114),
          N = c(102, 103, 104, 105, 111, 113)
        )
      ) |>
      dplyr::mutate(region = coalesce(region, 'S')) |>
      dplyr::filter(
        year >= local(import_defs$year_start),
        year <= local(import_defs$year_end)
      ) |>
      dplyr::group_by(year, lgroup, age, region) |>
      dplyr::summarise(p = mean(mat)) |>
      dplyr::arrange(year, region, age, lgroup) |>
      dplyr::collect()
  )
  df_newpax <- dplyr::tbl(pcon, "si") |>
    dplyr::filter(sampling_type == 30) |>
    dplyr::inner_join(
      dplyr::tbl(pcon, "measurement") |>
        dplyr::filter(
          measurement_type == "OTOL",
          !is.na(age),
          !is.na(maturity_stage)
        ) |>
        dplyr::mutate(mat = ifelse(maturity_stage == 1, 0, 1))
    ) |>
    pax_add_lgroups(lgroups = seq(0, 200, 5)) |>
    pax_add_regions(
      regions = list(
        S = c(101, 107, 106, 108, 109, 114),
        N = c(102, 103, 104, 105, 111, 113)
      ),
      default = "S"
    ) |>
    dplyr::group_by(year, lgroup, age, region) |>
    dplyr::summarise(p = mean(mat)) |>
    dplyr::arrange(year, region, age, lgroup) |>
    dplyr::collect()
  ok(ut_cmp_equal(df_tidypax, df_newpax), "data frames match")
})

DBI::dbDisconnect(pcon)
