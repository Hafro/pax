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
  pax_import(pcon, pax_def_strata("old_strata"))
  pax_import(pcon, pax_def_strata("new_strata_spring"))

  writeLines("=== pax_mar_station")
  pax_import(pcon, do.call(pax_mar_station, import_defs))
  writeLines("=== pax_mar_measurement")
  pax_import(pcon, do.call(pax_mar_measurement, import_defs))
  writeLines("=== pax_mar_logbook")
  pax_import(pcon, do.call(pax_mar_logbook, import_defs))
  writeLines("=== pax_mar_landings")
  pax_import(pcon, do.call(pax_mar_landings, import_defs))
  writeLines("=== pax_mar_sampling")
  pax_import(pcon, do.call(pax_mar_sampling, import_defs))
  writeLines("=== pax_mar_aldist")
  pax_import(pcon, pax_mar_aldist(mar, species = import_defs$species))
  writeLines("=== pax_mar_ldist")
  pax_import(pcon, pax_mar_ldist(mar, species = import_defs$species))
  writeLines("=== pax_mar_lw_coeffs")
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
    dplyr::tbl(pcon, "station") |>
    dplyr::filter(sampling_type %in% 30, coalesce(tow_number, 0) %in% 0:35) |>
    pax_ldist_alk(
      tgroup = NULL,
      regions = list(
        S = c(101, 107, 106, 108, 109, 114),
        N = c(102, 103, 104, 105, 111, 113)
      ),
      gear_group = NULL
    )

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

  test_sample_ids <- dplyr::tbl(pcon, "station") |>
    dplyr::filter(year == 1990, month == 3) |>
    dplyr::pull(sample_id)
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
        dplyr::filter(sample_id %in% local(test_sample_ids)) |>
        dplyr::mutate(sample_id = as.numeric(sample_id)) |> # NB: sample_id is character, we need integer to sort it
        dplyr::filter(species == 2) |> # NB: Our ldist is broken down by species, we should do the same here
        dplyr::group_by(sample_id, species, length, sex) |>
        dplyr::summarize(count = sum(count)) |> # NB: les_lengd doesn't group
        dplyr::arrange(sample_id, species, length, sex) |>
        as.data.frame(),
      dplyr::tbl(pcon, "ldist") |>
        dplyr::filter(sample_id %in% local(test_sample_ids)) |>
        dplyr::mutate(sample_id = as.numeric(sample_id)) |> # NB: sample_id is character, we need integer to sort it
        dplyr::arrange(sample_id, species, length, sex) |>
        as.data.frame(),
      end = NULL
    ),
    "ldist matches for 1990/3"
  )
  ok(
    ut_cmp_equal(
      mar::les_aldur(mar) |>
        dplyr::select(
          sample_id = synis_id,
          species = tegund_nr,
          length = lengd,
          age = aldur,
          weight = thyngd,
          count = fjoldi
        ) |>
        dplyr::filter(sample_id %in% local(test_sample_ids)) |>
        dplyr::mutate(sample_id = as.numeric(sample_id)) |> # NB: sample_id is character, we need integer to sort it
        dplyr::filter(species == 2) |> # NB: Our ldist is broken down by species, we should do the same here
        dplyr::group_by(sample_id, species, length, age) |>
        dplyr::summarize(
          count = sum(count),
          weight = sum(weight * count) / sum(count)
        ) |> # NB: les_lengd doesn't group
        dplyr::arrange(sample_id, species, length, age) |>
        as.data.frame(),
      dplyr::tbl(pcon, "aldist") |>
        dplyr::filter(sample_id %in% local(test_sample_ids)) |>
        dplyr::mutate(sample_id = as.numeric(sample_id)) |> # NB: sample_id is character, we need integer to sort it
        dplyr::arrange(sample_id, species, length, age) |>
        as.data.frame(),
      end = NULL
    ),
    "aldist matches for 1990/3"
  )

  tidypax_lw_dat <-
    mar::les_stod(mar) |>
    dplyr::left_join(mar::les_syni(mar)) |>
    dplyr::left_join(mar::les_aldur(mar)) |>
    dplyr::filter(synaflokkur_nr == 30, tegund_nr == local(species_code)) |>
    dplyr::filter(ar == 1990) |> # NB: Filter to avoid differences in selection
    dplyr::select(species = tegund_nr, length = lengd, weight = thyngd) |>
    dplyr::filter(!is.na(length), weight > 0) |>
    dplyr::collect(n = Inf)
  newpax_lw_dat <-
    dplyr::tbl(pcon, "station") |>
    dplyr::filter(sampling_type %in% 30) |>
    dplyr::left_join(dplyr::tbl(pcon, "aldist"), by = c('sample_id')) |>
    dplyr::filter(year == 1990) |> # NB: Filter to avoid differences in selection
    dplyr::select(species, length, weight) |>
    dplyr::filter(!is.na(length), weight > 0) |>
    dplyr::collect(n = Inf)
  ok(
    ut_cmp_equal(
      tidypax_lw_dat |>
        dplyr::arrange(species, length, weight) |>
        as.data.frame(),
      newpax_lw_dat |>
        dplyr::arrange(species, length, weight) |>
        as.data.frame()
    ),
    "lw_dat: Can generate from both tidypax & newpax"
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
    tidypax::si_stations(mar) |>
    dplyr::filter(sampling_type %in% 30, nvl(tow_number, 0) %in% 0:35) |>
    tidypax::si_by_length(species = species_code, ldist = function(src) {
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
    dplyr::tbl(pcon, "station") |>
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
          si_abund = N,
          si_biomass = B
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
          si_abund,
          si_biomass
        ) |>
        as.data.frame(),
      end = NULL
    ),
    "tidypax_igfs_by_length: Match for 1990"
  )

  tidypax_igfs_at_age <-
    tidypax::si_stations(mar) |>
    dplyr::filter(sampling_type %in% 30, nvl(tow_number, 0) %in% 0:35) |>
    dplyr::filter(
      year >= local(import_defs$year_start),
      year <= local(import_defs$year_end)
    ) |>
    tidypax::si_by_length(species = species_code, ldist = function(src) {
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
    tidypax::si_by_age(
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
    dplyr::filter(stratification == 'old_strata')

  newpax_igfs_at_age <-
    dplyr::tbl(pcon, "station") |>
    dplyr::filter(sampling_type %in% 30, coalesce(tow_number, 0) %in% 0:35) |>
    pax_si_by_length(
      ldist = dplyr::tbl(pcon, "ldist") |>
        dplyr::left_join(
          pax_temptbl(pcon, lw_pred),
          by = c("species", 'length')
        )
    ) |>
    pax_si_scale_by_strata("old_strata") |>
    pax_si_scale_by_alk(
      tgroup = NULL,
      regions = list(
        S = c(101, 107, 106, 108, 109, 114),
        N = c(102, 103, 104, 105, 111, 113)
      ),
      gear_group = NULL,
      alk = newpax_igfs_alk
    )

  # Find stations in 1990 where we agree on the stratum
  agreeing_stations <- tidypax_igfs_at_age |>
    dplyr::filter(year == 1990) |>
    dplyr::group_by(station, stratum) |>
    dplyr::summarize() |>
    dplyr::collect() |>
    dplyr::inner_join(
      newpax_igfs_at_age |>
        dplyr::filter(year == 1990) |>
        dplyr::group_by(station, stratum) |>
        dplyr::summarize() |>
        dplyr::collect(),
      by = c("station", "stratum")
    ) |>
    dplyr::pull(station)
  agreeing_stations <- c(4120273, 4121373, 4130573)

  ok(
    ut_cmp_equal(
      tidypax_igfs_at_age |>
        dplyr::ungroup() |>
        #dplyr::filter(sample_id == 44490) |>
        dplyr::filter(year == 1990, station %in% local(agreeing_stations)) |>
        dplyr::select(
          ygroup,
          tgroup,
          gear_name,
          lgroup,
          station,
          age,
          length,
          agep,
          si_abund = adj_N,
          si_biomass = adj_B
        ) |>
        dplyr::collect() |>
        dplyr::mutate(
          si_abund = round(si_abund, 5),
          si_biomass = round(si_biomass, 5),
          agep = round(agep, 3)
        ) |>
        dplyr::arrange(
          ygroup,
          tgroup,
          gear_name,
          lgroup,
          station,
          age,
          length
        ) |>
        as.data.frame(),
      newpax_igfs_at_age |>
        dplyr::ungroup() |>
        #dplyr::filter(sample_id == 44490) |>
        dplyr::filter(year == 1990, station %in% local(agreeing_stations)) |>
        dplyr::select(
          ygroup,
          tgroup,
          gear_name,
          lgroup,
          station,
          age,
          length,
          agep,
          si_abund,
          si_biomass
        ) |>
        dplyr::collect() |>
        dplyr::mutate(
          si_abund = round(si_abund, 5),
          si_biomass = round(si_biomass, 5),
          agep = round(agep, 3)
        ) |>
        dplyr::arrange(
          ygroup,
          tgroup,
          gear_name,
          lgroup,
          station,
          age,
          length
        ) |>
        as.data.frame()
    ),
    "newpax_igfs_at_age: Matches tidypax at selected stations"
  )
})

ok_group("R/R/06-surveyplots.R:survey index by area", {
  # Do the si by_strata query, and extract the station/stratum mapping from it
  df_newpax_strata <- dplyr::tbl(pcon, "station") |>
    dplyr::filter(
      sampling_type == 30 & coalesce(tow_number, 0) %in% 0:35
    ) |>
    pax_si_by_length() |>
    pax_si_scale_by_strata(strata_tbl = "new_strata_spring") |>
    dplyr::group_by(station, stratum) |>
    #dplyr::summarise(
    #  h3_cell = to_hex(min(list_first(h3_cells))),
    #  begin_lat = min(begin_lat),
    #  begin_lon = min(begin_lon)
    #) |>
    #dplyr::filter(!is.na(h3_cell)) |>
    as.data.frame()

  # Get station mapping from tidypax
  df_tidypax_strata <- tidypax::si_strata_stations(mar) |>
    dplyr::filter(stratification == "new_strata", synaflokkur == 30) |>
    dplyr::select(station, stratum)

  # TODO: In an ideal world, these match, but there's a mismatch between what we assign & strata_stations:
  # https://github.com/Hafro/haftaf/issues/12
  #mapview::mapview(
  #  layer.name = "strata",
  #  pax::pax_def_strata('new_strata_spring') |>
  #    dplyr::mutate(stratum = as.character(stratum)),
  #  zcol = "stratum",
  #  legend = FALSE,
  #  col.regions = rainbow(60)
  #) + mapview::mapview(
  #  df_strata_comparision |> dplyr::filter(!match),
  #  xcol = "begin_lon", ycol = "begin_lat", zcol = "stratum.newpax", crs = 4326
  #) + mapview::mapview(
  #  h3jsr::cell_to_polygon(df_strata_comparision[!df_strata_comparision$match, "h3_cell", drop = FALSE]),
  #  layer.name = "h3_cell"
  #)
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
  ok(
    sum(df_strata_comparision$match) >= 24,
    "At least 24 stations have the same stratum assigned"
  )
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
  df_newpax <- dplyr::tbl(pcon, "logbook") |>
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
  # NB: tidypax's noaa_bathymetry is wrong: https://github.com/Hafro/pax/issues/9 so queries don't match
  # > df_newpax |> dplyr::filter(year == 1990, mfdb_gear_code == "DSE", region == "NE")
  # > df_tidypax |> dplyr::filter(year == 1990, mfdb_gear_code == "DSE", region == "NE")
  ok(
    ut_cmp_equal(
      df_tidypax |>
        dplyr::group_by(year, mfdb_gear_code, region) |>
        dplyr::summarise(c = sum(c)),
      df_newpax |>
        dplyr::group_by(year, mfdb_gear_code, region) |>
        dplyr::summarise(c = sum(c))
    ),
    "data frames match, ignoring ocean_depth_class"
  )
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
  df_newpax <- dplyr::tbl(pcon, "station") |>
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
