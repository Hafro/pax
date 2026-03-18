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

import_defs <- list(
  species = 2,
  year_start = 1990,
  year_end = 1994
)

test_db_path <- "/tmp/test-pax_si-alk.duckdb"
if (!file.exists(test_db_path)) {
  pcon <- pax_from_mar(
    species = import_defs$species,
    year_start = import_defs$year_start,
    year_end = import_defs$year_end,
    dbdir = test_db_path
  )
} else {
  pcon <- pax::pax_connect(test_db_path)
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
          dplyr::summarize(count = sum(count, na.rm = TRUE))
      }
    ) |>
    dplyr::filter(species == local(import_defs$species))

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
        dplyr::summarize(count = sum(count, na.rm = TRUE)) |> # NB: les_lengd doesn't group
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
          count = sum(count, na.rm = TRUE),
          weight = sum(weight * count, na.rm = TRUE) / sum(count, na.rm = TRUE)
        ) |> # NB: les_lengd doesn't group
        dplyr::arrange(sample_id, species, length, age) |>
        as.data.frame(),
      dplyr::tbl(pcon, "aldist") |>
        dplyr::filter(sample_id %in% local(test_sample_ids)) |>
        dplyr::mutate(sample_id = as.numeric(sample_id)) |> # NB: sample_id is character, we need integer to sort it
        # NB: aldist isn't aggregated by it's columns: https://github.com/Hafro/pax/issues/17
        dplyr::group_by(sample_id, species, length, age) |>
        dplyr::summarize(
          count = sum(count, na.rm = TRUE),
          weight = sum(weight * count, na.rm = TRUE) / sum(count, na.rm = TRUE)
        ) |>
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
    dplyr::filter(
      synaflokkur_nr == 30,
      tegund_nr == local(import_defs$species)
    ) |>
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
    dplyr::filter(
      synaflokkur_nr == 30,
      tegund_nr == local(import_defs$species)
    ) |>
    dplyr::select(species = tegund_nr, length = lengd, weight = thyngd) |>
    dplyr::filter(!is.na(length), weight > 0) |>
    dplyr::collect(n = Inf)
  # NB: Can't use gam::s inside formula, presumably something is looking for as.symbol("s")
  s <- gam::s
  lw_pred <-
    tibble::tibble(species = import_defs$species, length = 1:150) |>
    modelr::add_predictions(
      gam::gam(
        weight ~ s(log(length), df = 8),
        family = Gamma(link = log),
        data = tidypax_lw_dat
      ),
      var = 'weight'
    ) |>
    dplyr::mutate(weight = as.numeric(exp(weight)))
  ok(
    ut_cmp_equal(
      signif(
        lw_pred |>
          dplyr::filter(length %% 10 == 0) |>
          dplyr::arrange(length) |>
          dplyr::pull(weight),
        5
      ),
      c(
        8.5151,
        63.142,
        231.1,
        582.99,
        1172.8,
        2051.3,
        3272.3,
        4884.4,
        6948.6,
        9524,
        12667,
        16435,
        20883,
        26067,
        32045
      ),
      tolerance = 1e-3
    ),
    "lw_pred matches baseline"
  )

  tidypax_igfs_by_length <-
    tidypax::si_stations(mar) |>
    dplyr::filter(sampling_type %in% 30, nvl(tow_number, 0) %in% 0:35) |>
    tidypax::si_by_length(species = import_defs$species, ldist = function(src) {
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
        dplyr::summarize(count = sum(count, na.rm = TRUE)) |> # NB: les_lengd doesn't group
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
    tidypax::si_by_length(species = import_defs$species, ldist = function(src) {
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
        dplyr::summarize(count = sum(count, na.rm = TRUE)) |> # NB: les_lengd doesn't group
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
