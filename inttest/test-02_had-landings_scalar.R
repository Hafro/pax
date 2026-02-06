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

  writeLines("=== pax_marmap_ocean_depth")
  pax_import(pcon, pax::pax_marmap_ocean_depth(), overwrite = TRUE)
  writeLines("=== pax_mar_station")
  pax_import(pcon, do.call(pax_mar_station, import_defs))
  writeLines("=== pax_mar_measurement")
  pax_import(pcon, do.call(pax_mar_measurement, import_defs))
  writeLines("=== pax_mar_logbook")
  pax_import(pcon, do.call(pax_mar_logbook, import_defs))
  writeLines("=== pax_mar_landings")
  pax_import(
    pcon,
    pax_mar_landings(
      mar,
      species = import_defs$species,
      ices_area_like = "5a%",
      year_start = import_defs$year_start,
      year_end = import_defs$year_end
    )
  )
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

ok_group("Commercial catch at age", {
  tidypax_comm_alk <-
    tidypax::si_stations(mar) |>
    dplyr::filter(sampling_type %in% c(1, 2, 8)) |>
    tidypax::si_make_alk(
      tgroup = list(t1 = 1:6, t2 = 7:12),
      gear_group = list(
        Other = 'Var',
        BMT = NA, # Assume unknown gear are BMT
        BMT = c('BMT', 'NPT', 'SHT', 'PGT', 'DRD'),
        LLN = c('HLN', 'LLN', 'GIL'),
        DSE = c('PSE', 'DSE')
      )
    ) |>
    dplyr::filter(species == local(species_code))
  tidypax_catch_at_age <-
    tidypax::si_stations(mar) |>
    dplyr::filter(sampling_type %in% c(1, 2, 8)) |>
    dplyr::mutate(gear = nvl(gear, 'BMT')) |>
    tidypax::si_by_length(species = species_code) |>
    tidypax::si_by_age(
      pre_scaling = function(x, ...) {
        x
      },
      tgroup = list(t1 = 1:6, t2 = 7:12),
      gear_group = list(
        Other = 'Var',
        BMT = c('BMT', 'NPT', 'SHT', 'PGT', 'DRD'),
        LLN = c('HLN', 'LLN', 'GIL'),
        DSE = c('PSE', 'DSE')
      ),
      post_scaling = tidypax:::landings_scalar,
      alk = tidypax_comm_alk
    )

  newpax_comm_alk <-
    dplyr::tbl(pcon, "station") |>
    dplyr::filter(sampling_type %in% c(1, 2, 8)) |>
    dplyr::mutate(mfdb_gear_code = coalesce(mfdb_gear_code, 'BMT')) |>
    pax_ldist_alk(
      tgroup = list(t1 = 1:6, t2 = 7:12),
      gear_group = list(
        Other = 'Var',
        BMT = c('BMT', 'NPT', 'SHT', 'PGT', 'DRD'),
        LLN = c('HLN', 'LLN', 'GIL'),
        DSE = c('PSE', 'DSE')
      )
    )
  newpax_catch_at_age <-
    dplyr::tbl(pcon, "station") |>
    dplyr::filter(sampling_type %in% c(1, 2, 8)) |>
    dplyr::mutate(mfdb_gear_code = coalesce(mfdb_gear_code, 'BMT')) |>
    pax_si_by_length() |>
    pax_si_scale_by_alk(
      tgroup = list(t1 = 1:6, t2 = 7:12),
      gear_group = list(
        Other = 'Var',
        BMT = c('BMT', 'NPT', 'SHT', 'PGT', 'DRD'),
        LLN = c('HLN', 'LLN', 'GIL'),
        DSE = c('PSE', 'DSE')
      ),
      alk = newpax_comm_alk
    ) |>
    pax_si_scale_by_landings(
      tgroup = list(t1 = 1:6, t2 = 7:12),
      gear_group = list(
        Other = 'Var',
        BMT = c('BMT', 'NPT', 'SHT', 'PGT', 'DRD'),
        LLN = c('HLN', 'LLN', 'GIL'),
        DSE = c('PSE', 'DSE')
      ),
      landings_tbl = dplyr::tbl(pcon, "landings") |>
        # Add defaults for missing values
        dplyr::mutate(month = coalesce(month, 6)) |>
        dplyr::mutate(mfdb_gear_code = coalesce(mfdb_gear_code, 'BMT'))
    )

  ok(
    ut_cmp_equal(
      tidypax_comm_alk |>
        dplyr::filter(ygroup == 1990) |>
        dplyr::arrange(ygroup, gear_name, region, tgroup, lgroup, age) |>
        as.data.frame(),
      newpax_comm_alk |>
        dplyr::filter(ygroup == 1990) |>
        dplyr::arrange(ygroup, gear_name, region, tgroup, lgroup, age) |>
        as.data.frame(),
      context.lines = 5
    ),
    "comm_alk: 1990 matches"
  )
  ok(
    ut_cmp_equal(
      tidypax_catch_at_age |>
        dplyr::filter(year == 1990) |>
        # TODO: Rows aren't grouped in tidypax to the same degree. Why?
        dplyr::group_by(
          species,
          tgroup,
          gear_name,
          region,
          sample_id,
          year,
          month,
          station,
          age,
          lgroup
        ) |>
        dplyr::summarise(
          si_abund = sum(adj_N),
          si_biomass = sum(adj_B),
          catch = mean(catch)
        ) |>
        dplyr::select(
          species,
          tgroup,
          gear_name,
          region,
          sample_id,
          year,
          month,
          station,
          age,
          lgroup,
          catch,
          si_abund,
          si_biomass
        ) |>
        dplyr::mutate(
          si_abund = round(si_abund, 2),
          si_biomass = round(si_biomass, 2)
        ) |>
        dplyr::arrange(
          species,
          tgroup,
          gear_name,
          region,
          sample_id,
          year,
          month,
          station,
          age,
          lgroup
        ) |>
        as.data.frame(),
      newpax_catch_at_age |>
        dplyr::filter(year == 1990) |>
        dplyr::group_by(
          species,
          tgroup,
          gear_name,
          region,
          sample_id,
          year,
          month,
          station,
          age,
          lgroup
        ) |>
        dplyr::summarise(
          si_abund = sum(si_abund),
          si_biomass = sum(si_biomass),
          catch = mean(catch)
        ) |>
        dplyr::select(
          species,
          tgroup,
          gear_name,
          region,
          sample_id,
          year,
          month,
          station,
          age,
          lgroup,
          catch,
          si_abund,
          si_biomass
        ) |>
        dplyr::mutate(
          si_abund = round(si_abund, 2),
          si_biomass = round(si_biomass, 2)
        ) |>
        dplyr::arrange(
          species,
          tgroup,
          gear_name,
          region,
          sample_id,
          year,
          month,
          station,
          age,
          lgroup
        ) |>
        as.data.frame(),
      context.lines = 5
    ),
    "catch_at_age: 1990 matches"
  )
})
