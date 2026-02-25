pax_from_mar <- function(
  species,
  year_start = NULL,
  year_end = NULL,
  sampling_type = c(1, 2, 8, 10, 11, 30, 35),
  ices_area_like = "5a%",
  strata = pax_def_strata_list(),
  mar_opts = list(),
  dbdir = ":memory:"
) {
  pcon <- pax_connect(dbdir = dbdir)

  # Open a connection to upstream hafro DB
  mar <- do.call(mar::connect_mar, mar_opts)
  on.exit(DBI::dbDisconnect(mar), add = TRUE, after = TRUE)

  import_defs <- list(
    mar,
    species = species,
    year_start = year_start,
    year_end = year_end
  )

  pax_import(pcon, pax_marmap_ocean_depth())
  # Extract required tables, place into pcon
  for (s in strata) {
    pax_import(pcon, pax_def_strata(s))
  }
  pax_import(
    pcon,
    pax_mar_station(
      mar,
      species = species,
      year_start = year_start,
      year_end = year_end,
      sampling_type = sampling_type
    )
  )
  pax_import(pcon, do.call(pax_mar_measurement, import_defs))
  pax_import(pcon, do.call(pax_mar_logbook, import_defs))
  pax_import(
    pcon,
    pax_mar_landings(
      mar,
      species = import_defs$species,
      ices_area_like = ices_area_like,
      year_start = import_defs$year_start,
      year_end = import_defs$year_end
    )
  )
  pax_import(pcon, do.call(pax_mar_sampling, import_defs))
  pax_import(pcon, pax_mar_aldist(mar, species = import_defs$species))
  pax_import(pcon, pax_mar_ldist(mar, species = import_defs$species))
  pax_import(
    pcon,
    pax_mar_lw_coeffs(mar, species = import_defs$species)
  )
  return(pcon)
}
