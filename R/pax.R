pax_connect <- function(dbdir = ":memory:") {
  pcon <- DBI::dbConnect(duckdb::duckdb(), dbdir)

  # TODO: Pre-define common schema items? If per-package, where do they hang?

  return(pcon)
}

pax_import <- function(
  pcon,
  tbl,
  name = attr(tbl, "pax_name"),
  cite = attr(tbl, "pax_cite")
) {
  tbl_colnames <- colnames(head(tbl, 0))

  if (!startsWith(name, "paxdat_")) {
    # TODO: Check schema
    # TODO: Populate lookup tables
    # TODO: unique_indexes / indexes arguments
  }

  dplyr::copy_to(pcon, tbl, name = name, temporary = FALSE)

  if (!startsWith(name, "paxdat_")) {
    # Populate ancillary tables
    # TODO: Eventually these will hang of the schema definition
    if ("gridcell" %in% tbl_colnames) {
      pax_dat_gridcell(
        pcon,
        dplyr::tbl(pcon, name) |>
          dplyr::distinct(gridcell) |>
          dplyr::pull(gridcell)
      )
    }
    if (all(c("lat", "lon") %in% tbl_colnames)) {
      # TODO: Separately to gridcell, fetch noaa depth for maps?
    }
    if ("sampling_type" %in% tbl_colnames) {
      pax_dat_sampling_type_desc(pcon)
    }
    if ("mfdb_gear_code" %in% tbl_colnames) {
      pax_dat_mfdb_gear_code_desc(pcon)
    }
  }

  if (!is.null(cite)) {
    # TODO: Copy citation to citation table
  }
}

pax_decorate <- function(tbl, cite = NULL, name = NULL) {
  if (!is.null(cite)) {
    attr(tbl, "pax_cite") <- cite
  }
  if (!is.null(name)) {
    attr(tbl, "pax_name") <- name
  }
  return(tbl)
}

pax_temptbl <- function(pcon, tbl) {
  # TODO: If lots of rows, store as temp tbl first and return that
  dbplyr::copy_inline(pcon, tbl)
}
