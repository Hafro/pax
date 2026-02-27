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

if (!file.exists("/tmp/camel.duckdb")) {
  pcon <- pax::pax_from_mar(
    species = import_defs$species,
    year_start = import_defs$year_start,
    year_end = import_defs$year_end,
    dbdir = "/tmp/camel.duckdb"
  )
} else {
  pcon <- pax::pax_connect("/tmp/camel.duckdb")
}

ok_group("pax::pax_landings_fishingyear_summary", {
  # Original fishing year summary, not using landadur_afli(), using veidisvaedi, not ICES area
  tidypax_lnd_by_fishing_year <-
    mar::fiskifelag_oslaegt(mar) |>
    dplyr::filter(ar < 1993) |>
    dplyr::union_all(mar::lods_oslaegt(mar)) |>
    dplyr::filter(substr(timabil, 1, 4) != local(import_defs$year_end - 1)) |>
    dplyr::mutate(
      fishing_year = dplyr::case_when(
        ar < 1991 ~ to_char(ar),
        ar == 1991 & man < 9 ~ to_char(ar),
        man < 9 ~
          paste(
            to_char(to_number(to_char(l_dags, "YYYY")) - 1),
            to_char(l_dags, "YYYY"),
            sep = '/'
          ),
        TRUE ~
          paste(
            to_char(l_dags, "YYYY"),
            to_char(to_number(to_char(l_dags, "YYYY")) + 1),
            sep = '/'
          )
      )
    ) |>
    dplyr::filter(fteg == local(import_defs$species), veidisvaedi == 'I') |>
    dplyr::left_join(mar::vessel(mar), by = c('skip_nr' = 'registration_no')) |>
    dplyr::group_by(fishing_year) |>
    dplyr::summarise(catch_kt = round(sum(magn_oslaegt) / 1000))

  # Newpax uses dump of landadur_afli() for species / ICES area
  newpax_lnd_by_fishing_year <-
    dplyr::tbl(pcon, "landings") |>
    # TODO: We aren't filtering by !is.na(timabil), so do country == "Iceland", which is sorta-similar
    dplyr::filter(!is.na(month)) |>
    #dplyr::filter(country == "Iceland") |>
    pax::pax_landings_fishingyear_summary()

  # TODO: A half-way house, using landadur_afli() but it's own timabil
  bodgepax_lnd <-
    mar::landadur_afli(mar) |>
    dplyr::filter(
      tegund_nr == 2,
      ices_svaedi %like% "5a%",
      # NB: NA timabil entries ~are foreign landings
      !is.na(timabil)
    ) |>
    dplyr::group_by(timabil) |>
    dplyr::summarize(catch_kt = round(sum(magn_oslaegt) / 1000)) |>
    dplyr::collect() |>
    dplyr::mutate(fishing_year = as.character(timabil)) |>
    dplyr::mutate(
      fishing_year = ifelse(
        nchar(fishing_year) < 8,
        fishing_year,
        paste(substr(fishing_year, 1, 4), substr(fishing_year, 5, 8), sep = "/")
      )
    ) |>
    dplyr::select(-timabil) |>
    dplyr::arrange(fishing_year)

  # TODO: Turn this into actual test
  newpax_lnd_by_fishing_year |>
    dplyr::collect() |>
    dplyr::full_join(
      tidypax_lnd_by_fishing_year |> dplyr::collect(),
      by = "fishing_year",
      suffix = c(".newpax", ".tidypax")
    ) |>
    dplyr::full_join(
      bodgepax_lnd |> dplyr::collect(),
      by = "fishing_year",
      suffix = c("", ".bodgepax")
    ) |>
    dplyr::arrange(fishing_year) |>
    as.data.frame()
})
