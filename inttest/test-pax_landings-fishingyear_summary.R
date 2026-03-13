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
  year_start = 1979,
  year_end = lubridate::year(Sys.Date())
)

test_db_path <- "/tmp/test-pax_landings-fishingyear_summary.duckdb"
if (!file.exists(test_db_path)) {
  pcon <- pax::pax_connect(test_db_path)
  pax_import(
    pcon,
    pax_mar_landings(
      mar,
      import_defs$species,
      year_start = import_defs$year_start,
      year_end = import_defs$year_end
    )
  )
} else {
  pcon <- pax::pax_connect(test_db_path)
}

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

ok_group("pax::pax_landings_fishingyear_summary:ignore-empty-month", {
  # Newpax uses dump of landadur_afli() for species / ICES area
  cmp_tbl <-
    dplyr::tbl(pcon, "landings") |>
    # NB: Filter yearly landings, which won't be present in tidypax_lnd_by_fishing_year
    dplyr::filter(!is.na(month)) |>
    pax::pax_landings_fishingyear_summary() |>
    dplyr::collect() |>
    dplyr::full_join(
      tidypax_lnd_by_fishing_year |> dplyr::collect(),
      by = "fishing_year",
      suffix = c(".newpax", ".tidypax")
    ) |>
    dplyr::mutate(diff = catch_kt.newpax - catch_kt.tidypax) |>
    dplyr::arrange(fishing_year)
  ok(
    all(
      cmp_tbl |>
        dplyr::filter(
          !(fishing_year %in% c("1979", "1980", "1981", "1992/1993"))
        ) |>
        dplyr::pull(diff) ==
        0
    ),
    "~all entries match tidypax"
  )
})

ok_group("pax::pax_landings_fishingyear_summary:with-empty-month", {
  # Compare without filtering - only latter years should match
  cmp_tbl <-
    dplyr::tbl(pcon, "landings") |>
    pax::pax_landings_fishingyear_summary() |>
    dplyr::collect() |>
    dplyr::full_join(
      tidypax_lnd_by_fishing_year |> dplyr::collect(),
      by = "fishing_year",
      suffix = c(".newpax", ".tidypax")
    ) |>
    dplyr::mutate(diff = catch_kt.newpax - catch_kt.tidypax) |>
    dplyr::arrange(fishing_year)
  ok(
    all(
      cmp_tbl |>
        dplyr::filter(as.integer(substr(fishing_year, 1, 4)) > 2012) |>
        dplyr::pull(diff) ==
        0
    ),
    "latter entries match tidypax"
  )
})
