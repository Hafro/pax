# TODO: Species naming should match biota.measure
# TODO: Species filtering here isn't normal
# Was: tidypax::quota_transfer_table & tidypax::quota_transfer_plot (common section at start)
pax_quotatransfer.hafropax <- function(pcon, species) {
  mar:::kvoti_stada_summarised(pcon) |>
    pax::pax_tbl(pcon = pcon) |>
    dplyr::filter(fteg == local(species_no)) |>
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
    dplyr::ungroup()
}
