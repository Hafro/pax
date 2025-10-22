pax_ldist.hafropax <- function(pcon) {
  mar::les_lengd(pcon$dbcon) |>
    pax::pax_tbl(pcon = pcon) |>
    mar::skala_med_taldir() |>
    dplyr::select(
      sample_id = synis_id,
      species = tegund_nr,
      length = lengd,
      sex = kyn_nr,
      count = fjoldi
    )
}

pax_aldist.hafropax <- function(pcon) {
  mar::les_aldur(pcon$dbcon) |>
    pax::pax_tbl(pcon = pcon) |>
    dplyr::select(
      sample_id = synis_id,
      species = tegund_nr,
      length = lengd,
      age = aldur
    )
}

# Was: mar::skala_med_taldir
pax_ldist_scale_abund.hafropax <- function(tbl) {
  r <- NULL # Mask NSE variable

  ratio <- mar::les_skala(pax::as_pax(tbl)$dbcon) |>
    dplyr::mutate(maeldir = nvl(maeldir, 1)) |>
    dplyr::mutate(
      r = ifelse(
        nvl(taldir, 0) == 0,
        1,
        1 + taldir / ifelse(maeldir > 0, maeldir, 1)
      )
    ) |>
    dplyr::select(sample_id, species, r)

  tbl |>
    dplyr::left_join(ratio) |>
    dplyr::mutate(count = count * r)
}
