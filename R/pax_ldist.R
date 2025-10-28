comment('
> pax_ldist(pcon) |> head()
Joining with `by = join_by(synis_id, tegund_nr)`
# Source:   SQL [?? x 5]
# Database: OraConnection
  sample_id species length   sex count
  <chr>       <dbl>  <dbl> <dbl> <dbl>
1 7228            9     62     1     1
2 7228            9     69     1     1
3 7228            9     67     1     1
4 7228            9     65     1     1
5 7228            9     64     1     1
6 7228            9     61     1     1

> pax_aldist(pcon) |> head()
# Source:   SQL [?? x 4]
# Database: OraConnection
  sample_id species length   age
  <chr>       <dbl>  <dbl> <dbl>
1 7228            9     62    10
2 7228            9     69     9
3 7228            9     67     9
4 7228            9     65     9
5 7228            9     64     9
6 7228            9     61     9
')

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
