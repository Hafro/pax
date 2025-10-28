comment('
> pax_measurement(pcon) |> dplyr::filter(species == 2) |> head()
# Source:   SQL [?? x 13]
# Database: OraConnection
  individual_id sample_id species measurement_type length_cm   age   sex maturity_stage weight_g gonad_weight gut_weight liver_weight count
  <chr>         <chr>       <dbl> <chr>                <dbl> <dbl> <dbl>          <dbl>    <dbl>        <dbl>      <dbl>        <dbl> <dbl>
1 23282522      537068          2 LEN                     35    NA    NA             NA       NA           NA         NA           NA     1
2 23282523      537068          2 LEN                     32    NA    NA             NA       NA           NA         NA           NA     1
3 23282524      537068          2 LEN                     51    NA    NA             NA       NA           NA         NA           NA     1
4 23282525      537068          2 LEN                     32    NA    NA             NA       NA           NA         NA           NA     1
5 23282526      537068          2 LEN                     30    NA    NA             NA       NA           NA         NA           NA     1
6 23282527      537068          2 LEN                     54    NA    NA             NA       NA           NA         NA           NA     1
')
# Was: tidypax::sampling_tables, tidypax::age_reading_status
pax_measurement.hafropax <- function(pcon) {
  kvarna_nr <- NULL # Mask NSE variable

  mar::les_maelingu(pcon$dbcon) |>
    pax::pax_tbl(pcon = pcon) |>
    # TODO: From tidypax::sampling_tables - good idea, or should we expose it & let pax_sampling_detail do it, if so, what is it?
    dplyr::mutate(
      maeling_teg = ifelse(
        maeling_teg == 'OTOL' & is.na(kvarna_nr),
        'LEN',
        maeling_teg
      )
    ) |>
    dplyr::select(
      individual_id = maeling_id,
      sample_id = synis_id,
      species = tegund_nr,
      measurement_type = maeling_teg,
      length_cm = lengd,
      age = aldur,
      sex = kyn_nr,
      maturity_stage = kynthroski_nr,
      weight_g = thyngd,
      gonad_weight = kynfaeri,
      gut_weight = magi,
      liver_weight = lifur,
      count = fjoldi
    )
}
