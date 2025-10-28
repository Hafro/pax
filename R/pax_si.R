comment('
> pax_si(pcon) |> dplyr::filter(year == 2000) |> head()
# Source:   SQL [?? x 23]
# Database: OraConnection
  sample_id  year month station trip    gridcell begin_lat begin_lon end_lat end_lon depth mfdb_gear_code rectangle tow_number gear_id tow_length sampling_type smareitur tow_start tow_end skiki fjardarreitur fixed
  <chr>     <dbl> <dbl>   <dbl> <chr>      <dbl>     <dbl>     <dbl>   <dbl>   <dbl> <dbl> <chr>              <dbl>      <dbl>   <dbl>      <dbl>         <dbl>     <dbl> <dttm>    <dttm>  <dbl>         <dbl> <dbl>
1 274398     2000     3      NA MRK.ERL       NA      59.1      5.27    59.1    5.27    NA PSE                   NA         NA      10         NA            90        NA NA        NA         NA            NA     0
2 274399     2000     3      NA MRK.ERL       NA      59.4      5.25    59.4    5.25    NA PSE                   NA         NA      10         NA            90        NA NA        NA         NA            NA     0
3 274400     2000     3      NA MRK.ERL       NA      59.4      5.25    59.4    5.25    NA PSE                   NA         NA      10         NA            90        NA NA        NA         NA            NA     0
4 274401     2000     3      NA MRK.ERL       NA      60.6      4.87    60.6    4.87    NA PSE                   NA         NA      10         NA            90        NA NA        NA         NA            NA     0
5 274402     2000     3      NA MRK.ERL       NA      60.9      4.42    60.9    4.42    NA PSE                   NA         NA      10         NA            90        NA NA        NA         NA            NA     0
6 274403     2000     3      NA MRK.ERL       NA      60.4      4.72    60.4    4.72    NA PSE                   NA         NA      10         NA            90        NA NA        NA         NA            NA     0
')

#' @title MFRI survey indices
#'
#' @description
#'
#'  Generate a survey station list with default stratification
#'
#' @param Hafropax connection
#'
#' @return query to the database for station information
#' @export
# TODO: Was si_stations, removed postfix for consistency, but may make less sense?
pax_si.hafropax <- function(pcon) {
  mar::les_stod(pcon$dbcon) |>
    pax::pax_tbl(pcon = pcon) |>
    ## skip MAGEI and MOGUN, these are stomach samples and should be a seperate sampling type
    dplyr::filter(
      # TODO: lifted from sampling_overview_plot(), sensible?
      !(leidangur %like% 'MAG%'),
      !(leidangur %like% 'MO%')
    ) |>
    dplyr::mutate(gridcell = 10 * reitur + smareitur) |> ## change to nautical miles^2
    dplyr::left_join(mar::les_syni(pcon$dbcon), by = 'stod_id') |>
    dplyr::mutate(
      station = reitur * 10000 + nvl(tog_nr, 0) * 100 + veidarfaeri
    ) |> ## change to nautical miles^2
    dplyr::left_join(
      pax::pax_tbl(mar::tbl_mar(pcon$dbcon, 'biota.gear_mapping'), pcon = pcon),
      by = 'veidarfaeri'
    ) |>
    dplyr::select(
      sample_id = synis_id,
      year = ar,
      month = man,
      station,
      trip = leidangur,
      gridcell,
      begin_lat = kastad_breidd,
      begin_lon = kastad_lengd,
      end_lat = hift_breidd,
      end_lon = hift_lengd,
      depth = botndypi_kastad,
      # NB: Extracted rename from si_by_age
      mfdb_gear_code = gear,
      rectangle = reitur,
      tow_number = tog_nr,
      gear_id = veidarfaeri,
      tow_length = toglengd,
      sampling_type = synaflokkur_nr,
      smareitur,
      tow_start = togbyrjun,
      tow_end = togendir,
      skiki,
      fjardarreitur
    ) |>
    dplyr::mutate(
      fixed = case_when(
        sampling_type == 30 &
          (rectangle * 100 + nvl(tow_number, 0)) %in%
            c(
              27401,
              37212,
              37302,
              41214,
              41412,
              46211,
              46212,
              46214,
              46216,
              46311,
              46312,
              46313,
              51301,
              52413,
              56214,
              57412,
              62311,
              71912,
              72314
            ) ~
          0,
        sampling_type == 30 & tow_number %in% 1:19 ~ 1,
        sampling_type == 30 &
          tow_number %in% c(31, 32) &
          rectangle %in%
            c(319, 321, 367, 370, 371, 372, 414, 415, 422, 474, 523) ~
          1,
        TRUE ~ 0
      )
    )
}
