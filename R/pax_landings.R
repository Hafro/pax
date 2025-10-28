comment('
> pax_landings(pcon) |> dplyr::filter(species == 2, year == 2000) |> head()
# Source:   SQL [?? x 9]
# Database: OraConnection
  species mfdb_gear_code ices_area  year month ices_division country boat_id landings
    <dbl> <chr>          <chr>     <dbl> <dbl> <chr>         <chr>     <dbl>    <dbl>
1       2 DSE            5a         2000     2 5a            Iceland    1751      836
2       2 DSE            5a         2000     2 5a            Iceland    1751       44
3       2 BMT            5a         2000     6 5a            Iceland     310     3877
4       2 BMT            5a         2000     9 5a            Iceland    2154     1310
5       2 LLN            5a         2000     9 5a            Iceland    7402      310
6       2 LLN            5a         2000    11 5a            Iceland    7462      262
')

# Was: tidypax::landings_by_gear
pax_landings.hafropax <- function(pcon) {
  mar::landadur_afli(pcon$dbcon) |>
    pax::pax_tbl(pcon = pcon) |>
    dplyr::select(
      species = tegund_nr,
      mfdb_gear_code = mfdb_gear_code,
      ices_area = ices_svaedi,
      year = ar,
      month = man,
      ices_division = ices_svaedi,
      country = land,
      boat_id = skip_nr,
      landings = magn_oslaegt
    )
}
