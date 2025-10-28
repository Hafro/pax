comment('
> pax_catch(pcon, 2) |> head()
# Source:   SQL [?? x 26]
# Database: OraConnection
       id species towtime mfdb_gear_code vessel_nr  year month   lat   lon gridcell depth.original catch total hooks nr_net num_traps gear_size    dx     dy     x     y  area sq                    m      r depth
    <dbl>   <dbl>   <dbl> <chr>              <dbl> <dbl> <dbl> <dbl> <dbl>    <dbl>          <dbl> <dbl> <dbl> <dbl>  <dbl>     <dbl>     <dbl> <dbl>  <dbl> <dbl> <dbl> <dbl> <chr>             <dbl>  <dbl> <dbl>
1 2000169       2     150 BMT                   80  1973     5  63.9 -24.8     3741            100    50  3300    NA     NA        NA        NA 0.125 0.0625 -24.7  63.9  12.4 -24.6875:63.90625 175    0.571 183.
2 2029258       2      90 BMT                 1253  1973     6  64.4 -14.8     4141             NA   100   300    NA     NA        NA        NA 0.125 0.0625 -14.7  64.4  12.1 -14.6875:64.40625  13.2 NA      NA
3 2021446       2     100 BMT                 1137  1973     3  64.4 -13.8     4131             80   300   400    NA     NA        NA        NA 0.125 0.0625 -13.7  64.4  12.1 -13.6875:64.40625  77.9  1.03  146.
4 2022107       2     100 BMT                 1137  1973     7  64.4 -13.8     4131             72  1500  1500    NA     NA        NA        NA 0.125 0.0625 -13.7  64.4  12.1 -13.6875:64.40625  77.9  0.924 132.
5 2019824       2     180 BMT                 1121  1973     9  66.1 -24.8     6243             41   100   800    NA     NA        NA        NA 0.125 0.0625 -24.7  66.2  11.4 -24.6875:66.15625  55.0  0.746  75.0
6 2014397       2     180 BMT                  215  1973     5  63.4 -19.8     3191             60  1000  1700    NA     NA        NA        NA 0.125 0.0625 -19.7  63.4  12.6 -19.6875:63.40625  62.2  0.965 110.
')

# Was tidypax::catch_data
pax_catch.hafropax <- function(
  pcon,
  species,
  year_end = lubridate::year(Sys.Date())
) {
  mar::tbl_mar(pcon$dbcon, 'ops$bthe."logbooks_compiled"') |>
    pax::pax_tbl(pcon = pcon) |>
    dplyr::filter(
      species == local(species),
      year < local(year_end)
    ) |>
    dplyr::rename(mfdb_gear_code = gear)
}
