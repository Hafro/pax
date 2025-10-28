comment('
> pax_quotatransfer(pcon, 2)
Joining with `by = join_by(fteg)`
# A tibble: 36 × 14
    fteg timabil varanlegt jofnsj m_ara kvoti  afli stada  tilf eftir upptaka
   <dbl> <chr>       <dbl>  <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>   <dbl>
 1     2 1991/91     48769     NA     0 48772 40214  8558 -3454  5104     354
 2     2 1991/92     51531     NA  4618 56300 45379 10921 -4453  6468     788
 3     2 1992/93     62970     NA  6834 70018 45043 24975 -5226 19749     566
 4     2 1993/94     60637    105 11194 74446 52743 21703 -3306 18397      24
 5     2 1994/95     60702   3206 11010 71720 56687 15033 -2159 12874      38
 6     2 1995/96     54817   2362 10218 65047 51063 13985 -2985 11000       9
 7     2 1996/97     42811      0  8995 51808 48543  3264   471  3735      28
 8     2 1997/98     42813      0  3364 46178 33917 12261 -4459  7802      13
 9     2 1998/99     32500      0  7320 39830 38514  1316   145  1461      34
10     2 1999/00     32497    463  1412 33924 33071   854   725  1579      31
')
# TODO: Species naming should match biota.measure
# TODO: Species filtering here isn't normal
# Was: tidypax::quota_transfer_table & tidypax::quota_transfer_plot (common section at start)
pax_quotatransfer.hafropax <- function(pcon, species) {
  mar:::kvoti_stada_summarised(pcon$dbcon) |>
    pax::pax_tbl(pcon = pcon) |>
    dplyr::filter(fteg == local(species)) |>
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
