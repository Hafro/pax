if (!interactive()) {
  options(warn = 2, error = function() {
    sink(stderr())
    traceback(3)
    q(status = 1)
  })
}
library(unittest)

library(pax)

pcon <- pax_connect(":memory:")

ok_group("pax_landings_fishingyear_summary", {
  tbl <- expand.grid(month = 1:12, year = 2000:2003)
  tbl$catch <- runif(nrow(tbl), 1e5, 1e6)
  out <- pax:::ut_tbl(pcon, tbl) |>
    pax_landings_fishingyear_summary() |>
    as.data.frame()
  ok(
    ut_cmp_equal(
      sort(out$fishing_year),
      c("1999/2000", "2000/2001", "2001/2002")
    ),
    "Got all expected fishing years"
  )
  ok(
    ut_cmp_equal(
      out[out$fishing_year == "1999/2000", "catch_kt"],
      round(
        sum(c(
          tbl |> dplyr::filter(year == 2000, month < 9) |> dplyr::pull(catch),
          NULL
        )) /
          1000
      )
    ),
    "fishing_year: 1999/2000"
  )
  ok(
    ut_cmp_equal(
      out[out$fishing_year == "2000/2001", "catch_kt"],
      round(
        sum(c(
          tbl |> dplyr::filter(year == 2000, month >= 9) |> dplyr::pull(catch),
          tbl |> dplyr::filter(year == 2001, month < 9) |> dplyr::pull(catch),
          NULL
        )) /
          1000
      )
    ),
    "fishing_year: 2000/2001"
  )
  ok(
    ut_cmp_equal(
      out[out$fishing_year == "2001/2002", "catch_kt"],
      round(
        sum(c(
          tbl |> dplyr::filter(year == 2001, month >= 9) |> dplyr::pull(catch),
          tbl |> dplyr::filter(year == 2002, month < 9) |> dplyr::pull(catch),
          NULL
        )) /
          1000
      )
    ),
    "fishing_year: 2001/2002"
  )
})

ok_group("pax_landings_fishingyear_summary:nomonth", {
  tbl <- expand.grid(month = NA_integer_, year = 2000:2003)
  tbl$catch <- runif(nrow(tbl), 1e5, 1e6)
  out <- pax:::ut_tbl(pcon, tbl) |>
    pax_landings_fishingyear_summary() |>
    as.data.frame()
  ok(
    ut_cmp_equal(
      sort(out$fishing_year),
      c("1999/2000", "2000/2001", "2001/2002")
    ),
    "Got all expected fishing years"
  )
  ok(
    ut_cmp_equal(
      out[out$fishing_year == "1999/2000", "catch_kt"],
      round(
        sum(c(
          tbl |> dplyr::filter(year == 2000) |> dplyr::pull(catch),
          NULL
        )) /
          1000
      )
    ),
    "fishing_year: 1999/2000 (i.e. assigned to month 6)"
  )
  ok(
    ut_cmp_equal(
      out[out$fishing_year == "2000/2001", "catch_kt"],
      round(
        sum(c(
          tbl |> dplyr::filter(year == 2001) |> dplyr::pull(catch),
          NULL
        )) /
          1000
      )
    ),
    "fishing_year: 2000/2001 (i.e. assigned to month 6)"
  )
  ok(
    ut_cmp_equal(
      out[out$fishing_year == "2001/2002", "catch_kt"],
      round(
        sum(c(
          tbl |> dplyr::filter(year == 2002) |> dplyr::pull(catch),
          NULL
        )) /
          1000
      )
    ),
    "fishing_year: 2001/2002 (i.e. assigned to month 6)"
  )
})
