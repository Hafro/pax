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

ok_group("pax_ldist_scale_tow_area", {
  ldist <- pax:::ut_tbl(
    pcon,
    data.frame(
      sample_id = 1:6,
      species = 6,
      gear_id = 91,
      sampling_type = c(34, 34, 34, 30, 30, 30),
      tow_length = c(NA, 0, 0.4, NA, 0, 10),
      count = 100
    )
  )
  out <- pax_ldist_scale_tow_area(ldist) |>
    dplyr::arrange(sample_id) |>
    dplyr::collect()
  ok(
    ut_cmp_equal(out$count[1:3], rep(100 / (0.5 * 50), 3)),
    "Gillnet survey: every net is 0.5 nm, whatever tow_length says"
  )
  ok(
    ut_cmp_equal(out$count[4:6], 100 / (c(1, 1, 8) * 17 / 1852)),
    "Groundfish survey: missing or 0 tow length is 1, long tows clamped"
  )
})
