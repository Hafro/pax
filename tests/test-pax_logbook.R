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

ok_group("pax_add_cpue", {
  pac <- function(
    catch = 1e5,
    mfdb_gear_code = NA_character_,
    tow_hooks = NA_real_,
    tow_time = NA_real_,
    tow_num_nets = NA_real_
  ) {
    pax:::ut_tbl(
      pcon,
      data.frame(
        catch = catch,
        mfdb_gear_code = mfdb_gear_code,
        tow_hooks = tow_hooks,
        tow_time = tow_time,
        tow_num_nets = tow_num_nets
      )
    ) |>
      pax_add_cpue() |>
      dplyr::pull(cpue)
  }

  ok(ut_cmp_equal(pac(12345), 12345), "cpue defaults to 1")
  ok(ut_cmp_equal(pac(99123), 99123), "catch copied from input")
  ok(ut_cmp_equal(pac(0), numeric(0)), "zero-catch filtered")

  ok(ut_cmp_equal(pac(tow_num_nets = 5), 1e5 / 5), "tow_num_nets used next")
  ok(ut_cmp_equal(pac(tow_num_nets = 0), numeric(0)), "zero-effort filtered")

  ok(
    ut_cmp_equal(pac(tow_hooks = 3000, tow_num_nets = 5), 1e5 / 3),
    "tow_hooks used in preference"
  )
  ok(
    ut_cmp_equal(
      pac(tow_time = 120, tow_hooks = 3000, tow_num_nets = 5),
      1e5 / 2
    ),
    "tow_time used in preference"
  )

  ok(
    ut_cmp_equal(
      pac(
        mfdb_gear_code = "DSE",
        tow_time = 120,
        tow_hooks = 3000,
        tow_num_nets = 5
      ),
      1e5 / 1
    ),
    "DSE gear wins"
  )
})

ok_group("pax_logbook_cpue_plot", {
  # Test that a plot at least generates
  pax:::ut_tbl(
    pcon,
    expand.grid(
      year = 2000:2020,
      mfdb_gear_code = c("LLN", "DSE"),
      cpue = 1e5,
      catch = 1e6,
      catch_total = 10e6
    )
  ) |>
    pax_logbook_cpue_plot() |>
    ggplot2::ggsave(
      filename = tempfile(pattern = "test-pax_logbook", fileext = ".png")
    )
})
