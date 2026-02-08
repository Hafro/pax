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

ok_group("pax_describe_mfdb_gear_code", {
  out <- pax:::ut_tbl(
    pcon,
    data.frame(mfdb_gear_code = c("Badger", "DSE", "LLN"))
  ) |>
    pax_describe_mfdb_gear_code()
  ok(
    ut_cmp_equal(
      pax:::ut_as_sort_df(out),
      data.frame(
        mfdb_gear_code = c("Badger", "DSE", "LLN"),
        mfdb_gear_code_desc = c("Other", "Danish Seine", "Long Line")
      )
    ),
    "pax_describe_mfdb_gear_code: Added values, 'other'"
  )
})
