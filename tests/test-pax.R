if (!interactive()) {
  options(warn = 2, error = function() {
    sink(stderr())
    traceback(3)
    q(status = 1)
  })
}
library(unittest)

library(pax)

pretend_pax_fn <- function(vals) {
  pax_decorate(data.frame(val = vals), name = "pretend")
}

ok_group("pax_import:cite", {
  pcon <- pax::pax_connect(":memory:")
  pax_import(pcon, pretend_pax_fn("85"))
  pax_import(pcon, pretend_pax_fn("85"), name = "name2", cite = "cite2")
  pax_import(pcon, data.frame(val = 1:100), name = "name3", cite = "cite3")
  ok(
    ut_cmp_equal(
      pax:::ut_as_sort_df(pax_contents(pcon)),
      data.frame(
        tbl_name = c("name2", "name3", "pretend"),
        citation = c("cite2", "cite3", "pretend_pax_fn(\"85\")")
      )
    ),
    "pax_contents: Use function name by default, overrides worked"
  )
})

ok_group("pax_import:name", {
  pcon <- pax::pax_connect(":memory:")
  lovely_table <- data.frame(val = 1:100)
  pax_import(pcon, lovely_table)
  pax_import(pcon, lovely_table, name = "lovelier_table")

  ok(
    ut_cmp_equal(
      pax:::ut_as_sort_df(pax_contents(pcon)),
      data.frame(
        tbl_name = c("lovelier_table", "lovely_table"),
        citation = NA_character_
      )
    ),
    "pax_contents: Used variable name when available"
  )
  ok(
    ut_cmp_error(
      pax_import(pcon, data.frame(val = 1:100)),
      "No table name supplied"
    ),
    "pax_import: If we can't derive a name, fall over"
  )
})

ok_group("pax_import:csvread", {
  pcon <- pax::pax_connect(":memory:")
  lovely_table <- data.frame(val = 1:100)
  lovely_csv <- tempfile(fileext = ".csv")
  write.csv(lovely_table, file = lovely_csv, row.names = FALSE)
  pax_import(pcon, lovely_csv, cite = lovely_csv)

  ok(
    ut_cmp_equal(
      pax:::ut_as_sort_df(pax_contents(pcon)),
      data.frame(
        tbl_name = c("lovely_csv"),
        citation = lovely_csv
      )
    ),
    "pax_contents: Imported CSV, used provided citation"
  )

  ok(
    ut_cmp_equal(
      pax:::ut_as_sort_df(dplyr::tbl(pcon, "lovely_csv")),
      lovely_table
    ),
    "lovely_csv: Table imported"
  )
})
