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
