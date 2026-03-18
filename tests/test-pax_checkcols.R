if (!interactive()) {
  options(warn = 2, error = function() {
    sink(stderr())
    traceback(3)
    q(status = 1)
  })
}
library(unittest)

library(pax)

pcon <- pax::pax_connect(":memory:")

do_check <- function(tbl) {
  # NB: NULL parameters are filtered
  pax_checkcols(tbl, "a", "b", NULL, "c")
  return(TRUE)
}

ok(
  ut_cmp_error(do_check(data.frame(a = 1, b = 2)), "do_check: c\n"),
  "Missing column, NULL ignored"
)
ok(do_check(data.frame(a = 1, b = 2, c = 3)), "ok, NULL ignored")
