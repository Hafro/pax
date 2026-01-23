if (!interactive()) {
  options(warn = 2, error = function() {
    sink(stderr())
    traceback(3)
    q(status = 1)
  })
}
library(unittest)

library(pax)

# NB: We shouldn't need read-write priviledges to do this
pcon <- pax::pax_connect(":memory:", read_only = TRUE)

#### Storing data.frames with hash names
x1 <- pax::pax_temptbl(pcon, data.frame(a = 1:10000))
x2 <- pax::pax_temptbl(pcon, data.frame(a = 1:10000))
x3 <- pax::pax_temptbl(pcon, data.frame(a = 1:10001))

ok(
  ut_cmp_identical(as.character(x1$lazy_query), as.character(x2$lazy_query)),
  "x1 & x2 are the same table"
)
ok(
  any(as.character(x1$lazy_query) != as.character(x3$lazy_query)),
  "x1 & x3 are not the same table"
)
