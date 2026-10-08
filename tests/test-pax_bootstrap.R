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

grp <- list(
  "1" = c(1011L, 1012L, 1013L, 1021L, 1022L, 1031L, 1032L),
  "2" = c(1101L, 1102L, 1111L),
  "3" = 1146L
)

# mfdb's own draws (mfdb warns about the "Rounding" sampler; with warn = 2
# that would be an error, and mfdb would then fall back to another sampler)
mfdb_draws <- function(n, group, seed) {
  op <- options(warn = 0)
  on.exit(options(op))
  rk <- RNGkind()
  on.exit(suppressWarnings(RNGkind(rk[1], rk[2], rk[3])), add = TRUE)
  out <- suppressWarnings(
    mfdb::mfdb_bootstrap_group(n, do.call(mfdb::mfdb_group, group), seed = seed)
  )
  lapply(unclass(out), function(x) lapply(x, as.vector))
}

ok_group("pax_bootstrap_group: the draws of mfdb_bootstrap_group", {
  ours <- pax_bootstrap_group(grp, 1:5, seed = 2298)
  theirs <- mfdb_draws(5, grp, seed = 2298)
  ok(ut_cmp_equal(ours, theirs), "Replicates 1-5 as mfdb (seed 2298)")
  ok(
    ut_cmp_equal(pax_bootstrap_group(grp, 4, seed = 2298)[[1]], theirs[[4]]),
    "Replicate 4 alone is mfdb's 4th replicate"
  )
  ok(
    ut_cmp_equal(
      pax_bootstrap_group(grp, 1:3, seed = 1337),
      mfdb_draws(3, grp, seed = 1337)
    ),
    "Another seed (1337) as mfdb"
  )
  ok(
    ut_cmp_equal(
      pax_bootstrap_group(do.call(mfdb::mfdb_group, grp), 2, seed = 2298),
      pax_bootstrap_group(grp, 2, seed = 2298)
    ),
    "An mfdb_group gives the same as a list"
  )
})

ok_group("pax_bootstrap_group: reproducible, original group, RNG state kept", {
  a <- pax_bootstrap_group(grp, 3, seed = 2459)
  pax_bootstrap_group(grp, 1:10, seed = 99)
  ok(
    ut_cmp_identical(a, pax_bootstrap_group(grp, 3, seed = 2459)),
    "Same seed and replicate, same draw"
  )
  ok(
    !isTRUE(all.equal(a, pax_bootstrap_group(grp, 3, seed = 2460))),
    "Another seed, another draw"
  )
  ok(
    !isTRUE(all.equal(a, pax_bootstrap_group(grp, 2, seed = 2459))),
    "Another replicate, another draw"
  )
  ok(
    ut_cmp_identical(pax_bootstrap_group(grp, 0, seed = 1)[[1]], grp),
    "Replicate 0 is the original group"
  )
  ok(
    ut_cmp_identical(a[[1]][["3"]], 1146L),
    "A group of one subdivision is not resampled"
  )
  ok(
    all(mapply(function(x, g) length(x) == length(g) && all(x %in% g), a[[1]], grp)),
    "Each group keeps its size and draws from its own subdivisions"
  )

  rk <- RNGkind()
  set.seed(42)
  x1 <- stats::runif(3)
  set.seed(42)
  pax_bootstrap_group(grp, 1:3, seed = 2298)
  pax_bootstrap_lognormal(1:3, 0.2, 2, seed = 7)
  x2 <- stats::runif(3)
  ok(ut_cmp_identical(x1, x2), "The session's random numbers are not disturbed")
  ok(ut_cmp_identical(RNGkind(), rk), "The session's RNG kinds are restored")

  ok(ut_cmp_error(pax_bootstrap_group(grp, -1, seed = 1), "replicate"), "Negative replicate")
  ok(ut_cmp_error(pax_bootstrap_group(list(1:3), 1, seed = 1), "named list"), "Unnamed group")
})

ok_group("pax_bootstrap_table", {
  bt <- pax_bootstrap_table(grp, 1:2, seed = 2298)
  draws <- pax_bootstrap_group(grp, 1:2, seed = 2298)
  ok(
    ut_cmp_equal(
      as.vector(table(bt$replicate)),
      c(11L, 11L)
    ),
    "One row per draw"
  )
  r1 <- bt[bt$replicate == 1 & bt$area == "1", ]
  ok(
    ut_cmp_equal(
      sort(r1$subdivision),
      sort(draws[[1]][["1"]])
    ),
    "Rows are the drawn subdivisions"
  )
  ok(
    ut_cmp_equal(
      tapply(r1$boot_copy, r1$subdivision, max),
      tapply(draws[[1]][["1"]], draws[[1]][["1"]], length),
      check.attributes = FALSE
    ),
    "boot_copy counts the copies of a subdivision"
  )
  ok(
    ut_cmp_equal(
      nrow(pax_bootstrap_table(grp, 0, seed = 1)),
      11L
    ),
    "Replicate 0: each subdivision once"
  )
})

ok_group("pax_bootstrap_resample", {
  stations <- data.frame(
    sample_id = 1:8,
    subdivision = c(1011L, 1011L, 1012L, 1013L, 1021L, 1101L, 1146L, 9999L)
  )
  draw <- pax_bootstrap_group(grp, 3, seed = 2298)[[1]]
  times <- function(s) sum(unlist(draw) == s)

  out <- pax_bootstrap_resample(stations, grp, 3, seed = 2298)
  ok(
    ut_cmp_equal(
      as.vector(table(factor(out$sample_id, levels = 1:8))),
      sapply(stations$subdivision, times)
    ),
    "data.frame: each station as many times as its subdivision was drawn"
  )
  ok(!(8 %in% out$sample_id), "Stations outside the group are dropped")
  ok(
    ut_cmp_equal(sort(unique(out$area)), sort(names(grp)[sapply(grp, function(g) any(g %in% out$subdivision))])),
    "The area column has the group names"
  )

  base <- pax_bootstrap_resample(stations, grp, 0, seed = 2298)
  ok(
    ut_cmp_equal(sort(base$sample_id), 1:7),
    "Replicate 0: every station in the group once"
  )

  lazy <- pax:::ut_tbl(pcon, stations) |>
    pax_bootstrap_resample(grp, 3, seed = 2298) |>
    dplyr::collect() |>
    as.data.frame()
  ok(
    ut_cmp_equal(
      pax:::ut_as_sort_df(lazy[, names(out)]),
      pax:::ut_as_sort_df(out),
      check.attributes = FALSE
    ),
    "A query gives the same rows as a data.frame"
  )

  # gridcell -> subdivision from the internal mapping (as pax_add_regions())
  gc <- data.frame(sample_id = 1:3, gridcell = c(6322L, 6684L, 7133L))
  g2 <- list(a = c(1101L, 1032L), b = 1133L)
  out_gc <- pax_bootstrap_resample(gc, g2, 1, seed = 5)
  ok(
    ut_cmp_equal(
      sort(unique(out_gc$subdivision)),
      sort(unique(unlist(pax_bootstrap_group(g2, 1, seed = 5)[[1]])))
    ),
    "gridcell mapped to subdivision with the internal gridcell table"
  )
  ok(
    ut_cmp_equal(
      pax_bootstrap_resample(gc, g2, 1, seed = 5) |> dplyr::arrange(sample_id, boot_copy),
      pax:::ut_tbl(pcon, gc) |>
        pax_bootstrap_resample(g2, 1, seed = 5) |>
        dplyr::collect() |>
        as.data.frame() |>
        dplyr::arrange(sample_id, boot_copy),
      check.attributes = FALSE
    ),
    "gridcell: a query gives the same rows as a data.frame"
  )
  own_map <- data.frame(gridcell = c(6322L, 6684L, 7133L), subdivision = c(1L, 1L, 2L))
  out_own <- pax_bootstrap_resample(gc, list(x = 1:2), 0, seed = 1, division_tbl = own_map)
  ok(ut_cmp_equal(sort(out_own$subdivision), c(1L, 1L, 2L)), "division_tbl replaces the mapping")

  ok(
    ut_cmp_error(pax_bootstrap_resample(stations, grp, 1:2, seed = 1), "one replicate"),
    "One replicate at a time"
  )
  ok(
    ut_cmp_error(pax_bootstrap_resample(out, grp, 1, seed = 1, area_col = "a2"), "boot_copy"),
    "No resampling twice"
  )
})

ok_group("pax_bootstrap_lognormal", {
  x <- c(10, 20, 30, 40)
  ok(ut_cmp_identical(pax_bootstrap_lognormal(x, 0.2, 0, seed = 1), x), "Replicate 0 unchanged")
  r2 <- pax_bootstrap_lognormal(x, 0.2, 2, seed = 1)
  pax_bootstrap_lognormal(x, 0.2, 5, seed = 1)
  ok(ut_cmp_identical(r2, pax_bootstrap_lognormal(x, 0.2, 2, seed = 1)), "Reproducible")
  ok(!isTRUE(all.equal(r2, pax_bootstrap_lognormal(x, 0.2, 3, seed = 1))), "Replicates differ")
  ok(ut_cmp_identical(pax_bootstrap_lognormal(x, 0, 2, seed = 1), x), "sdlog 0: unchanged")
  m <- sapply(1:400, function(i) pax_bootstrap_lognormal(1, 0.3, i, seed = 3))
  ok(abs(mean(m) - 1) < 0.05, "Mean-unbiased errors have mean about 1")
  md <- sapply(1:400, function(i) pax_bootstrap_lognormal(1, 0.3, i, seed = 3, mean_unbiased = FALSE))
  ok(abs(stats::median(md) - 1) < 0.05, "Median-unbiased errors have median about 1")
})
