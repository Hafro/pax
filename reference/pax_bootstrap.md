# Spatial bootstrap of survey and commercial samples

A port of the spatial bootstrap of
[`mfdb::mfdb_bootstrap_group()`](https://rdrr.io/pkg/mfdb/man/mfdb_aggregate_group.html),
as the MFRI Gadget assessments used it (e.g. blue ling, Greenland
halibut and beaked redfish): the subdivisions of each area group are
resampled with replacement, and the samples (stations) of a subdivision
drawn k times count k times in the replicate, so that length, age and
maturity distributions and survey indices computed from the resampled
stations vary as they would between surveys of the same design.

## Usage

``` r
pax_bootstrap_group(
  group,
  replicate,
  seed,
  sample_kind = c("Rounding", "Rejection")
)

pax_bootstrap_table(
  group,
  replicate,
  seed,
  sample_kind = c("Rounding", "Rejection")
)

pax_bootstrap_resample(
  tbl,
  group,
  replicate,
  seed,
  division_tbl = NULL,
  area_col = "area",
  sample_kind = c("Rounding", "Rejection")
)

pax_bootstrap_lognormal(x, sdlog, replicate, seed, mean_unbiased = TRUE)
```

## Arguments

- group:

  Named list of area groups: area name to a vector of subdivisions (e.g.
  `list("1" = unique(gridcell$subdivision))`), or an
  [`mfdb::mfdb_group()`](https://rdrr.io/pkg/mfdb/man/mfdb_aggregate_group.html).
  Groups of one subdivision are not resampled, as in mfdb.

- replicate:

  Replicate numbers (1, 2, ...; 0 for the original group).
  `pax_bootstrap_resample()` takes one.

- seed:

  Seed of the bootstrap (an integer).

- sample_kind:

  Sampler of [`set.seed()`](https://rdrr.io/r/base/Random.html):
  "Rounding" (default) gives the draws of mfdb (R \< 3.6 sampler, as
  mfdb sets it), "Rejection" the current R default.

- tbl:

  Query (a pax table) or data.frame of samples, with a `gridcell` or a
  `subdivision` column, e.g. stations. Each row is repeated as many
  times as its subdivision was drawn in the replicate, and rows of
  subdivisions not drawn (or not in `group`) are dropped, so that
  everything joined to the result by sample counts the same number of
  times.

- division_tbl:

  Mapping of `gridcell` to `subdivision` (data.frame or table), used
  when `tbl` has no `subdivision` column. Defaults to the internal
  [gridcell](https://hafro.github.io/pax/reference/gridcell.md) mapping;
  give mfdb's reitmapping (with lower-case column names) to use the
  subdivisions of the old mfdb models.

- area_col:

  Name of the column that gets the area group name, or NULL for none.

- x:

  Numeric vector of index values (e.g. a survey index by year) for
  `pax_bootstrap_lognormal()`, for indices that are not computed from
  resampled stations.

- sdlog:

  Standard deviation of the log-normal error (one value, or one per
  value of `x`); for a CV, `sqrt(log(1 + cv^2))`.

- mean_unbiased:

  If TRUE (default) the errors have mean 1 (log mean `-sdlog^2/2`),
  otherwise median 1.

## Value

### pax_bootstrap_group

List with one element per replicate, each a named list like `group` with
the drawn subdivisions

### pax_bootstrap_table

data.frame with columns `replicate`, `area` (group name), `subdivision`
and `boot_copy`: one row for each time a subdivision was drawn (a
subdivision drawn twice has rows with `boot_copy` 1 and 2; subdivisions
not drawn have no row)

### pax_bootstrap_resample

`tbl` with its rows repeated by the number of draws of their
subdivision, and the columns `subdivision`, `boot_copy` and `area_col`.
Same type as `tbl` (a query stays a query)

### pax_bootstrap_lognormal

`x` times seeded log-normal errors. Replicate `i` is determined by
`seed`, `i` and `length(x)`; replicate 0 returns `x` unchanged

## Details

Every replicate is reproducible: replicate `i` is determined by `seed`
and `i` alone. It is the `i`-th draw after `set.seed(seed)` with the
random number generators mfdb used (Mersenne-Twister, Inversion, and by
default the "Rounding" sampler), so
`pax_bootstrap_group(group, i, seed)` gives the same subdivisions as
`mfdb::mfdb_bootstrap_group(n, group, seed)[[i]]` for any `n >= i`. The
random number state of the session is restored afterwards. Replicate 0
is the original group (each subdivision once), so the same code gives
the base data.

## Examples

``` r
g <- list("1" = c(1011, 1012, 1013, 1021, 1022))
pax_bootstrap_group(g, 1:2, seed = 2298)
#> [[1]]
#> [[1]]$`1`
#> [1] 1012 1021 1012 1013 1021
#> 
#> 
#> [[2]]
#> [[2]]$`1`
#> [1] 1013 1021 1013 1011 1013
#> 
#> 
pax_bootstrap_table(g, 1, seed = 2298)
#>   replicate area subdivision boot_copy
#> 1         1    1        1012         1
#> 2         1    1        1012         2
#> 3         1    1        1013         1
#> 4         1    1        1021         1
#> 5         1    1        1021         2
```
