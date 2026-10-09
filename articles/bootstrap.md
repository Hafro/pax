# Bootstrap data for Gadget models

The Gadget assessments (blue ling 07-bli, Greenland halibut 22-ghl,
beaked redfish 61-reb) estimate uncertainty by refitting the model to
bootstrap data sets. The old setup used
[`mfdb::mfdb_bootstrap_group()`](https://rdrr.io/pkg/mfdb/man/mfdb_aggregate_group.html):
the subdivisions of an area are resampled with replacement, and the
samples of a subdivision drawn k times count k times. The length, age
and maturity distributions computed from the resampled stations then
vary as they would between surveys of the same design.

pax does the same on a pax database, without mfdb:

- [`pax_bootstrap_group()`](https://hafro.github.io/pax/reference/pax_bootstrap.md):
  the drawn subdivisions of each replicate;
- [`pax_bootstrap_table()`](https://hafro.github.io/pax/reference/pax_bootstrap.md):
  the same as a data.frame (one row per draw);
- [`pax_bootstrap_resample()`](https://hafro.github.io/pax/reference/pax_bootstrap.md):
  a station table with each station repeated as often as its subdivision
  was drawn;
- [`pax_bootstrap_lognormal()`](https://hafro.github.io/pax/reference/pax_bootstrap.md):
  log-normal errors on an index that isn’t recomputed from resampled
  stations.

Each replicate is reproducible on its own: replicate `i` depends only on
`seed` and `i`, and is the `i`-th draw of `mfdb_bootstrap_group()` with
the same seed and subdivisions. Replicate 0 is the original data, so one
function gives both the base data and the replicates.

## Drawing subdivisions

``` r

library(pax)
library(dplyr, warn.conflicts = FALSE)

group <- list("1" = c(1011, 1012, 1013, 1021, 1022))
pax_bootstrap_group(group, replicate = 0:2, seed = 2298)
#> [[1]]
#> [[1]]$`1`
#> [1] 1011 1012 1013 1021 1022
#> 
#> 
#> [[2]]
#> [[2]]$`1`
#> [1] 1012 1021 1012 1013 1021
#> 
#> 
#> [[3]]
#> [[3]]$`1`
#> [1] 1013 1021 1013 1011 1013
pax_bootstrap_table(group, replicate = 2, seed = 2298)
#>   replicate area subdivision boot_copy
#> 1         2    1        1011         1
#> 2         2    1        1013         1
#> 3         2    1        1013         2
#> 4         2    1        1013         3
#> 5         2    1        1021         1
```

A replicate does not depend on the others (replicate 5 is the same alone
or as the fifth of five), so targets can compute each in its own branch:

``` r

identical(
  pax_bootstrap_group(group, 1:5, seed = 2298)[[5]],
  pax_bootstrap_group(group, 5, seed = 2298)[[1]]
)
#> [1] TRUE
```

and it matches mfdb (pax uses the random number generators mfdb set,
with the old “Rounding” sampler; `sample_kind = "Rejection"` uses R’s
current default instead):

``` r

if (requireNamespace("mfdb", quietly = TRUE)) {
  old <- mfdb::mfdb_bootstrap_group(5, mfdb::mfdb_group("1" = group[[1]]), seed = 2298)
  all.equal(unclass(old[[5]])[["1"]], pax_bootstrap_group(group, 5, seed = 2298)[[1]][["1"]])
}
#> [1] TRUE
```

The random number state of the session is restored afterwards, so the
bootstrap does not change other random draws.

## Resampling stations

[`pax_bootstrap_resample()`](https://hafro.github.io/pax/reference/pax_bootstrap.md)
takes a station table (a data.frame or a query on a pax database) with a
`gridcell` or `subdivision` column. It finds the subdivision of each
gridcell (by default from the package’s `gridcell` table; the old mfdb
models used mfdb’s reitmapping, which 07-bli passes as `division_tbl`),
and repeats each station by the number of draws:

``` r

set.seed(1)
data("gridcell", package = "pax")
cells <- gridcell[gridcell$subdivision %in% group[[1]], ]
station <- data.frame(
  sample_id = 1:40,
  year = 2024,
  gridcell = sample(cells$gridcell, 40, replace = TRUE)
)
ldist <- do.call(rbind, lapply(station$sample_id, function(i) {
  data.frame(sample_id = i, length = round(rnorm(30, 50 + i %% 7, 8)), count = 1)
}))

boot_mean_length <- function(replicate) {
  station |>
    pax_bootstrap_resample(group, replicate, seed = 2298) |>
    inner_join(ldist, by = "sample_id", relationship = "many-to-many") |>
    summarise(replicate = replicate, stations = n_distinct(sample_id, boot_copy),
              mean_length = sum(length * count) / sum(count))
}
bind_rows(lapply(0:5, boot_mean_length))
#>   replicate stations mean_length
#> 1         0       40    52.82083
#> 2         1       41    52.47317
#> 3         2       38    53.37982
#> 4         3       35    53.09714
#> 5         4       23    53.37391
#> 6         5       32    52.79688
```

The result has the columns `subdivision`, `boot_copy` (1, 2, … for each
copy of a station) and the area name (`area_col`, `NULL` for none). Join
it to the length or age data by sample: a station drawn twice counts
twice. In dplyr the join warns of a many-to-many relationship; that is
by design (07-bli muffles the warning). On a pax database the query
stays lazy:

``` r

pcon <- pax_connect()
#> This is removed when the R session ends.
#> • Extensions are re-downloaded each session.
#> • Secrets are lost.
pax_import(pcon, station)
tbl(pcon, "station") |>
  pax_bootstrap_resample(group, 1, seed = 2298) |>
  count(subdivision, boot_copy) |>
  arrange(subdivision, boot_copy) |>
  collect()
#> # A tibble: 5 × 3
#>   subdivision boot_copy     n
#>         <int>     <int> <dbl>
#> 1        1012         1     4
#> 2        1012         2     4
#> 3        1013         1     7
#> 4        1021         1    13
#> 5        1021         2    13
DBI::dbDisconnect(pcon)
```

Leave out subdivisions with too little data before drawing, as the old
setup did (07-bli: fewer than 230 blue ling in the autumn survey since
2001; 22-ghl: fewer than 5 a year). Groups of one subdivision are not
resampled.

## Indices with log-normal errors

A combined index that adds foreign surveys, or a stratified index whose
strata drop out when their subdivisions aren’t drawn, is perturbed
instead of recomputed:

``` r

index <- c(120, 135, 150, 110, 160)
cv <- 0.2
pax_bootstrap_lognormal(index, sdlog = sqrt(log(1 + cv^2)), replicate = 1, seed = 2299)
#> [1]  86.07992 164.45882 112.70733 133.25577 206.28960
pax_bootstrap_lognormal(index, sdlog = sqrt(log(1 + cv^2)), replicate = 0, seed = 2299)
#> [1] 120 135 150 110 160
```

The errors have mean 1 (`mean_unbiased = TRUE`) or median 1 (`FALSE`, as
22-ghl’s old code). The old code drew them unseeded; the stock repos
seed them with `seed + 1`, so they are reproducible but independent of
the station draws.

## In a stock repo

The bootstrap targets of 07-bli, 22-ghl and 61-reb
(`R/gadget_bootstrap.R`, off by default, switched on by an environment
variable such as `BLI_GADGET_BOOT=1`) follow this pattern:

``` r
# 07-bli/script_assessment_model.R (shortened)
tar_target(boot_group, bli_boot_group(pax_db, model_years)),
tar_target(boot_replicate, seq_len(gadget_boot_n)),
tar_target(
  boot_data,
  bli_boot_data(gadget_data, pax_db, model_years, survey_indices,
                boot_group, boot_replicate, gadget_boot_seed),
  pattern = map(boot_replicate)
),
tar_target(boot_runs, bli_boot_refit(boot_data, ...), pattern = map(boot_data))
```

Each replicate’s data are the standard data with the distributions of
the resampled stations and the perturbed index; each refit starts from
the standard fit’s parameters and likelihood weights.

## AI use

This vignette was drafted with Claude (Anthropic) in October 2026 and
has not yet been checked by a person. (MFRI policy on AI use.)
