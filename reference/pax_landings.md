# Summarise and visualise landings data

Functions to aggregate, plot and tabulate commercial landings data.

## Usage

``` r
pax_landings_by_gear(
  tbl,
  gear_group = list(Other = "Var", Other = pax_add_other(), BMT = c("BMT", "NPT", "SHT",
    "PGT"), LLN = "LLN", DSE = c("PSE", "DSE"))
)

pax_landings_boat_summary(tbl)

pax_landings_significantboats_summary(tbl)

pax_add_fishing_year(tbl)

pax_landings_fishingyear_summary(tbl, ignore_final_year = TRUE)
```

## Arguments

- tbl:

  A dplyr query from a landings table

- gear_group:

  Named list mapping gear group names to vectors of `mfdb_gear_code`
  values

- ignore_final_year:

  Boolean, exclude the final (likely incomplete) year?

## Value

### pax_landings_by_gear

A dplyr query summarising catch and boat counts by year, species, gear,
country, and ICES area

### pax_landings_boat_summary

A data.frame with catch and boat counts by gear and year, suitable for a
summary table. Input should be from `pax_landings_by_gear()`.

### pax_landings_significantboats_summary

A dplyr query with columns `year`, `n` (number of vessels accounting for
95%% of catch), and `catch` (in kt). Input should be from
`pax_landings_by_gear()`.

### pax_landings_fishingyear_summary

Adds a `fishing_year` column to the incoming landings table

### pax_landings_fishingyear_summary

A dplyr query with columns `fishing_year` and `catch_kt`, ordered by
fishing year
