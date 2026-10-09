# Check that a table contains required columns

Validates that a data frame or table contains all expected columns. If
any columns are missing, an informative error is raised naming the
missing columns, the calling function, and the full list of columns
actually present.

## Usage

``` r
pax_checkcols(tbl, ..., expected = NULL)
```

## Arguments

- tbl:

  A data frame or object with
  [`colnames()`](https://rdrr.io/r/base/colnames.html).

- ...:

  One or more column name strings that must be present in `tbl`.

- expected:

  Either a table name or function call (ending with ()), that will be
  shown as a hint to the correct data source.

## Value

`invisible(NULL)` if all expected columns are present. Otherwise,
[`stop`](https://rdrr.io/r/base/stop.html) is called.

## Examples

``` r
df <- data.frame(a = 1, b = 2)
pax_checkcols(df, "a", "b")   # passes silently
if (FALSE) { # \dontrun{
pax_checkcols(df, "a", "c")   # error: missing column "c"
} # }
```
