# Map plotting functions

Functions to create base maps and add spatial layers for survey and
catch data around Iceland.

## Usage

``` r
pax_map_base(
  plot_greenland = FALSE,
  plot_faroes = FALSE,
  xlim = c(-26.5, -12),
  ylim = c(63, 67.25),
  low_res = FALSE
)

pax_map_layer_depth(base, ocean_depth_tbl, depth_lines = c(-100, -500, -1000))

pax_map_layer_catch(
  base,
  data,
  breaks = c(seq(0, 20, by = 3), 40, 60),
  fill_lab = "Catch (t/nm2)",
  annotation = "year",
  alpha = 0.5,
  na.fill = -1,
  label_x = -18.5,
  label_y = 65
)
```

## Arguments

- plot_greenland:

  Boolean, include Greenland coastline?

- plot_faroes:

  Boolean, include Faroe Islands coastline?

- xlim:

  Numeric vector of length 2, longitude limits

- ylim:

  Numeric vector of length 2, latitude limits

- low_res:

  Boolean, use low-resolution world map?

- base:

  A ggplot2 map object, as returned by `pax_map_base()`

- ocean_depth_tbl:

  A dplyr query with columns `lon`, `lat`, and `ocean_depth`

- depth_lines:

  Numeric vector of depth contour values to draw (negative, in metres)

- data:

  A dplyr query with columns `year`, `lon`, `lat`, and `catch`, and
  optionally `mfdb_gear_code`

- breaks:

  Numeric vector of contour fill break points

- fill_lab:

  Legend label for the fill scale

- annotation:

  Character, type of facet annotation: `"year"` or `"gear"`

- alpha:

  Numeric, fill transparency (0–1)

- na.fill:

  Fill value used for missing catch data in contour interpolation

- label_x, label_y:

  Coordinates for the annotation label

## Value

### pax_map_base

A ggplot2 object with Iceland coastline and a clean theme, ready for
additional layers

### pax_map_layer_depth

The `base` ggplot2 object with depth contour lines added

### pax_map_layer_catch

The `base` ggplot2 object with a filled contour catch layer and facets
added
