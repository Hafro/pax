#' Length distributions of invertebrate surveys
#'
#' Raising, rounding and selection of the length counts of the
#' ``ldist_maturity`` table (see [pax_mar_ldist_maturity()]), as the old
#' shrimp survey scripts did with ``mar::skala_med_afla()``,
#' ``mar::skala_med_taldir()`` and a knife-edge ramp for the fishable
#' biomass.
#'
#' @param tbl A dplyr query of length counts with columns ``sample_id``,
#'   ``species``, ``length`` and ``count``, e.g. ``ldist_maturity``
#' @param sample_weight_tbl The ``sample_weight`` table (see
#'   [pax_mar_sample_weight()]), a table name or a dplyr query
#' @name pax_ldist_survey
NULL

#' @return \subsection{pax_ldist_scale_catch_weight}{``tbl`` with ``count``
#'   raised to the catch weight, as ``mar::skala_med_afla()``: multiplied by
#'   ``catch_weight / sample_weight`` (1 if the catch weight is missing or
#'   0, a missing sample weight taken as 1 kg), ``NA`` if the sample has no
#'   ``sample_weight`` row; ``count_unscaled`` (the count before raising);
#'   and ``mean_weight``, the sample weight divided by the number measured in
#'   the sample (kg per animal; the sample weight itself if none were
#'   measured, 0 without a ``sample_weight`` row). The number measured is
#'   the sum of ``count`` over the rows of the sample and species in
#'   ``tbl``}
#' @rdname pax_ldist_survey
pax_ldist_scale_catch_weight <- function(
  tbl,
  sample_weight_tbl = "sample_weight"
) {
  pcon <- dbplyr::remote_con(tbl)
  # NSE variables
  sample_id <- species <- count <- n_sample <- sample_weight <- NULL
  catch_weight <- r <- sw <- count_unscaled <- NULL

  ratio <- pax_temptbl(pcon, sample_weight_tbl) |>
    dplyr::mutate(sw = dplyr::coalesce(sample_weight, 1)) |>
    dplyr::mutate(
      r = ifelse(
        dplyr::coalesce(catch_weight, 0) == 0,
        1,
        catch_weight / ifelse(sw > 0, sw, 1)
      )
    ) |>
    dplyr::select(sample_id, species, r, sw)

  tbl |>
    dplyr::group_by(sample_id, species) |>
    dplyr::mutate(n_sample = sum(count, na.rm = TRUE)) |>
    dplyr::ungroup() |>
    dplyr::left_join(ratio, by = c("sample_id", "species")) |>
    dplyr::mutate(
      count_unscaled = count,
      count = count * r,
      mean_weight = ifelse(
        n_sample > 0,
        dplyr::coalesce(sw, 0) / n_sample,
        dplyr::coalesce(sw, 0)
      )
    ) |>
    dplyr::select(-r, -sw, -n_sample)
}

#' @return \subsection{pax_ldist_scale_counted}{``tbl`` with ``count``
#'   raised to the counted animals, as ``mar::skala_med_taldir()``:
#'   multiplied by ``1 + n_counted / n_measured`` (1 if none were counted),
#'   ``NA`` if the sample has no ``sample_weight`` row. The ``ldist`` table
#'   of [pax_mar_ldist()] is already raised this way}
#' @rdname pax_ldist_survey
pax_ldist_scale_counted <- function(
  tbl,
  sample_weight_tbl = "sample_weight"
) {
  pcon <- dbplyr::remote_con(tbl)
  # NSE variables
  sample_id <- species <- count <- n_counted <- n_measured <- r <- m <- NULL

  ratio <- pax_temptbl(pcon, sample_weight_tbl) |>
    dplyr::mutate(m = dplyr::coalesce(n_measured, 1)) |>
    dplyr::mutate(
      r = ifelse(
        dplyr::coalesce(n_counted, 0) == 0,
        1,
        1 + n_counted / ifelse(m > 0, m, 1)
      )
    ) |>
    dplyr::select(sample_id, species, r)

  tbl |>
    dplyr::left_join(ratio, by = c("sample_id", "species")) |>
    dplyr::mutate(count = count * r) |>
    dplyr::select(-r)
}

#' @param scale Multiplier of the recorded length, e.g. 10 for shrimp
#'   carapace lengths recorded in cm and used in mm
#' @param unit Length class width after scaling, e.g. 0.5 (mm)
#' @return \subsection{pax_ldist_round_length}{``tbl`` with ``length``
#'   multiplied by ``scale`` and rounded to the nearest ``unit``, halves
#'   up (as Oracle ``ROUND()``, not R's round half to even): the old
#'   shrimp ``lengd10 = round((lengd * 10) / 0.5) * 0.5``}
#' @rdname pax_ldist_survey
pax_ldist_round_length <- function(tbl, scale = 10, unit = 0.5) {
  # The small offset keeps decimal halves (e.g. 1.425 cm = 28.5 half mm)
  # that are just below .5 in binary floating point rounding up, as they do
  # in Oracle's decimal arithmetic
  dplyr::mutate(
    tbl,
    length = floor(length * local(scale) / local(unit) + 0.5 + 1e-9) *
      local(unit)
  )
}

#' @param selection A data frame with columns ``length`` and ``p``: the
#'   selected fraction at each length of a ramp (e.g. the fishable part of
#'   shrimp between 13 and 15.5 mm). Lengths below the smallest get 0, above
#'   the largest 1, and lengths in between must be in the table (the lengths
#'   of ``tbl`` are on the same grid, e.g. after
#'   [pax_ldist_round_length()]). Rows with ``length`` ``NA`` get 1, as in
#'   the old shrimp scripts (their biomass is 0)
#' @param name Name of the new column of fractions
#' @return \subsection{pax_ldist_add_selection}{``tbl`` with a column
#'   ``name`` of selected fractions}
#' @rdname pax_ldist_survey
pax_ldist_add_selection <- function(tbl, selection, name = "p_selected") {
  pcon <- dbplyr::remote_con(tbl)
  # NSE variables
  p_sel <- n <- NULL

  if (!all(c("length", "p") %in% names(selection))) {
    stop("selection needs columns length and p")
  }
  lmin <- min(selection$length)
  lmax <- max(selection$length)
  sel <- pax_temptbl(
    pcon,
    data.frame(length = selection$length, p_sel = selection$p)
  )
  out <- tbl |>
    dplyr::left_join(sel, by = "length") |>
    dplyr::mutate(
      p_sel = dplyr::case_when(
        is.na(length) ~ 1,
        length < local(lmin) ~ 0,
        length > local(lmax) ~ 1,
        TRUE ~ p_sel
      )
    )
  bad <- out |>
    dplyr::filter(is.na(p_sel)) |>
    dplyr::summarise(n = dplyr::n()) |>
    dplyr::pull(n)
  if (bad > 0) {
    stop(
      bad,
      " rows have a length inside the selection ramp that is not in it; ",
      "round the lengths to the grid of the ramp first"
    )
  }
  dplyr::rename_with(out, function(x) name, "p_sel")
}
