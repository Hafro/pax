#' Length distribution functions
#'
#' Functions to compute, scale and plot length-frequency distributions from
#' survey data.
#'
#' @param tbl A dplyr query, typically from the station table
#' @name pax_ldist
NULL

#' @param lgroups Numeric vector of length group lower bounds
#' @param regions Named list mapping region names to vectors of division codes
#' @param gear_group Named list mapping gear group names to vectors of
#'   ``mfdb_gear_code`` values
#' @param tgroup Named list mapping temporal group names to vectors of month
#'   integers, or ``NULL`` to use a single annual group
#' @param ygroup Named list mapping year group names to vectors of year
#'   integers, or ``NULL`` for one group per year
#' @param aldist_tbl A dplyr query from the aldist table, pre-aggregated by
#'   ``sample_id``, ``species``, ``length``, and ``age``
#' @return \subsection{pax_ldist_alk}{A dplyr query with columns for grouping
#'   variables, ``age``, and ``agep`` (proportion at age within each length
#'   group)}
#' @rdname pax_ldist
# Was tidypax::si_make_alk
pax_ldist_alk <- function(
  tbl,
  lgroups = seq(0, 200, 5),
  regions = list(all = 101:115),
  gear_group = list(
    Other = 'VAR',
    BMT = c('BMT', 'NPT', 'SHT', 'PGT'),
    LLN = 'LLN',
    DSE = c('PSE', 'DSE')
  ),
  tgroup = NULL,
  ygroup = NULL,
  aldist_tbl = dplyr::tbl(dbplyr::remote_con(tbl), "aldist") |>
    # NB: aldist isn't aggregated by it's columns: https://github.com/Hafro/pax/issues/17
    dplyr::group_by(sample_id, species, length, age) |>
    dplyr::summarize(
      count = sum(count, na.rm = TRUE),
      weight = sum(weight * count, na.rm = TRUE) / sum(count, na.rm = TRUE)
    )
) {
  pax_checkcols(tbl, "sample_id", expected = "station")
  pax_checkcols(aldist_tbl, "sample_id", "age", "count", expected = "aldist")
  pcon <- dbplyr::remote_con(tbl)

  # NSE variables
  sample_id <- NULL
  species <- NULL
  age <- NULL
  count <- NULL
  weight <- NULL
  region <- NULL
  gear_name <- NULL
  lgroup <- NULL
  n <- NULL

  # NB: This did rename gear -> mfdb_gear_code, moved to pax_si.hafropax()
  tbl |>
    dplyr::left_join(aldist_tbl, by = c('sample_id')) |>
    pax_add_groupings(
      groupings = pax_def_groupings(
        regions = regions,
        gear_group = gear_group,
        lgroups = lgroups,
        tgroup = tgroup,
        ygroup = ygroup,
      )
    ) |>
    dplyr::mutate(
      count = if_else(is.na(age), 0, count),
      region = coalesce(region, 'all')
    ) |>
    dplyr::filter(count > 0) |>
    dplyr::group_by(ygroup, gear_name, region, species, tgroup, lgroup, age) |>
    dplyr::summarise(n = sum(count, na.rm = TRUE)) |>
    dplyr::group_by(ygroup, gear_name, region, species, tgroup, lgroup) |>
    dplyr::mutate(
      agep = ifelse(sum(n, na.rm = TRUE) == 0, 1, n / sum(n, na.rm = TRUE))
    ) |>
    #dplyr::left_join(matp) |>
    dplyr::filter(!is.na(age)) |>
    dplyr::select(-n)
}

#' @return \subsection{pax_ldist_scale_round}{A dplyr query with the ``length``
#'   column rounded to the nearest integer}
#' @rdname pax_ldist
pax_ldist_scale_round <- function(tbl) {
  tbl |> dplyr::mutate(length = round(length))
}

#' @param lw_coeffs_tbl A dplyr query or table name for length-weight
#'   coefficients, with columns ``a``, ``b``, and optionally ``species`` and
#'   ``sex`` (or the columns of ``by``)
#' @param by Columns to match the coefficients on. ``NULL`` (default): the
#'   columns ``species`` and ``sex`` that ``lw_coeffs_tbl`` has, as before.
#'   With ``by``, an ``NA`` in a key column of ``lw_coeffs_tbl`` matches any
#'   value: a row with ``maturity_stage`` ``NA`` holds for all maturity
#'   stages of its species and sex, unless a row with that stage exists
#'   (rows are matched from the most specific, all keys given, to the least).
#'   E.g. ``by = c("species", "sex", "maturity_stage")`` for shrimp, whose
#'   weight differs by maturity stage
#' @param missing What to do with rows without coefficients: ``"default"``
#'   (the default) a = 0.01 and b = 3, a fish in cm and g; ``"stop"`` stop
#'   with an error naming them; ``"zero"`` give them weight 0 (as the old
#'   shrimp scripts did for unsexed or unstaged animals, whose weight then
#'   goes to the others when weights are scaled to the sample weight). With
#'   ``"stop"`` and ``"zero"`` a species with no coefficients at all is an
#'   error, since a = 0.01, b = 3 is wrong for e.g. shrimp lengths in mm.
#'   Rows with ``length`` ``NA`` (stations without the species) are never
#'   an error
#' @return \subsection{pax_ldist_add_weight}{A dplyr query with a ``weight``
#'   column added, calculated as ``a * length^b``}
#' @rdname pax_ldist
pax_ldist_add_weight <- function(
  tbl,
  lw_coeffs_tbl = "lw_coeffs",
  by = NULL,
  missing = c("default", "stop", "zero")
) {
  pcon <- dbplyr::remote_con(tbl)
  missing <- match.arg(missing)

  a <- NULL # Mask NSE variable
  b <- NULL # Mask NSE variable
  lw_coeffs_tbl <- pax_temptbl(pcon, lw_coeffs_tbl)
  lw_coeffs_tbl_colnames <- colnames(lw_coeffs_tbl)

  if (is.null(by)) {
    out <- tbl |>
      dplyr::left_join(
        lw_coeffs_tbl,
        by = intersect(lw_coeffs_tbl_colnames, c("species", "sex"))
      )
  } else {
    out <- ldist_join_lw_wildcard(tbl, lw_coeffs_tbl, by)
  }

  if (missing == "default") {
    out <- out |>
      dplyr::mutate(
        a = ifelse(is.na(a), 0.01, a),
        b = ifelse(is.na(b), 3.00, b),
        weight = a * length^b
      )
  } else {
    ldist_check_lw(out, lw_coeffs_tbl, missing)
    # "zero" (and lengths NA under "stop"): no coefficients, weight 0
    out <- out |>
      dplyr::mutate(weight = ifelse(is.na(a) | is.na(b), 0, a * length^b))
  }
  dplyr::select(out, -a, -b)
}

# Join length-weight coefficients on the columns of `by`, NA in a key column
# of the coefficients matching any value; the most specific match wins
ldist_join_lw_wildcard <- function(tbl, lw, by) {
  a <- b <- a_k <- b_k <- NULL # NSE
  missing_cols <- setdiff(c(by, "a", "b"), colnames(lw))
  if (length(missing_cols) > 0) {
    stop("lw_coeffs_tbl has no column(s) ", paste(missing_cols, collapse = ", "))
  }
  out <- tbl |>
    dplyr::mutate(a = NA_real_, b = NA_real_)
  # From all keys given to the first key only
  for (k in rev(seq_along(by))) {
    keys <- by[seq_len(k)]
    rest <- setdiff(by, keys)
    lw_k <- lw
    for (col in keys) {
      lw_k <- dplyr::filter(lw_k, !is.na(!!rlang::sym(col)))
    }
    for (col in rest) {
      lw_k <- dplyr::filter(lw_k, is.na(!!rlang::sym(col)))
    }
    lw_k <- lw_k |>
      dplyr::select(dplyr::all_of(keys), a_k = "a", b_k = "b")
    out <- out |>
      dplyr::left_join(lw_k, by = keys) |>
      dplyr::mutate(
        a = dplyr::coalesce(a, a_k),
        b = dplyr::coalesce(b, b_k)
      ) |>
      dplyr::select(-"a_k", -"b_k")
  }
  out
}

# Stop if a species has no coefficients, or (missing = "stop") any row with a
# length has none
ldist_check_lw <- function(out, lw, missing) {
  a <- species <- n <- NULL # NSE
  if ("species" %in% colnames(out)) {
    sp_lw <- lw |> dplyr::distinct(species) |> dplyr::pull(species)
    sp <- out |>
      dplyr::filter(!is.na(length), !is.na(species)) |>
      dplyr::distinct(species) |>
      dplyr::pull(species)
    no_coef <- setdiff(sp, sp_lw)
    if (length(no_coef) > 0) {
      stop(
        "No length-weight coefficients for species ",
        paste(no_coef, collapse = ", ")
      )
    }
  }
  if (missing == "stop") {
    bad <- out |>
      dplyr::filter(!is.na(length), is.na(a)) |>
      dplyr::summarise(n = dplyr::n()) |>
      dplyr::pull(n)
    if (bad > 0) {
      stop(bad, " length rows have no length-weight coefficients")
    }
  }
  invisible(TRUE)
}

#' @param towdims_tbl A data.frame of per-sampling-type tow dimension standards
#'   with columns ``sampling_type``, ``min_towlength``, ``max_towlength``,
#'   ``std_towlength``, and ``std_width``
#' @param vfadj_tbl A data.frame of vertical fishing adjustments with columns
#'   ``gear_id`` and ``vf_adj``
#' @return \subsection{pax_ldist_scale_tow_area}{A dplyr query with ``count``
#'   rescaled to fish per square nautical mile}
#' @rdname pax_ldist
# must result in fjoldi/square nautical mile
# Was: tidypax::scale_by_tow_area
pax_ldist_scale_tow_area <-
  function(
    tbl,
    towdims_tbl = data.frame(
      sampling_type = c(30, 35, 31, 37, 19, 34),
      min_towlength = c(2, 2, 0.5, 0.5, 0.5, 0.5),
      max_towlength = c(8, 8, 4, 4, 4, 0.5),
      std_towlength = c(4, 4, 1, 1, 2, 0.5),
      std_width = c(
        17 / 1852,
        17 / 1852,
        17 / 1852,
        27.595 / 1.852^2 / 1000,
        4 / 1852,
        50
      )
    ),
    vfadj_tbl = data.frame(
      gear_id = 78,
      vf_adj = 1.25
    )
  ) {
    pcon <- dbplyr::remote_con(tbl)

    # NSE variables
    species <- NULL
    count <- NULL
    tow_length <- NULL
    max_towlength <- NULL
    std_towlength <- NULL
    min_towlength <- NULL
    std_width <- NULL
    vf_adj <- NULL

    tbl |>
      dplyr::left_join(pax_temptbl(pcon, towdims_tbl)) |>
      dplyr::left_join(pax_temptbl(pcon, vfadj_tbl)) |>
      dplyr::mutate(
        vf_adj = ifelse(species == 19, 1, coalesce(vf_adj, 1)), #temp fix for GSS until index redefined
        std_width = coalesce(std_width, 1)
      ) |> ## for all other gears
      dplyr::mutate(
        tow_length = case_when(
          # Fixed tow length (gillnet survey: each net counts as 0.5 nm)
          min_towlength == max_towlength ~ std_towlength,
          tow_length == 0 ~ 1,
          tow_length > coalesce(max_towlength, 1e6) ~
            coalesce(max_towlength, 1),
          tow_length < coalesce(min_towlength, 0) ~ coalesce(min_towlength, 1),
          TRUE ~ coalesce(tow_length, 1)
        )
      ) |>
      dplyr::mutate(count = count / (tow_length * std_width * vf_adj)) |>
      dplyr::select(
        -c(vf_adj, min_towlength, max_towlength, std_towlength, std_width)
      )
  }

#' @param ldist_tbl A dplyr query from the ldist table, pre-processed with
#'   [pax_ldist_scale_round()]. The ldist table from [pax_mar_ldist()] is
#'   already raised to the counted fish (``mar::skala_med_taldir()``), so
#'   don't apply [pax_ldist_scale_abund()] to it again
#' @return \subsection{pax_ldist_by_year}{A dplyr query of length distributions
#'   aggregated by species, year, sex, length, and gear}
#' @rdname pax_ldist
# Was: tidypax::ldist_by_year
pax_ldist_by_year <- function(
  tbl, # probably dplyr::tbl(pcon, "station")
  ldist_tbl = dplyr::tbl(dbplyr::remote_con(tbl), "ldist") |>
    pax_ldist_scale_round()
) {
  con <- dbplyr::remote_con(tbl)

  # NSE variables
  species <- NULL
  year <- NULL
  sex <- NULL
  mfdb_gear_code <- NULL
  count <- NULL
  n <- NULL

  tbl |>
    dplyr::left_join(ldist_tbl, by = 'sample_id') |>
    dplyr::group_by(species, year, sex, length, mfdb_gear_code) |>
    dplyr::summarise(n = sum(count, na.rm = TRUE)) |>
    dplyr::ungroup() |>
    dplyr::select(
      year,
      mfdb_gear_code,
      species,
      length,
      sex,
      n
    )
}

#' @param measurement_tbl A dplyr query from the measurement table, used to
#'   compute the ratio of counted (CNT/WEI) to length-measured (LEN/LENM/LENC)
#'   fish for abundance scaling
#' @return \subsection{pax_ldist_scale_abund}{A dplyr query with ``count``
#'   scaled up to represent total abundance based on subsample ratios. Only
#'   for unraised length counts: the ldist table from [pax_mar_ldist()] is
#'   already raised}
#' @rdname pax_ldist
# Was: mar::skala_med_taldir
pax_ldist_scale_abund <- function(
  tbl,
  measurement_tbl = dplyr::tbl(dbplyr::remote_con(tbl), "measurement")
) {
  # NSE variables
  sample_id <- NULL
  species <- NULL
  measurement_type <- NULL
  count <- NULL
  ratio_count_cnt <- NULL
  ratio_count_len <- NULL
  ratio <- NULL

  # Was: biota.skala_v
  ratio_tbl <- measurement_tbl |>
    dplyr::group_by(sample_id, species) |>
    dplyr::summarize(
      ratio_count_len = coalesce(
        sum(
          ifelse(
            measurement_type %in% c('LEN', 'LENM', 'LENC', 'OTOL'),
            count,
            0
          ),
          na.rm = TRUE
        ),
        0
      ),
      ratio_count_cnt = coalesce(
        sum(ifelse(measurement_type %in% c('CNT', 'WEI'), count, 0)),
        0
      )
    )

  tbl |>
    dplyr::left_join(ratio_tbl) |>
    dplyr::mutate(
      ratio = ifelse(
        ratio_count_cnt == 0,
        1,
        1 + ratio_count_cnt / ifelse(ratio_count_len == 0, 1, ratio_count_len)
      )
    ) |>
    dplyr::mutate(count = count * ratio) |>
    dplyr::select(-ratio, -ratio_count_len, -ratio_count_cnt)
}

#' @param scale Numeric; ``1`` to plot proportions (default), any other value
#'   to plot raw counts
#' @param expand Boolean, whether to expand the data to fill all
#'   length/year combinations with zeroes
#' @return \subsection{pax_ldist_plot}{A ggplot2 faceted plot of length
#'   distributions by year, with mean length and sample size annotations}
#' @rdname pax_ldist
# Was: tidypax::ldist_plot
pax_ldist_plot <- function(tbl, scale = 1, expand = FALSE) {
  pcon <- if (is.data.frame(tbl)) NULL else dbplyr::remote_con(tbl)
  # NB: Not ifelse(scale == 1, ...), which on a data.frame gives one value
  #     per group
  p_expr <- if (isTRUE(scale == 1)) quote(n / sum(n, na.rm = TRUE)) else quote(n)

  # NSE variables
  year <- NULL
  n <- NULL
  p <- NULL
  mp <- NULL
  mL <- NULL

  summ.dat <-
    tbl |>
    dplyr::group_by(year) |>
    dplyr::summarise(
      mL = sum(length * n, na.rm = TRUE) / sum(n, na.rm = TRUE),
      n = sum(n, na.rm = TRUE)
    )

  if (expand) {
    ldist <-
      tbl |>
      dplyr::full_join(
        tbl |>
          dplyr::select(year, length) |>
          dplyr::distinct() |>
          dplyr::collect() |>
          tidyr::expand(year, length) |>
          (function(x) if (is.null(pcon)) x else pax_temptbl(pcon, x))(),
        by = c('year', 'length')
      )
  } else {
    ldist <- tbl
  }

  ldist |>
    dplyr::group_by(year, length) |>
    dplyr::summarise(n = sum(n, na.rm = TRUE)) |>
    dplyr::group_by(year) |>
    dplyr::mutate(p = !!p_expr) |>
    dplyr::group_by(length) |>
    dplyr::mutate(mp = mean(p, na.rm = TRUE)) |>
    ggplot2::ggplot(ggplot2::aes(length, p)) +
    ggplot2::geom_density(stat = 'identity', fill = '#045a8d', col = NA) +
    ggplot2::geom_line(
      data = ldist |>
        dplyr::group_by(year, length) |>
        dplyr::summarise(n = sum(n, na.rm = TRUE)) |>
        dplyr::group_by(year) |>
        dplyr::mutate(p = !!p_expr) |>
        dplyr::group_by(length) |>
        dplyr::summarise(mp = mean(p, na.rm = TRUE)),
      ggplot2::aes(y = mp)
    ) +
    ggplot2::labs(x = 'Length', y = 'Proportion of catches') +
    ggplot2::theme_light() +
    ggplot2::theme(
      legend.position = 'right',
      legend.key.size = ggplot2::unit(0.4, "cm"),
      legend.text = ggplot2::element_text(size = 7),
      strip.background = ggplot2::element_blank(),
      strip.text = ggplot2::element_blank()
    ) +
    ggplot2::geom_label(
      data = ldist |> dplyr::select(year) |> dplyr::distinct(),
      fill = 'white',
      label.size = 0.2,
      ggplot2::aes(label = year, group = 1),
      x = -Inf,
      y = Inf,
      size = 3,
      vjust = 1.1,
      hjust = -0.1
    ) +
    ggplot2::geom_text(
      data = summ.dat,
      x = Inf,
      y = Inf,
      vjust = 1.1,
      hjust = 1,
      ggplot2::aes(label = paste0('ML = ', round(mL))),
      size = 3
    ) +
    ggplot2::geom_text(
      data = summ.dat,
      x = Inf,
      y = Inf,
      vjust = 2.2,
      hjust = 1,
      ggplot2::aes(label = paste0('n = ', round(n))),
      size = 3
    ) +
    ggplot2::facet_wrap(~year)
}

#' @param ldist A data.frame or dplyr query of length distributions, with
#'   columns ``year``, ``mfdb_gear_code``, ``length``, and ``n``
#' @param max_height Maximum ridge height in plot units
#' @param split_by_sex Boolean, whether to produce separate facets for each sex
#' @return \subsection{pax_ldist_joy_plot}{A ggplot2 ridgeline plot of length
#'   distributions faceted by gear and optionally by sex}
#' @rdname pax_ldist
# Was: tidypax::ldist_joy_plot
pax_ldist_joy_plot <- function(ldist, max_height = 50, split_by_sex = FALSE) {
  # NSE variables
  year <- NULL
  mfdb_gear_code_desc <- NULL
  n <- NULL
  sex <- NULL
  p <- NULL

  if (!split_by_sex) {
    pdat <-
      ldist |>
      pax_describe_mfdb_gear_code() |>
      dplyr::collect(n = Inf) |>
      dplyr::group_by(year, mfdb_gear_code_desc, length) |>
      dplyr::summarise(n = sum(n, na.rm = TRUE), .groups = 'drop') |>
      dplyr::group_by(year, mfdb_gear_code_desc) |>
      dplyr::mutate(p = n / sum(n)) |>
      dplyr::ungroup()
  } else {
    pdat <-
      ldist |>
      dplyr::collect(n = Inf) |>
      dplyr::left_join(pax_describe_mfdb_gear_code(), by = 'mfdb_gear_code') |>
      tidyr::drop_na(sex) |>
      dplyr::mutate(
        sex = dplyr::case_when(
          sex == 1 ~ 'Male',
          sex == 2 ~ 'Female',
          .default = as.character(sex)
        )
      ) |>
      dplyr::group_by(year, mfdb_gear_code_desc, sex) |>
      dplyr::mutate(p = n / sum(n)) |>
      dplyr::ungroup()
  }

  pdat |>
    ggplot2::ggplot(ggplot2::aes(
      length,
      forcats::fct_rev(forcats::as_factor(year)),
      group = year,
      height = max_height * p
    )) +
    ggridges::geom_ridgeline(fill = '#045a8d', alpha = 0.5) +
    {
      if (split_by_sex) {
        ggplot2::facet_wrap(~ mfdb_gear_code_desc + sex, ncol = 6)
      } else {
        ggplot2::facet_wrap(~mfdb_gear_code_desc, ncol = 5)
      }
    } +
    ggplot2::theme_bw() +
    ggplot2::theme(strip.background = ggplot2::element_blank()) +
    ggplot2::labs(y = 'Year', x = 'Length')
}
