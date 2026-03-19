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
    Other = 'Var',
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
#'   ``sex``
#' @return \subsection{pax_ldist_add_weight}{A dplyr query with a ``weight``
#'   column added, calculated as ``a * length^b``}
#' @rdname pax_ldist
pax_ldist_add_weight <- function(
  tbl,
  lw_coeffs_tbl = "lw_coeffs"
) {
  pcon <- dbplyr::remote_con(tbl)

  a <- NULL # Mask NSE variable
  b <- NULL # Mask NSE variable
  lw_coeffs_tbl <- pax_temptbl(pcon, lw_coeffs_tbl)
  lw_coeffs_tbl_colnames <- colnames(lw_coeffs_tbl)

  tbl |>
    dplyr::left_join(
      lw_coeffs_tbl,
      by = intersect(lw_coeffs_tbl_colnames, c("species", "sex"))
    ) |>
    dplyr::mutate(
      a = ifelse(is.na(a), 0.01, a),
      b = ifelse(is.na(b), 3.00, b),
      weight = a * length^b
    ) |>
    dplyr::select(-a, -b)
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
#'   [pax_ldist_scale_round()] and [pax_ldist_scale_abund()]
#' @return \subsection{pax_ldist_by_year}{A dplyr query of length distributions
#'   aggregated by species, year, sex, length, and gear}
#' @rdname pax_ldist
# Was: tidypax::ldist_by_year
pax_ldist_by_year <- function(
  tbl, # probably dplyr::tbl(pcon, "station")
  ldist_tbl = dplyr::tbl(dbplyr::remote_con(tbl), "ldist") |>
    pax_ldist_scale_round() |>
    pax_ldist_scale_abund()
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
#'   scaled up to represent total abundance based on subsample ratios}
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
  pcon <- dbplyr::remote_con(tbl)

  # NSE variables
  year <- NULL
  n <- NULL
  p <- NULL
  mp <- NULL
  unit <- NULL
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
          pax_temptbl(pcon = pcon),
        by = c('year', 'length')
      )
  } else {
    ldist <- tbl
  }

  ldist |>
    dplyr::group_by(year, length) |>
    dplyr::summarise(n = sum(n, na.rm = TRUE)) |>
    dplyr::group_by(year) |>
    dplyr::mutate(p = ifelse(local(scale) == 1, n / sum(n), n)) |>
    dplyr::group_by(length) |>
    dplyr::mutate(mp = mean(p)) |>
    ggplot2::ggplot(ggplot2::aes(length, p)) +
    ggplot2::geom_density(stat = 'identity', fill = '#045a8d', col = NA) +
    ggplot2::geom_line(
      data = ldist |>
        dplyr::group_by(year, length) |>
        dplyr::summarise(n = sum(n, na.rm = TRUE)) |>
        dplyr::group_by(year) |>
        dplyr::mutate(p = ifelse(local(scale) == 1, n / sum(n), n)) |>
        dplyr::group_by(length) |>
        dplyr::summarise(mp = mean(p, na.rm = TRUE)),
      ggplot2::aes(y = mp)
    ) +
    ggplot2::labs(x = 'Length', y = 'Proportion of catches') +
    ggplot2::theme_light() +
    ggplot2::theme(
      legend.position = 'right',
      legend.key.size = unit(0.4, "cm"),
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
      dplyr::collect(n = Inf) |>
      dplyr::left_join(pax_describe_mfdb_gear_code(), by = 'mfdb_gear_code') |>
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
