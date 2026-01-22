pax_ldist_scale_round <- function(tbl) {
  tbl |> dplyr::mutate(length = round(length))
}

pax_ldist_add_weight <- function(
  tbl,
  lw_coeffs_tbl = "lw_coeffs"
) {
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
    max_towlength <- NULL # Mask NSE variable
    std_towlength <- NULL # Mask NSE variable
    min_towlength <- NULL # Mask NSE variable
    std_width <- NULL # Mask NSE variable
    vf_adj <- NULL # Mask NSE variable

    tbl |>
      dplyr::left_join(pax_temptbl(pcon, towdims_tbl, force_tbl = TRUE)) |> # NB: copy_inline is losing precision
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

# Was: tidypax::ldist_by_year
pax_ldist_by_year <- function(
  tbl,
  sampling_type,
  species,
  ldist = pax_ldist(con) |> pax_ldist_scale_round() |> pax_ldist_scale_abund()
) {
  con <- dbplyr::remote_con(tbl)

  tbl |> # TODO: Was pax_si(con)
    dplyr::filter(
      sampling_type %in% local(sampling_type),
      species %in% local(species)
    ) |>
    dplyr::left_join(ldist, by = 'sample_id') |>
    dplyr::group_by(species, year, sex, length, mfdb_gear_code) |>
    dplyr::summarise(n = sum(count)) |>
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

# Was: tidypax::ldist_plot
pax_ldist_plot <- function(tbl, scale = 1, expand = FALSE) {
  pcon <- dbplyr::remote_con(tbl)

  summ.dat <-
    tbl |>
    dplyr::group_by(year) |>
    dplyr::summarise(mL = sum(length * n) / sum(n), n = sum(n))

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
    dplyr::summarise(n = sum(n)) |>
    dplyr::group_by(year) |>
    dplyr::mutate(p = ifelse(local(scale) == 1, n / sum(n), n)) |>
    dplyr::group_by(length) |>
    dplyr::mutate(mp = mean(p)) |>
    ggplot2::ggplot(ggplot2::aes(length, p)) +
    ggplot2::geom_density(stat = 'identity', fill = '#045a8d', col = NA) +
    ggplot2::geom_line(
      data = ldist |>
        dplyr::group_by(year, length) |>
        dplyr::summarise(n = sum(n)) |>
        dplyr::group_by(year) |>
        dplyr::mutate(p = ifelse(local(scale) == 1, n / sum(n), n)) |>
        dplyr::group_by(length) |>
        dplyr::summarise(mp = mean(p)),
      ggplot2::aes(y = mp)
    ) +
    ggplot2::labs(x = 'Length', y = 'Proportion of catches') +
    ggplot2::theme_light() +
    ggplot2::theme(
      legend.position = 'right',
      legend.key.size = unit(0.4, "cm"),
      legend.text = element_text(size = 7),
      strip.background = element_blank(),
      strip.text = element_blank()
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

# Was: tidypax::ldist_joy_plot
pax_ldist_joy_plot <- function(ldist, max_height = 50, split_by_sex = FALSE) {
  if (!split_by_sex) {
    pdat <-
      ldist |>
      dplyr::collect(n = Inf) |>
      dplyr::left_join(pax_add_mfdb_gear_code_desc(), by = 'mfdb_gear_code') |>
      dplyr::mutate(
        description = ifelse(is.na(description), 'Other', description)
      ) |>
      dplyr::group_by(year, description, length) |>
      dplyr::summarise(n = sum(n), .groups = 'drop') |>
      dplyr::group_by(year, description) |>
      dplyr::mutate(p = n / sum(n)) |>
      dplyr::ungroup()
  } else {
    pdat <-
      ldist |>
      dplyr::collect(n = Inf) |>
      dplyr::left_join(pax_add_mfdb_gear_code_desc(), by = 'mfdb_gear_code') |>
      dplyr::mutate(
        description = ifelse(is.na(description), 'Other', description)
      ) |>
      tidyr::drop_na(sex) |>
      dplyr::mutate(
        sex = dplyr::case_when(
          sex == 1 ~ 'Male',
          sex == 2 ~ 'Female',
          .default = as.character(sex)
        )
      ) |>
      dplyr::group_by(year, description, sex) |>
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
        ggplot2::facet_wrap(~ description + sex, ncol = 6)
      } else {
        ggplot2::facet_wrap(~description, ncol = 5)
      }
    } +
    ggplot2::theme_bw() +
    ggplot2::theme(strip.background = element_blank()) +
    ggplot2::labs(y = 'Year', x = 'Length')
}
