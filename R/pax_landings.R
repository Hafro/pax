#' Summarise and visualise landings data
#'
#' Functions to aggregate, plot and tabulate commercial landings data.
#'
#' @param tbl A dplyr query from a landings table
#' @name pax_landings
NULL

#' @param gear_group Named list mapping gear group names to vectors of
#'   ``mfdb_gear_code`` values
#' @return \subsection{pax_landings_by_gear}{A dplyr query summarising catch
#'   and boat counts by year, species, gear, country, and ICES area}
#' @rdname pax_landings
# Was: tidypax::landings_by_gear
pax_landings_by_gear <- function(
  tbl,
  gear_group = list(
    Other = 'Var',
    Other = pax_add_other(),
    BMT = c('BMT', 'NPT', 'SHT', 'PGT'),
    LLN = 'LLN',
    DSE = c('PSE', 'DSE')
  )
) {
  pax_checkcols(
    tbl,
    "year",
    "species",
    "ices_area",
    "country",
    "mfdb_gear_code",
    "boat_id",
    "catch",
    expected = "landings"
  )

  # NSE variables
  year <- NULL
  species <- NULL
  gear_name <- NULL
  country <- NULL
  ices_area <- NULL
  catch <- NULL
  boat_id <- NULL

  tbl |>
    pax_add_gear_group(gear_group) |>
    dplyr::group_by(year, species, gear_name, country, ices_area) |>
    dplyr::summarise(
      catch = sum(catch, na.rm = TRUE),
      num_boats = dplyr::n_distinct(boat_id, na.rm = TRUE)
    )
}

#' @param ylab Y-axis label
#' @param xlab X-axis label
#' @param breaks X-axis tick positions
#' @return \subsection{pax_landings_plot}{A ggplot2 stacked bar chart of
#'   landings by country and year}
#' @rdname pax_landings
# Was tidypax::landings_plot
pax_landings_plot <- function(
  tbl,
  ylab = 'Landings (in kt)',
  xlab = 'Year',
  breaks = seq(0, 1e5, by = 10)
) {
  pax_checkcols(tbl, "year", "country", "catch", expected = "landings")

  # NSE variables
  year <- NULL
  country <- NULL
  catch <- NULL
  desc <- NULL

  tbl |>
    dplyr::group_by(year, country) |>
    dplyr::summarise(c = sum(catch, na.rm = TRUE)) |>
    dplyr::arrange(desc(country)) |>
    ggplot2::ggplot(ggplot2::aes(year, c / 1e3, fill = country)) +
    ggplot2::geom_bar(stat = 'identity') +
    ggplot2::theme_bw() +
    ggplot2::labs(y = ylab, x = xlab, fill = '') +
    ggplot2::theme(
      legend.background = ggplot2::element_blank(),
      legend.position = c(0.15, 0.75)
    ) +
    ggplot2::scale_x_continuous(breaks = breaks) +
    pax_scale_fill_crayola()
}

#' @return \subsection{pax_landings_boat_summary}{A data.frame with catch and
#'   boat counts by gear and year, suitable for a summary table. Input should
#'   be from ``pax_landings_by_gear()``.}
#' @rdname pax_landings
# Was: tidypax::boat_summary_table
pax_landings_boat_summary <- function(tbl) {
  # i.e. pax_landings_by_gear()
  pax_checkcols(
    tbl,
    "year",
    "gear_name",
    "catch",
    "num_boats",
    expected = "pax_landings_by_gear()"
  )

  # NSE variables
  total_catch <- NULL
  mfdb_gear_code_desc <- NULL
  catch <- NULL
  num_boats <- NULL
  year <- NULL
  Year <- NULL

  tbl |>
    # NB: Assumes groups have valid mfdb_gear_code names
    pax_describe_mfdb_gear_code() |>
    dplyr::select(year, mfdb_gear_code_desc, catch, num_boats) |>
    dplyr::collect() |>
    tidyr::pivot_wider(
      names_from = mfdb_gear_code_desc,
      values_from = c(catch, num_boats),
      values_fill = 0
    ) |>
    dplyr::mutate(
      total_catch = rowSums(dplyr::across(dplyr::starts_with('catch_')))
    ) |>
    dplyr::select(
      Year = year,
      dplyr::starts_with('num_boats_'),
      dplyr::starts_with('catch_'),
      total_catch
    )
}

#' @return \subsection{pax_landings_significantboats_summary}{A dplyr query
#'   with columns ``year``, ``n`` (number of vessels accounting for 95%% of
#'   catch), and ``catch`` (in kt). Input should be from
#'   ``pax_landings_by_gear()``.}
#' @rdname pax_landings
# Was: tidypax::num_boats_table
pax_landings_significantboats_summary <- function(
  tbl
) {
  # i.e. pax_landings_boat_summary
  pax_checkcols(
    tbl,
    "year",
    "boat_id",
    "catch",
    expected = "pax_landings_boat_summary()"
  )

  # NSE variables
  year <- NULL
  boat_id <- NULL
  catch <- NULL
  cc <- NULL
  ct <- NULL
  n <- NULL

  tbl |>
    dplyr::group_by(year, boat_id) |>
    dplyr::summarise(c = sum(catch, na.rm = TRUE)) |>
    dplyr::filter(c > 0) |>
    dbplyr::window_order(year, c) |>
    dplyr::group_by(year) |>
    dplyr::mutate(cc = cumsum(c), ct = sum(c, na.rm = TRUE)) |>
    dplyr::filter(cc > 0.05 * ct) |>
    dplyr::summarise(n = n(), catch = sum(c, na.rm = TRUE) / 1e3) |>
    dplyr::select(-cc, -ct) |>
    dplyr::arrange(year)
}

#' @return \subsection{pax_landings_fishingyear_summary}{
#'   Adds a ``fishing_year`` column to the incoming landings table}
#' @rdname pax_landings
pax_add_fishing_year <- function(tbl) {
  pax_checkcols(
    tbl,
    "year",
    "month",
    "catch",
    expected = "landings"
  )
  fishingyear_cal_start <- 9

  tbl |>
    dplyr::mutate(
      fishing_year = dplyr::case_when(
        # Pre-1991 regulations were different, no fishingyear
        year < 1991 ~ as.character(sql("year::INTEGER")),
        year == 1991 && month < local(fishingyear_cal_start) ~
          as.character(sql("year::INTEGER")),
        month >= local(fishingyear_cal_start) ~
          paste0(sql("year::INTEGER"), '/', sql("year::INTEGER + 1")),
        TRUE ~ # NB: Includes NA months (yearly entries are month ~6)
          paste0(sql("year::INTEGER - 1"), '/', sql("year::INTEGER"))
      )
    )
}

#' @param ignore_final_year Boolean, exclude the final (likely incomplete) year?
#' @return \subsection{pax_landings_fishingyear_summary}{A dplyr query with
#'   columns ``fishing_year`` and ``catch_kt``, ordered by fishing year}
#' @rdname pax_landings
# Was: landings_by_fishing_year.csv
pax_landings_fishingyear_summary <- function(
  tbl,
  ignore_final_year = TRUE
) {
  pax_checkcols(
    tbl,
    "year",
    "month",
    "catch",
    expected = "landings"
  )

  # NSE variables
  month <- NULL
  fishing_year <- NULL
  catch <- NULL
  year <- NULL

  out <- tbl |>
    pax_add_fishing_year() |>
    dplyr::group_by(fishing_year) |>
    dplyr::summarize(
      catch_kt = round(sum(catch, na.rm = TRUE) / 1000)
    )
  if (isTRUE(ignore_final_year)) {
    max_year <- tbl |>
      dplyr::summarize(year = max(year, na.rm = TRUE)) |>
      dplyr::pull(year)
    out <- dplyr::filter(
      out,
      left(fishing_year, 4L) != local(max_year),
      right(fishing_year, 4L) != local(max_year)
    )
  }
  return(out |> dplyr::arrange(fishing_year))
}
