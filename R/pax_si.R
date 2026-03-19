#' Survey index computation functions
#'
#' Functions to compute and scale survey indices of abundance and biomass.
#' The typical workflow is:
#' 1. [pax_si_by_length()] -- join station and length data, scale by tow area
#' 2. [pax_si_scale_by_strata()] -- multiply by stratum area
#' 3. [pax_si_strata_summary()] -- aggregate within strata
#' 4. [pax_si_year_summary()] -- aggregate across strata to annual totals
#'
#' @param tbl A dplyr query, typically from the station table
#' @name pax_si
NULL

#' @param lgroups Numeric vector of length group lower bounds
#' @param regions Named list mapping region names to vectors of division codes
#' @param tgroup Named list mapping temporal group names to vectors of month
#'   integers, or ``NULL`` for a single annual group
#' @param ygroup Named list mapping year group names to vectors of year
#'   integers, or ``NULL`` for one group per year
#' @param gear_group Named list mapping gear group names to vectors of
#'   ``mfdb_gear_code`` values
#' @param alk_tbl A dplyr query with ``agep`` column, as returned by
#'   [pax_ldist_alk()]
#' @return \subsection{pax_si_scale_by_alk}{A dplyr query with ``si_abund``
#'   and ``si_biomass`` scaled by the age-length key proportions, filtered to
#'   positive values}
#' @rdname pax_si
# Was tidypax:si_by_age
pax_si_scale_by_alk <- function(
  tbl,
  lgroups = seq(0, 200, 5),
  regions = list(all = 101:115),
  tgroup = NULL,
  ygroup = NULL,
  gear_group = NULL,
  alk_tbl
) {
  pcon <- dbplyr::remote_con(tbl)

  pax_checkcols(tbl, "si_abund", "si_biomass", expected = "pax_si_by_length()")
  pax_checkcols(alk_tbl, "agep", expected = "pax_ldist_alk()")

  # NSE variables
  agep <- NULL
  si_abund <- NULL
  si_biomass <- NULL

  # NB: This did rename gear -> mfdb_gear_code, moved to pax_si.hafropax()
  tbl |>
    pax_add_groupings(
      groupings = pax_def_groupings(
        regions = regions,
        gear_group = gear_group,
        lgroups = lgroups,
        tgroup = tgroup,
        ygroup = ygroup,
      )
    ) |>
    dplyr::left_join(pax_temptbl(pcon, alk_tbl)) |>
    dplyr::mutate(
      si_abund = coalesce(agep, 0) * si_abund,
      si_biomass = coalesce(agep, 0) * si_biomass
    ) |>
    dplyr::filter(si_biomass > 0, si_abund > 0)
}

#' @param species Integer species code
#' @param landings_tbl A dplyr query from the landings table
#' @param logbook_tbl A dplyr query from the logbook table, used to disaggregate
#'   landings by area when more than one region is defined
#' @return \subsection{pax_si_scale_by_landings}{A dplyr query with
#'   ``si_abund`` and ``si_biomass`` rescaled so that total biomass matches
#'   commercial landings}
#' @rdname pax_si
pax_si_scale_by_landings <- function(
  tbl,
  species,
  landings_tbl = dplyr::tbl(dbplyr::remote_con(tbl), "landings"),
  logbook_tbl = dplyr::tbl(dbplyr::remote_con(tbl), "logbook"),
  regions = list(all = 101:115),
  gear_group = list(
    Other = 'Var',
    BMT = c('BMT', 'NPT', 'SHT', 'PGT'),
    LLN = 'LLN',
    DSE = c('PSE', 'DSE')
  ),
  tgroup = list(t1 = 1:6, t2 = 7:12)
) {
  pcon <- dbplyr::remote_con(tbl)

  # NSE variables
  catch <- NULL
  catch_proportion <- NULL
  gear_name <- NULL
  region <- NULL
  si_abund <- NULL
  si_biomass <- NULL
  year <- NULL

  landings <-
    landings_tbl |>
    pax_add_groupings(
      groupings = pax_def_groupings(
        regions = regions,
        gear_group = gear_group,
        lgroups = NULL,
        tgroup = tgroup,
        ygroup = NULL,
      )
    ) |>
    dplyr::group_by(species, year, tgroup, gear_name) |>
    dplyr::summarise(catch = sum(catch, na.rm = TRUE))

  if (length(regions) > 1) {
    message("Scaling landings by logbook data to get per-area landings")
    catch_by_region <-
      logbook_tbl |>
      pax_add_groupings(
        groupings = pax_def_groupings(
          regions = regions,
          gear_group = gear_group,
          lgroups = NULL,
          tgroup = tgroup,
          ygroup = NULL,
        )
      ) |>
      dplyr::group_by(species, year, tgroup, gear_name, region) |>
      dplyr::summarise(catch = sum(catch, na.rm = TRUE)) |>
      dplyr::group_by(species, year, tgroup, gear_name) |>
      dplyr::mutate(
        catch_proportion = pmax(coalesce(catch, 0), 1) /
          sum(pmax(coalesce(catch, 1), 1))
      ) |>
      dplyr::select(-catch)

    landings <-
      landings |>
      dplyr::left_join(
        catch_by_region,
        by = c("species", "year", "tgroup", "gear_name")
      ) |>
      dplyr::mutate(catch = catch * coalesce(catch_proportion, 1)) |>
      ## this will cause problems down the road.. need to do something more sensible
      dplyr::mutate(region = coalesce(region, 'all'))
  } else {
    # TODO: This isn't quite right?
    landings <- landings |> dplyr::mutate(region = 'all')
  }

  tbl |>
    dplyr::left_join(landings) |>
    dplyr::group_by(species, year, tgroup, gear_name, region) |>
    dplyr::mutate(
      si_abund = si_abund * coalesce(catch, sum(si_biomass)) / sum(si_biomass),
      si_biomass = si_biomass *
        coalesce(catch, sum(si_biomass)) /
        sum(si_biomass)
    )
}

#' @param ldist A dplyr query from the ldist table, pre-processed with
#'   [pax_ldist_add_weight()] to provide a ``weight`` column
#' @return \subsection{pax_si_by_length}{A dplyr query joining station and
#'   length data, with ``si_abund`` (thousands of fish per stratum) and
#'   ``si_biomass`` (tonnes per stratum) columns added}
#' @rdname pax_si
# NB: Removing the species filtering, and assume that the ldist table is pre-filtered
# NB: Creates the si_biomass/si_abund values
pax_si_by_length <- function(
  tbl,
  ldist = dplyr::tbl(dbplyr::remote_con(tbl), "ldist") |> pax_ldist_add_weight()
) {
  pcon <- dbplyr::remote_con(tbl)
  # Choose a species to fill in gaps in data
  def_species = ldist |>
    dplyr::filter(!is.na(species)) |>
    dplyr::pull(species) |>
    dplyr::first()

  # NSE variables
  si_abund <- NULL
  si_biomass <- NULL
  species <- NULL
  count <- NULL
  weight <- NULL

  tbl |>
    ##  2. get length data
    dplyr::left_join(ldist, by = c("sample_id")) |>

    dplyr::mutate(
      species = dplyr::if_else(is.na(species), local(def_species), species),
      length = dplyr::if_else(is.na(length), 0, length), # 0 lengths are 0 counts at stations
      count = dplyr::if_else(is.na(count), 0, count)
    ) |>
    # 3. get count data
    #    Note: If only counted at station this code approach fails
    pax_ldist_scale_tow_area() |>
    dplyr::mutate(si_abund = count / 1e3) |>
    # 7. calculate_biomass from numbers, length and a and b
    # 7.a get the length weight coefficients
    dplyr::mutate(
      si_biomass = ifelse(is.na(si_abund), 0, si_abund) * weight / 1000
    ) |>
    dplyr::mutate(
      si_abund = coalesce(si_abund, 0),
      si_biomass = coalesce(si_biomass, 0)
    ) |>
    dplyr::select(-c(count, weight))
}

#' @param q Quantile threshold above which station biomass values are
#'   winsorized (default 0.95)
#' @return \subsection{pax_si_scale_winsorize}{A dplyr query with extreme
#'   ``si_abund`` and ``si_biomass`` values scaled down to the ``q``
#'   quantile within each year and species}
#' @rdname pax_si
pax_si_scale_winsorize <- function(tbl, q = 0.95) {
  # NSE variables
  si_abund <- NULL
  si_biomass <- NULL
  year <- NULL
  sample_id <- NULL
  species <- NULL
  B <- NULL
  B_quantile <- NULL
  B_scalar <- NULL

  winsor_table_b <-
    tbl |>
    dplyr::filter(si_biomass > 0) |>
    dplyr::group_by(year, sample_id, species) |>
    dplyr::summarise(
      si_biomass = sum(si_biomass, na.rm = TRUE),
      si_abund = sum(si_abund, na.rm = TRUE)
    ) |>
    dplyr::group_by(year, species) |>
    dplyr::mutate(B_quantile = quantile(B, q)) |>
    dplyr::filter(si_biomass > B_quantile) |>
    dplyr::mutate(B_scalar = min(si_biomass) / si_biomass) |>
    dplyr::ungroup() |>
    dplyr::select(sample_id, species, B_scalar)

  tbl |>
    dplyr::left_join(winsor_table_b) |>
    dplyr::mutate(
      si_biomass = coalesce(B_scalar, 1) * si_biomass,
      si_abund = coalesce(B_scalar, 1) * si_abund
    ) |>
    dplyr::select(-B_scalar)
}

#' @param strata_tbl A dplyr query or table name for strata polygons, with
#'   columns ``stratum``, ``h3_cells``, ``rall_area``, and ``geom``
#' @param area_col Name of the column in ``strata_tbl`` containing the
#'   surveyable area in km² (default ``"rall_area"``)
#' @return \subsection{pax_si_scale_by_strata}{A dplyr query with each station
#'   assigned to a stratum, and ``si_abund`` and ``si_biomass`` multiplied by
#'   the stratum area (in square nautical miles) and divided by the number of
#'   stations in that stratum}
#' @rdname pax_si
# Was tidypax:si_add_strata
pax_si_scale_by_strata <- function(
  tbl,
  strata_tbl,
  area_col = "rall_area"
) {
  pcon <- dbplyr::remote_con(tbl)
  # TODO: This should just be pax_temptbl()
  if (is.character(strata_tbl)) {
    strata_tbl <- dplyr::tbl(pcon, strata_tbl)
  }
  tbl_colnames <- colnames(tbl)
  strata_tbl_colnames <- colnames(strata_tbl)

  # NSE variables
  area <- NULL
  geom <- NULL
  gridcell <- NULL
  h3_cells <- NULL
  rall_area <- NULL
  sample_id <- NULL
  sampling_type <- NULL
  si_abund <- NULL
  si_biomass <- NULL
  species <- NULL
  station <- NULL
  stratum <- NULL
  tow_depth <- NULL
  year <- NULL

  if ("h3_cells" %in% tbl_colnames && "h3_cells" %in% strata_tbl_colnames) {
    out <- tbl |>
      # First join to a de-duplicated map of h3_cell -> stratum ID
      # TODO: This is assuming the "first" h3_cell of the station is what to join against, not using stationlist, midpoint, etc.
      dplyr::mutate(h3_cell = list_first(h3_cells)) |>
      dplyr::left_join(
        strata_tbl |>
          dplyr::group_by(h3_cell = dplyr::sql("UNNEST(h3_cells)")) |>
          dplyr::summarize(stratum = min(stratum, na.rm = TRUE)),
        by = c("h3_cell")
      )
  } else {
    stop(
      "Cannot join tables, expected columns missing (tbl: ",
      paste(tbl_colnames, collapse = ","),
      ", strata_tbl: ",
      paste(tbl_colnames, collapse = ","),
      ")"
    )
  }

  # ..then join again to pull in metadata from strata_tbl
  # TODO: Instead of area_col ideally we...
  #     * Calculate area from stratum shape
  #     * Use depth cut-off instead of rall_area
  out |>
    dplyr::left_join(
      strata_tbl |>
        dplyr::mutate(
          # Convert km^2 (reitmapping units) to square nautical miles (tow area units)
          area = coalesce(!!as.symbol(area_col), 0) / 1.852^2
        ) |>
        dplyr::select(-geom, -h3_cells, -rall_area),
      by = c("stratum")
    ) |>
    dplyr::group_by(
      sample_id,
      station,
      gridcell,
      species,
      year,
      length,
      tow_depth,
      stratum,
      sampling_type,
      area
    ) |>
    dplyr::summarize(
      si_abund = sum(si_abund, na.rm = TRUE),
      si_biomass = sum(si_biomass, na.rm = TRUE)
    ) |>
    dplyr::group_by(species, year, stratum, sampling_type, area) |>
    dplyr::mutate(
      # NB: Not summarise, i.e. window function
      si_abund = area * si_abund / dplyr::n_distinct(sample_id, na.rm = TRUE),
      si_biomass = area *
        si_biomass /
        dplyr::n_distinct(sample_id, na.rm = TRUE)
    )
}

#' @param length_range Numeric vector of length 2, only fish within this length
#'   range (exclusive) contribute to the summary
#' @param std.cv Coefficient of variation used as a floor for the standard
#'   deviation when all station values within a stratum are identical
#' @return \subsection{pax_si_strata_summary}{A dplyr query aggregated by
#'   species, year, stratum, sampling type, and area, with columns
#'   ``si_N``, ``si_abund``, ``si_abund_sd``, ``si_biomass``, and
#'   ``si_biomass_sd``}
#' @rdname pax_si
# Was tidypax::si_by_strata
pax_si_strata_summary <- function(
  tbl,
  length_range = c(5, 500),
  std.cv = 0.2
) {
  pax_checkcols(
    tbl,
    "sample_id",
    "station",
    "species",
    "year",
    "stratum",
    "sampling_type",
    "area",
    "si_abund",
    "si_biomass",
    expected = "pax_si_by_length()"
  )

  # NSE variables
  area <- NULL
  sample_id <- NULL
  station <- NULL
  species <- NULL
  year <- NULL
  stratum <- NULL
  sampling_type <- NULL
  si_abund <- NULL
  si_abund_sd <- NULL
  si_biomass <- NULL
  si_biomass_sd <- NULL
  si_N <- NULL

  tbl |>
    dplyr::mutate(
      si_biomass = ifelse(
        length < local(max(length_range)) & length > local(min(length_range)),
        si_biomass,
        0
      ),
      si_abund = ifelse(
        length < local(max(length_range)) & length > local(min(length_range)),
        si_abund,
        0
      )
    ) |>
    dplyr::group_by(
      sample_id,
      station,
      species,
      year,
      stratum,
      sampling_type,
      area
    ) |>
    dplyr::summarise(
      si_abund = sum(si_abund, na.rm = TRUE),
      si_biomass = sum(si_biomass, na.rm = TRUE)
    ) |>
    dplyr::group_by(
      species,
      year,
      stratum,
      sampling_type,
      area
    ) |>
    dplyr::summarise(
      si_N = dplyr::n(),
      si_abund = sum(si_abund, na.rm = TRUE), # number of fish
      si_abund_sd = sd(si_abund, na.rm = TRUE),
      si_biomass = sum(si_biomass, na.rm = TRUE), # biomass of fish
      si_biomass_sd = sd(si_biomass, na.rm = TRUE)
    ) |>
    dplyr::mutate(
      si_abund_sd = ifelse(
        si_abund_sd == 0,
        si_abund * local(std.cv),
        si_N * si_abund_sd
      ),
      si_biomass_sd = ifelse(
        si_biomass_sd == 0,
        si_biomass * local(std.cv),
        si_N * si_biomass_sd
      )
    )
}

#' @return \subsection{pax_si_year_summary}{A dplyr query aggregated by
#'   species, sampling type, and year, with columns ``si_N``, ``si_abund``,
#'   ``si_abund_cv``, ``si_biomass``, and ``si_biomass_cv``}
#' @rdname pax_si
# Was tidypax::si_by_year
pax_si_year_summary <- function(tbl) {
  pax_checkcols(
    tbl,
    "si_N",
    "si_abund",
    "si_abund_sd",
    "si_biomass",
    "si_biomass_sd",
    expected = "pax_si_by_length()"
  )

  # NSE variables
  species <- NULL
  sampling_type <- NULL
  year <- NULL
  n <- NULL
  si_abund <- NULL
  si_biomass <- NULL

  # Generate code to calculate CV for both si_abund & si_biomass
  tmp <- sapply(
    c('si_abund_cv', 'si_biomass_cv'),
    function(var_name) {
      substitute(
        sqrt(sum(
          coalesce(var_sd, 0)^2 *
            if_else(is.na(var_sd), 0, area)^2 /
            if_else(is.na(var_sd), 1, si_N),
          na.rm = TRUE
        )) /
          sum(if_else(is.na(var_sd), 1, area), na.rm = TRUE) *
          sum(area, na.rm = TRUE) /
          sum(
            if_else(is.na(var_sd), 1, var) * if_else(is.na(var_sd), 1, area),
            na.rm = TRUE
          ),
        list(
          var_sd = as.symbol(gsub("_cv$", "_sd", var_name)),
          var = as.symbol(gsub("_cv$", "", var_name)),
          end = NULL
        )
      )
    },
    simplify = FALSE
  )

  tbl |>
    dplyr::filter(si_abund > 0) |> ## remove strata with no fish to avoid division by zero
    dplyr::group_by(species, sampling_type, year) |>
    dplyr::summarise(
      si_N = dplyr::n(), # strata within a year
      si_abund = sum(si_abund, na.rm = TRUE),
      !!!tmp[1],
      si_biomass = sum(si_biomass, na.rm = TRUE),
      !!!tmp[2]
    ) |>
    dplyr::ungroup()
}
