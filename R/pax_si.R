pax_si <- function(pcon) UseMethod("pax_si")

# NB: Switching on the embedded pax object, not the table
pax_si_by_age <- function(
  tbl,
  species = 1,
  lgroups = seq(0, 200, 5),
  regions = list(all = 101:115),
  tgroup = NULL,
  ygroup = NULL,
  gear_group = NULL,
  pre_scaling = pax_add_strata, # TODO: Can we just tell people to call pax_add_strata?
  post_scaling = function(x, ...) x,
  alk,
  ...
) {
  UseMethod("pax_si_by_age", as_pax(tbl))
}
pax_si_by_length <- function(
  tbl,
  species = 1:100,
  ldist = pax_ldist(as_pax(tbl) |> pax_ldist_add_weight())
) {
  UseMethod("pax_si_by_length", as_pax(tbl))
}
pax_si_winsorize <- function(tbl, q = 0.95) {
  UseMethod("pax_si_winsorize", as_pax(tbl))
}
pax_si_by_strata <- function(tbl, length_range = c(5, 500), std.cv = 0.2) {
  UseMethod("pax_si_by_strata", as_pax(tbl))
}
pax_si_by_year <- function(tbl) UseMethod("pax_si_by_age", as_pax(tbl))
pax_si_fix_faroe_ridge <- function(tbl, action = 'remove') {
  UseMethod("pax_si_fix_faroe_ridge", as_pax(tbl))
}

pax_si_by_age.pax <- function(
  tbl,
  species = 1,
  lgroups = seq(0, 200, 5),
  regions = list(all = 101:115),
  tgroup = NULL,
  ygroup = NULL,
  gear_group = NULL,
  pre_scaling = pax_add_strata, # TODO: Can we just tell people to call pax_add_strata?
  post_scaling = function(x, ...) x,
  alk,
  ...
) {
  # NB: This did rename gear -> mfdb_gear_code, moved to pax_si.hafropax()
  tbl |>
    pre_scaling() |>
    pax_add_regions(regions) |>
    pax_add_gear_group(gear_group) |>
    pax_add_lgroups(lgroups) |>
    pax_add_temporal_grouping(tgroup) |>
    pax_add_yearly_grouping(ygroup) |>
    dplyr::left_join(alk) |>
    dplyr::mutate(adj_N = nvl(agep, 0) * N, adj_B = nvl(agep, 0) * B) |>
    dplyr::filter(adj_B > 0, N > 0) |>
    post_scaling(
      regions = regions,
      gear_group = gear_group,
      species = species,
      tgroup = tgroup,
      ...
    )
}

# TODO: Generic?
pax_si_make_alk <- function(
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
  aldist = pax_aldist(as_pax(tbl))
) {
  # NB: This did rename gear -> mfdb_gear_code, moved to pax_si.hafropax()
  # TODO: Validate it's the right kind of tbl?
  tbl |>
    pax_add_gear_group(gear_group) |>
    pax_add_regions(regions = regions) |>
    pax_add_temporal_grouping(tgroup) |>
    pax_add_yearly_grouping(ygroup) |>
    dplyr::left_join(aldist, by = c('sample_id')) |>
    pax_add_lgroups(lgroups = lgroups) |>
    dplyr::mutate(count = nvl2(age, 1, 0), region = nvl(region, 'all')) |>
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

pax_si_scale_by_landings <- function(
  tbl,
  species,
  regions = list(all = 101:115),
  ices_area = '5a%',
  gear_group = list(
    Other = 'Var',
    BMT = c('BMT', 'NPT', 'SHT', 'PGT'),
    LLN = 'LLN',
    DSE = c('PSE', 'DSE')
  ),
  tgroup = list(t1 = 1:6, t2 = 7:12)
) {
  area_filter <-
    sprintf('ices_area %%like%% \'%s\'', ices_area) |>
    paste(collapse = '|')

  landings <-
    pax_landings(as_pax(tbl)) |>
    dplyr::filter(!!rlang::parse_expr(area_filter)) |>
    ## assume unknown months are all in month 6
    dplyr::mutate(month = nvl(month, 6)) |>
    ## assume landings from unknown gears are from bottom trawls
    dplyr::mutate(mfdb_gear_code = nvl(mfdb_gear_code, 'BMT')) |>

    # TODO: Safer to clone the functions, or have a generic pax_add?
    pax_add_gear_group(gear_group) |>
    pax_add_temporal_grouping(tgroup) |>
    dplyr::group_by(species, year, tgroup, gear_name) |>
    dplyr::summarise(catch = sum(catch))

  if (length(regions) > 1) {
    catch_by_region <- # TODO: Was a whole separate function, needed?
      pax_catch(as_pax(tbl)) |>
      dplyr::filter(species %in% local(species)) |>
      pax_add_regions(regions) |>
      pax_add_gear_group(gear_group) |>
      pax_add_temporal_grouping(tgroup) |>
      dplyr::group_by(species, year, tgroup, gear_name, region) |>
      dplyr::summarise(catch = sum(catch)) |>
      dplyr::group_by(species, year, tgroup, gear_name) |>
      dplyr::mutate(
        catch_proportion = pmax(nvl(catch, 0), 1) / sum(pmax(nvl(catch, 1), 1))
      ) |>
      dplyr::select(-catch)

    landings <-
      landings |>
      dplyr::left_join(
        catch_by_region,
        by = c("species", "year", "tgroup", "gear_name")
      ) |>
      dplyr::mutate(catch = catch * nvl(catch_proportion, 1))
  } else {
    landings <- landings |> dplyr::mutate(region = 'all')
  }

  tbl |>
    dplyr::mutate(region = nvl(region, 'all')) |> ## this will cause problems down the road.. need to do something more sensible
    dplyr::left_join(landings) |>
    dplyr::group_by(species, year, tgroup, gear_name, region) |>
    dplyr::mutate(
      adj_N = adj_N * nvl(catch, sum(adj_B)) / sum(adj_B),
      adj_B = adj_B * nvl(catch, sum(adj_B)) / sum(adj_B)
    )
}

pax_si_by_length.pax <- function(
  tbl,
  species = 1:100,
  ldist = pax_ldist(as_pax(tbl) |> pax_ldist_add_weight())
) {
  # TODO: this should be a helper function
  species_tbl <- tibble::tibble(species = species, dummy = 1)

  tbl |>
    ##  2. get length data
    dplyr::mutate(dummy = 1) |>
    dplyr::left_join(
      pax_temptbl(as_pax(tbl), species_tbl),
      by = 'dummy'
    ) |>
    dplyr::select(-dummy) |>
    dplyr::left_join(ldist, by = c("sample_id", "species")) |>

    dplyr::mutate(
      length = dplyr::if_else(is.na(length), 0, length), # 0 lengths are 0 counts at stations
      count = dplyr::if_else(is.na(count), 0, count)
    ) |>
    # 3. get count data
    #    Note: If only counted at station this code approach fails
    pax_ldist_scale_tow_area() |>
    dplyr::mutate(N = count / 1e3) |>
    # 7. calculate_biomass from numbers, length and a and b
    # 7.a get the length weight coefficients
    dplyr::mutate(B = ifelse(is.na(N), 0, N) * weight / 1000) |>
    dplyr::mutate(N = nvl(N, 0), B = nvl(B, 0)) |>
    dplyr::select(-c(count, weight))
}

pax_si_winsorize.pax <- function(tbl, q = 0.95) {
  winsor_table_b <-
    tbl |>
    dplyr::filter(B > 0) |>
    dplyr::group_by(year, sample_id, species) |>
    dplyr::summarise(B = sum(B, na.rm = TRUE), N = sum(N, na.rm = TRUE)) |>
    dplyr::group_by(year, species) |>
    dplyr::mutate(B_quantile = quantile(B, q)) |>
    dplyr::filter(B > B_quantile) |>
    dplyr::mutate(B_scalar = min(B) / B) |>
    dplyr::ungroup() |>
    dplyr::select(sample_id, species, B_scalar)

  tbl |>
    dplyr::left_join(winsor_table_b) |>
    dplyr::mutate(B = nvl(B_scalar, 1) * B, N = nvl(B_scalar, 1) * N) |>
    dplyr::select(-B_scalar)
}

pax_si_by_strata.pax <- function(tbl, length_range = c(5, 500), std.cv = 0.2) {
  # TODO: Check that columns exist
  tbl |>
    dplyr::mutate(
      B = ifelse(
        length < local(max(length_range)) & length > local(min(length_range)),
        B,
        0
      ),
      N = ifelse(
        length < local(max(length_range)) & length > local(min(length_range)),
        N,
        0
      )
    ) |>
    dplyr::group_by(
      sample_id,
      station,
      species,
      year,
      stratification,
      stratum,
      sampling_type,
      area
    ) |>
    dplyr::summarise(N = sum(N, na.rm = TRUE), B = sum(B, na.rm = TRUE)) |>
    dplyr::group_by(
      species,
      year,
      stratification,
      stratum,
      sampling_type,
      area
    ) |>
    dplyr::summarise(
      s_N = n(),
      n_m = sum(N, na.rm = TRUE), # number of fish
      n_sd = sd(N),
      b_m = sum(B, na.rm = TRUE), # biomass of fish
      b_sd = sd(B)
    ) |>
    dplyr::mutate(
      n_sd = ifelse(n_sd == 0, n_m * local(std.cv), s_N * n_sd),
      b_sd = ifelse(b_sd == 0, b_m * local(std.cv), s_N * b_sd)
    )
}

pax_si_by_year.pax <- function(tbl) {
  # TODO: nvl2 is an oracle-ism, this needs fixing
  ff <- "sqrt(sum(nvl(%1$s_sd, 0)^2 * nvl2(%1$s_sd, area,0)^2/nvl2(%1$s_sd, s_N,1)))/sum(nvl2(%1$s_sd, area,1))* sum(area)/sum(nvl2(%1$s_sd, %1$s_m,1) * nvl2(%1$s_sd, area,1))"

  tmp <-
    sprintf(ff, c('n', 'b')) |>
    stats::setNames(c('n_cv', 'b_cv')) |>
    purrr::map(rlang::parse_expr)

  # TODO: Check that columns exist
  tbl |>
    dplyr::filter(n_m > 0) |> ## remove strata with no fish to avoid division by zero
    dplyr::group_by(stratification, species, sampling_type, year) |>
    dplyr::summarise(
      srN = n(), # strata within a year
      n = sum(n_m, na.rm = TRUE),
      !!!tmp[1],
      b = sum(b_m, na.rm = TRUE),
      !!!tmp[2]
    ) |>
    dplyr::ungroup()
}

#' @describeIn si_stations Adjusts indices for the exclusion of the Faroe ridge (placeholder)
#' @return adjusted query
#' @export
pax_si_fix_faroe_ridge.pax <- function(tbl, action = 'remove') {
  if (action == 'remove') {
    tbl |>
      dplyr::filter(
        !(stratum %in% c(1, 4, 93, 3, 92)) &
          stratification == 'old_strata' |
          !(stratum %in% c(4, 7, 44, 43)) & stratification == 'new_strata'
      )
  } else {
    stop(sprintf('Action %s not implemented', action))
  }
}

# Was: tidypax::survey_locations
pax_si_summary_locations <- function(
  pcon,
  ldist = pax_ldist(pcon) |>
    pax_ldist_scale_abund() |>
    pax_ldist_add_weight()
) {
  species_dummies <-
    tibble::tibble(species = 1:100, dummy = 1)

  pax_si(pcon) |>
    dplyr::left_join(ldist, by = c("sample_id", "species")) |>
    dplyr::mutate(dummy = 1) |>
    dplyr::left_join(
      pax_temptbl(pcon, species_dummies),
      by = c('species', 'dummy')
    ) |>
    dplyr::group_by(
      sample_id,
      begin_lat,
      begin_lon,
      year,
      sampling_type,
      species
    ) |>
    dplyr::summarise(
      bio = sum(
        abs(nvl(count, 0) * nvl(a, 0.01) * abs(nvl(length, 0))^nvl(b, 3)) /
          abs(nvl(pmax(tow_length, 0.1), 4)),
        na.rm = TRUE
      ) /
        1e3
    ) |>
    dplyr::mutate(zero_station = ifelse(bio == 0, 'Zero catch', 'Non zero')) |>
    dplyr::ungroup()
}

# TODO: alk_prep / calc_alk ? Where do they go?
