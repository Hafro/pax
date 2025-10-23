#!/usr/bin/env Rscript
# Test converted versions of queries from 02-had
library(unittest)

library(pax)
library(hafropax)

# TODO: 
comment('
# 
echo -n "User (e.g. \'ops$user\'): " ; read MAR_USER ; echo -n "Password: " ; read -s MAR_PASS ; export MAR_USER MAR_PASS ; echo
')
mar <- mar::connect_mar()
pcon <- hafropax::hpax_connect()

ok_group("R/01-plots_and_tables.R:sampling_position", {
  species_code <- 2
  tyr <- lubridate::year(Sys.Date()) - 2
  df_tidypax <- tidypax:::sampling_position(mar,species_nr = species_code, year_range = tyr) |>
    dplyr::arrange(lat, lon, year, mfdb_gear_code) |> as.data.frame()
  df_hafropax <-
    pax_sampling(pcon, species_code, year_range = tyr, include_stomach = TRUE) |>
    pax_sampling_position_summary() |>
    dplyr::arrange(lat, lon, year, mfdb_gear_code) |> as.data.frame()
  ok(ut_cmp_equal(df_tidypax, df_hafropax), "data frames match")
})

ok_group("R/01-plots_and_tables.R:sampling_tables", {
  species_code <- 2
  df_tidypax <- tidypax:::sampling_tables(mar,species_nr = species_code) |>
    dplyr::filter(Year %in% 2010:2020) |>
    dplyr::collect()
  df_hafropax <-
    pax_sampling(pcon, species_code, year_range = 2010:2020) |>
    pax_sampling_detail() |>
    dplyr::collect()
  ok(ut_cmp_equal(df_tidypax, df_hafropax), "data frames match")
})

ok_group("R/01-plots_and_tables.R:catch_agg", {
  species_code <- 2
  df_tidypax <- suppressWarnings(tidypax::catch_data(mar,species_code) |>
    tidypax::add_depth_labels(breaks = c(0,100,200,300)) |>
    tidypax::add_regions(regions = list(W=101,NW = 102, NE=c(103,104,105),
                               SE=c(107,106),
                               SW = 108)) |>
    dplyr::mutate(region = nvl(region, 'Other')) |>
    dplyr::group_by(year,mfdb_gear_code,region,depth_class) |>
    dplyr::summarise(c=sum(catch)/1e6) |>
    dplyr::ungroup() |>
    dplyr::arrange(year, mfdb_gear_code, region, depth_class) |>
    dplyr::collect()
  )
  df_hafropax <- pax_catch(pcon, species_code) |>
    pax_add_depth_labels(breaks = c(0,100,200,300)) |>
    pax_add_regions(regions = list(W=101,NW = 102, NE=c(103,104,105),
                               SE=c(107,106),
                               SW = 108)) |>
    dplyr::mutate(region = nvl(region, 'Other')) |>
    dplyr::group_by(year,mfdb_gear_code,region,depth_class) |>
    dplyr::summarise(c=sum(catch)/1e6) |>
    dplyr::ungroup() |>
    dplyr::arrange(year, mfdb_gear_code, region, depth_class) |>
    dplyr::collect()
  ok(ut_cmp_equal(df_tidypax, df_hafropax), "data frames match")
})

ok_group("assessment_model/00-setup/input_data.R:maturity_key", {
  df_tidypax <- suppressWarnings(tidypax::si_stations(mar) |>
    dplyr::filter(sampling_type == 30) |>
    dplyr::inner_join(mar::les_maelingu(mar) |>
                   dplyr::filter(maeling_teg == 'OTOL',
                          tegund_nr ==local(species_code),
                          !is.na(aldur),
                          !is.na(kynthroski_nr)) |>
                   dplyr::mutate(mat = ifelse(kynthroski_nr==1,0,1)) |>
                   dplyr::rename(sample_id = synis_id,
                          length = lengd, age = aldur)) |>
    tidypax::add_lgroups(lgroups = seq(0,200,5)) |>
    tidypax:::add_regions(regions = list(S=c(101,107,106,108,109,114),
                                         N = c(102,103,104,105,111,113))) |>
    dplyr::mutate(region = nvl(region, 'S')) |>
    dplyr::group_by(year,lgroup,age,region) |>
    dplyr::summarise(p = mean(mat)) |>
    dplyr::arrange(year, region, age, lgroup) |>
    dplyr::collect()
  )
  df_hafropax <- pax_si(pcon) |>
    dplyr::filter(sampling_type == 30) |>
    dplyr::inner_join(
      pax_measurement_agelen(pcon) |>
      dplyr::filter(species == local(species_code)) |>
      dplyr::mutate(mat = ifelse(maturity_stage == 1,0,1))
    ) |>
    pax_add_lgroups(lgroups = seq(0,200,5)) |>
    pax_add_regions(regions = list(S=c(101,107,106,108,109,114),
                                         N = c(102,103,104,105,111,113))) |>
    dplyr::mutate(region = nvl(region, 'S')) |>
    dplyr::group_by(year,lgroup,age,region) |>
    dplyr::summarise(p = mean(mat)) |>
    dplyr::arrange(year, region, age, lgroup) |>
    dplyr::collect()
  ok(ut_cmp_equal(df_tidypax, df_hafropax), "data frames match")
})
