# Routines used to extract the lookup tables in data/ from mar. Run from the
# directory above the pax checkout, as the other data_update_* functions:
#   mar <- mar::connect_mar()
#   pax:::data_update_reitmapping(mar)
#   pax:::data_update_gear_mapping(mar)

data_update_reitmapping <- function(mar, path = "pax/data/reitmapping.rda") {
  if (!requireNamespace("mar", quietly = TRUE)) {
    stop("mar package not available, cannot import from DB")
  }

  # NSE variables
  id <- gridcell <- division <- subdivision <- lat <- lon <- size <- NULL

  out <- mar::tbl_mar(mar, 'ops$bthe."reitmapping"') |>
    dplyr::collect() |>
    dplyr::transmute(
      id = as.integer(id),
      gridcell = as.integer(gridcell),
      division = as.integer(division),
      subdivision = as.integer(subdivision),
      lat,
      lon,
      size
    ) |>
    dplyr::arrange(id) |>
    as.data.frame()
  # NB: .rda, as write.table() keeps only 15 significant digits
  reitmapping <- out
  save(reitmapping, file = path, compress = "xz")
}

data_update_gear_mapping <- function(mar, path = "pax/data/gear_mapping.txt") {
  if (!requireNamespace("mar", quietly = TRUE)) {
    stop("mar package not available, cannot import from DB")
  }

  # NSE variables
  veidarfaeri <- gear <- gear_id <- mfdb_gear_code <- source <- NULL
  mfdb_gear_code_weight <- NULL

  biota <- mar::tbl_mar(mar, 'biota.gear_mapping') |>
    dplyr::collect() |>
    dplyr::transmute(
      gear_id = as.integer(veidarfaeri),
      mfdb_gear_code = gear,
      source = "biota.gear_mapping"
    )
  weight <- mar::tbl_mar(mar, 'ops$bthe."gear_mapping"') |>
    dplyr::collect() |>
    dplyr::transmute(
      gear_id = as.integer(veidarfaeri),
      mfdb_gear_code_weight = gear
    )

  out <- biota |>
    # Gear 91 (anglerfish gillnet) is missing from biota.gear_mapping
    dplyr::bind_rows(
      data.frame(gear_id = 91L, mfdb_gear_code = "GIL", source = "pax")
    ) |>
    dplyr::filter(!duplicated(gear_id)) |>
    dplyr::full_join(weight, by = "gear_id") |>
    dplyr::arrange(gear_id) |>
    as.data.frame()
  stopifnot(!anyNA(out$mfdb_gear_code))
  utils::write.table(out, file = path)
}

data_update_noaa_bathymetry <- function(
  mar,
  path = "pax/data/noaa_bathymetry.rda"
) {
  if (!requireNamespace("mar", quietly = TRUE)) {
    stop("mar package not available, cannot import from DB")
  }

  noaa_bathymetry <- mar::tbl_mar(mar, 'ops$bthe."noaa_bathymetry"') |>
    dplyr::collect() |>
    as.data.frame()
  noaa_bathymetry$reitur <- as.integer(noaa_bathymetry$reitur)
  noaa_bathymetry$smareitur <- as.integer(noaa_bathymetry$smareitur)
  save(noaa_bathymetry, file = path, compress = "xz")
}

#' Fill missing MFDB gear codes from the gear mapping
#'
#' Gives rows with a missing ``mfdb_gear_code`` the MFDB gear code of their
#' gear (``gear_id``) in the [gear_mapping] dataset, e.g. gear 91
#' (anglerfish gillnet), which ``biota.gear_mapping`` leaves unmapped, as
#' ``GIL``. Other rows are unchanged.
#'
#' @param tbl A dplyr query with ``gear_id`` and ``mfdb_gear_code`` columns,
#'   e.g. the pax ``station`` or ``landings`` table
#' @param gear_mapping_tbl The gear mapping, by default the [gear_mapping]
#'   dataset
#' @return ``tbl`` with ``mfdb_gear_code`` filled where it was missing
pax_fill_mfdb_gear_code <- function(
  tbl,
  gear_mapping_tbl = pax_temptbl(dbplyr::remote_con(tbl), "paxdat_gear_mapping")
) {
  # NSE variables
  gear_id <- mfdb_gear_code <- mfdb_gear_code_map <- NULL

  tbl_colnames <- colnames(tbl)
  tbl |>
    dplyr::left_join(
      gear_mapping_tbl |>
        dplyr::select(gear_id, mfdb_gear_code_map = mfdb_gear_code),
      by = "gear_id"
    ) |>
    dplyr::mutate(
      mfdb_gear_code = dplyr::coalesce(
        mfdb_gear_code,
        as.character(mfdb_gear_code_map)
      )
    ) |>
    dplyr::select(dplyr::all_of(tbl_colnames))
}

data_update_diel_correction_reg <- function(
  mar,
  path = "pax/data/diel_correction_reg.rda"
) {
  if (!requireNamespace("mar", quietly = TRUE)) {
    stop("mar package not available, cannot import from DB")
  }

  out <- lapply(c('ops$krik."predressmb_05"', 'ops$krik."predressmh_05"'), function(t) {
    x <- mar::tbl_mar(mar, t) |> dplyr::collect() |> as.data.frame()
    data.frame(
      fleet = x$fleet,
      year = as.integer(x$ar),
      time = x$kl.kastad,
      length_group = as.integer(x$lgnr),
      lengths = x$letxt,
      mult = x$mult,
      meanmult = x$meanmult,
      scaledmult = x$scaledmult,
      stringsAsFactors = FALSE
    )
  })
  out <- do.call(rbind, out)
  out <- out[order(out$fleet, out$time, out$length_group), ]
  rownames(out) <- NULL
  diel_correction_reg <- out
  save(diel_correction_reg, file = path, compress = "xz")
}

# AGF (agf.aflagrunnur, the landings register since September 2007) codes
# against the codes of the old landings register (kvoti.lods_oslaegt), from
# the landings found in both (same vessel, day, port, species and weight).
# The landing base parquet (mar::ln_*) keeps the AGF codes after August 2007,
# where the old register had its own: pax maps them back so that the
# fishing_area and stock_code columns keep one coding over all years. Needs
# the Oracle tables, so run it while they are there:
#   mar <- mar::connect_mar()
#   pax:::data_update_agf_codes(mar)
data_update_agf_codes <- function(mar, path = "pax/data") {
  if (!requireNamespace("mar", quietly = TRUE)) {
    stop("mar package not available, cannot import from DB")
  }

  # NSE variables
  agf_area <- lods_area <- w <- n <- veidistofn <- kfteg <- NULL
  fisktegund <- veidistofn_heiti <- NULL

  m <- DBI::dbGetQuery(
    mar,
    "select a.veidisvaedi agf_area, l.veidisvaedi lods_area, a.veidistofn,
       l.kfteg, count(*) n, sum(a.magn_oslaegt) w
     from agf.aflagrunnur_v a join kvoti.lods_oslaegt l
       on l.skip_nr = a.skip_numer and trunc(l.l_dags) = trunc(a.londun_hefst)
       and l.hofn = a.hafnarnumer and l.fteg = a.fisktegund
       and abs(l.magn_oslaegt - a.magn_oslaegt) < 0.5
     where a.londun_hefst >= to_date('2007-09-01', 'YYYY-MM-DD')
     group by a.veidisvaedi, l.veidisvaedi, a.veidistofn, l.kfteg"
  )
  names(m) <- tolower(names(m))
  # The code of the most landed weight (most landings where no weight)
  dominant <- function(x, key, value) {
    x |>
      dplyr::group_by(dplyr::across(dplyr::all_of(c(key, value)))) |>
      dplyr::summarise(w = sum(w), n = sum(n), .groups = "drop") |>
      dplyr::group_by(dplyr::across(dplyr::all_of(key))) |>
      dplyr::arrange(dplyr::desc(w), dplyr::desc(n), .by_group = TRUE) |>
      dplyr::mutate(share = ifelse(sum(w) > 0, w / sum(w), n / sum(n))) |>
      dplyr::slice(1) |>
      dplyr::ungroup()
  }

  agf_fishing_area <- m |>
    dominant("agf_area", "lods_area") |>
    dplyr::transmute(
      agf_area = as.character(agf_area),
      fishing_area = as.character(lods_area),
      share = round(share, 4)
    ) |>
    as.data.frame()

  stocks <- DBI::dbGetQuery(
    mar,
    "select distinct veidistofn, veidistofn_heiti, fisktegund
     from agf.aflagrunnur_v"
  )
  names(stocks) <- tolower(names(stocks))
  # The register's stock code of each AGF stock, leaving out the register's
  # "no stock" code 0
  kfteg_of <- m |>
    dplyr::filter(kfteg != 0) |>
    dominant("veidistofn", "kfteg") |>
    dplyr::select(veidistofn, kfteg, share)
  agf_stock <- stocks |>
    dplyr::group_by(veidistofn) |>
    # One name and species per stock (the first species code)
    dplyr::arrange(fisktegund, .by_group = TRUE) |>
    dplyr::slice(1) |>
    dplyr::ungroup() |>
    dplyr::left_join(kfteg_of, by = "veidistofn") |>
    dplyr::transmute(
      stock = as.integer(veidistofn),
      stock_name = veidistofn_heiti,
      species = as.integer(fisktegund),
      stock_code = as.integer(kfteg),
      share = round(share, 4)
    ) |>
    dplyr::arrange(stock) |>
    as.data.frame()

  # NB: .rda, as the names are not ASCII
  save(
    agf_fishing_area,
    file = file.path(path, "agf_fishing_area.rda"),
    compress = "xz"
  )
  save(agf_stock, file = file.path(path, "agf_stock.rda"), compress = "xz")
  invisible(list(agf_fishing_area = agf_fishing_area, agf_stock = agf_stock))
}

# The old landings gear codes (veidarfaeri of kvoti.lods_oslaegt and of the
# Fisheries Association's tables, the codes of biota.gear_mapping) of the
# harmonised gear codes of the mar landings parquet (gear_no, the AGF codes,
# with the original gear in gear_name before September 2007), from the
# landings found in both. One row per
# ``source`` ("vigtarskra", "fiskifelag", "lods", "agf"), ``gear_no`` and
# ``gear_name`` (NA for "agf", whose names are the AGF ones) and, for "agf"
# gears whose code depends on the species, ``species``, with the old
# code ``gear_id`` of the most landed weight and its ``share``. Writes
# data/landings_gear.rda. Needs the
# Oracle tables and the parquet, so run it while both are there (.rda, as the
# names are not ASCII):
#   mar <- mar::connect_mar()
#   pax:::data_update_landings_gear(mar)
data_update_landings_gear <- function(mar, path = "pax/data/landings_gear.rda") {
  mar_require_parquet()

  # NSE variables
  source <- gear_no <- gear_name <- gear_id <- w <- n <- l_id <- g_id <- NULL
  weight <- l_source <- l_source_id <- hofn <- komunr <- veidarf <- NULL
  registration_no <- port <- species_no <- year <- month <- NULL

  dominant <- function(x, by = c("source", "gear_no", "gear_name")) {
    x |>
      dplyr::filter(!is.na(gear_id)) |>
      dplyr::group_by(dplyr::across(dplyr::all_of(c(by, "gear_id")))) |>
      dplyr::summarise(w = sum(w), n = sum(n), .groups = "drop") |>
      dplyr::group_by(dplyr::across(dplyr::all_of(by))) |>
      dplyr::arrange(dplyr::desc(w), dplyr::desc(n), .by_group = TRUE) |>
      dplyr::mutate(share = ifelse(sum(w) > 0, w / sum(w), n / sum(n))) |>
      dplyr::slice(1) |>
      dplyr::ungroup() |>
      dplyr::select(dplyr::all_of(c(by, "gear_id", "share")))
  }

  # 1966-1993: the aggregated landings against the Oracle tables, summed by
  # year, month, vessel, port, species and gear, matched on the weight
  agg <- mar_with_parquet(mar::connect_landings_agg, function(con) {
    mar::ln_agg(con) |>
      dplyr::filter(source %in% c(
        "fiskifelagid.vigtarskra66_81",
        "fiskifelagid.landed_catch_pre94"
      )) |>
      dplyr::group_by(
        source, year, month, registration_no, port, species_no, gear_no,
        gear_name
      ) |>
      dplyr::summarise(w = sum(weight, na.rm = TRUE), .groups = "drop") |>
      dplyr::collect()
  })
  fix <- function(x) {
    names(x) <- tolower(names(x))
    dplyr::mutate(x, dplyr::across(-"source", function(v) suppressWarnings(as.numeric(v))))
  }
  ora <- dplyr::bind_rows(
    fix(DBI::dbGetQuery(
      mar,
      "select 'fiskifelagid.vigtarskra66_81' source, artal year,
         manudur month, skip_nr registration_no, vinnsluhofn port,
         fteg species_no, veidarfaeri gear_id, sum(magn) w
       from fiskifelagid.vigtarskra66_81
       group by artal, manudur, skip_nr, vinnsluhofn, fteg, veidarfaeri"
    )),
    fix(DBI::dbGetQuery(
      mar,
      "select 'fiskifelagid.landed_catch_pre94' source, ar year,
         man month, to_number(skip_nr) registration_no, hofn port,
         fteg species_no, veidarfaeri gear_id, sum(magn_oslaegt) w
       from fiskifelagid.landed_catch_pre94
       group by ar, man, skip_nr, hofn, fteg, veidarfaeri"
    ))
  )
  key <- function(x) {
    dplyr::mutate(
      x,
      dplyr::across(
        c("year", "month", "registration_no", "port", "species_no"),
        as.numeric
      ),
      wr = round(as.numeric(w))
    ) |>
      dplyr::filter(!is.na(wr), wr > 0)
  }
  agg_map <- key(agg) |>
    dplyr::inner_join(
      key(ora) |>
        dplyr::select(-w) |>
        dplyr::mutate(gear_id = as.numeric(gear_id)),
      by = c("source", "year", "month", "registration_no", "port",
        "species_no", "wr"),
      relationship = "many-to-many",
      na_matches = "never"
    ) |>
    dplyr::mutate(
      n = 1,
      source = ifelse(
        source == "fiskifelagid.vigtarskra66_81",
        "vigtarskra",
        "fiskifelag"
      )
    ) |>
    dominant()

  # September 1992 - August 2007: the landing base against
  # kvoti.lods_slaegt, by the landing (hofn and komunr)
  lb <- mar_with_parquet(mar::connect_landings, function(con) {
    mar::ln_gear(con) |>
      dplyr::select(g_id, l_id, gear_no, gear_name) |>
      dplyr::inner_join(
        mar::ln_landings(con) |>
          dplyr::filter(source == "kvoti.lods_slaegt.hofn_komunr") |>
          dplyr::select(l_id, l_source_id = source_id),
        by = "l_id"
      ) |>
      dplyr::inner_join(
        mar::ln_catch(con) |>
          dplyr::group_by(g_id) |>
          dplyr::summarise(w = sum(weight, na.rm = TRUE), .groups = "drop"),
        by = "g_id"
      ) |>
      dplyr::collect()
  })
  lods <- mar::tbl_mar(mar, "kvoti.lods_slaegt") |>
    dplyr::distinct(hofn, komunr, veidarf) |>
    dplyr::collect()
  id <- as.character(lb$l_source_id)
  lods_map <- lb |>
    dplyr::mutate(
      hofn = as.numeric(substr(id, 1, nchar(id) - 8)),
      komunr = as.numeric(substr(id, nchar(id) - 7, nchar(id)))
    ) |>
    dplyr::inner_join(
      dplyr::mutate(lods, hofn = as.numeric(hofn), komunr = as.numeric(komunr)),
      by = c("hofn", "komunr"),
      relationship = "many-to-many"
    ) |>
    dplyr::transmute(
      source = "lods",
      gear_no,
      gear_name,
      gear_id = as.numeric(veidarf),
      w,
      n = 1
    ) |>
    dominant()

  # From September 2007: AGF against kvoti.lods_oslaegt, as
  # data_update_agf_codes()
  agf <- DBI::dbGetQuery(
    mar,
    "select a.veidarfaeri gear_no, a.fisktegund species, l.veidarf gear_id,
       count(*) n, sum(a.magn_oslaegt) w
     from agf.aflagrunnur_v a join kvoti.lods_oslaegt l
       on l.skip_nr = a.skip_numer and trunc(l.l_dags) = trunc(a.londun_hefst)
       and l.hofn = a.hafnarnumer and l.fteg = a.fisktegund
       and abs(l.magn_oslaegt - a.magn_oslaegt) < 0.5
     where a.londun_hefst >= to_date('2007-09-01', 'YYYY-MM-DD')
     group by a.veidarfaeri, a.fisktegund, l.veidarf"
  )
  names(agf) <- tolower(names(agf))
  agf <- agf |>
    dplyr::transmute(
      source = "agf",
      gear_no = as.numeric(gear_no),
      gear_name = NA_character_,
      species = as.numeric(species),
      gear_id = as.numeric(gear_id),
      w = as.numeric(w),
      n = as.numeric(n)
    )
  # By gear, and by gear and species where the species' code differs (e.g.
  # the AGF dredge 15 is the sea urchin dredge 40 for sea urchin)
  agf_map <- dominant(agf)
  agf_species <- dominant(agf, c("source", "gear_no", "gear_name", "species")) |>
    dplyr::semi_join(
      agf_map |> dplyr::select(-"share"),
      by = c("source", "gear_no", "gear_name")
    ) |>
    dplyr::anti_join(
      agf_map |> dplyr::select(-"share"),
      by = c("source", "gear_no", "gear_name", "gear_id")
    )
  agf_map <- dplyr::bind_rows(agf_map, agf_species)

  out <- dplyr::bind_rows(agg_map, lods_map, agf_map) |>
    dplyr::mutate(
      gear_no = as.integer(gear_no),
      species = as.integer(species),
      gear_id = as.integer(gear_id),
      share = round(share, 4)
    ) |>
    dplyr::select(source, gear_no, gear_name, species, gear_id, share) |>
    dplyr::arrange(source, gear_no, gear_name, species) |>
    as.data.frame()
  landings_gear <- out
  save(landings_gear, file = path, compress = "xz")
  invisible(out)
}

# The maturity stage codes by species (biota.kynthroski), package data
# maturity_stages:
#   mar <- mar::connect_mar()
#   pax:::data_update_maturity_stages(mar)
data_update_maturity_stages <- function(mar, path = "pax/data/maturity_stages.rda") {
  if (!requireNamespace("mar", quietly = TRUE)) {
    stop("mar package not available, cannot import from DB")
  }

  # NSE variables
  tegund_nr <- kynthroski_nr <- kynthroski <- lysing <- frekari_lysing <- NULL
  species <- maturity_stage <- NULL

  maturity_stages <- mar::les_kynthroska(mar) |>
    dplyr::collect() |>
    dplyr::transmute(
      species = as.integer(tegund_nr),
      maturity_stage = as.integer(kynthroski_nr),
      stage = kynthroski,
      description = lysing,
      description_more = frekari_lysing
    ) |>
    dplyr::arrange(species, maturity_stage) |>
    as.data.frame()
  save(maturity_stages, file = path, compress = "xz")
}
