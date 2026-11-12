# The landings register from the parquet datasets of mar (DuckDB), which
# replaced the Oracle landings wrappers (mar::landadur_afli(),
# lods_oslaegt(), fiskifelag_oslaegt(), agf_aflagrunnur(), kvoti_*) in
# October 2026:
#
# * "vigtarskra": fiskifelagid.vigtarskra66_81 (1966-1981) and
#   "fiskifelag": fiskifelagid.landed_catch_pre94 (1982-1993), from the
#   aggregated landings (mar::ln_agg(), by month);
# * "lods": the landing base (mar::ln_landings(), ln_gear(), ln_catch()), the
#   old landings register kvoti.lods_slaegt from September 1992 and
#   agf.aflagrunnur from September 2007.
#
# The old kvoti.lods_oslaegt continued the register's codes after 2007; the
# landing base keeps the AGF codes there (fishing area "IS" for "I", the AGF
# stock 4101 for the register's stock code 51, ...). The datasets
# agf_fishing_area and agf_stock map them back, so that fishing_area and
# stock_code keep one coding over all years.

mar_require_parquet <- function() {
  if (!requireNamespace("mar", quietly = TRUE)) {
    stop("mar package not available, cannot import from DB")
  }
  if (!exists("connect_landings", envir = asNamespace("mar"))) {
    stop(
      "The installed mar has no parquet datasets (mar::connect_landings()), ",
      "update mar (github::mfriops/mar, from October 2026)"
    )
  }
  if (!requireNamespace("duckdb", quietly = TRUE)) {
    stop("duckdb package not available, cannot read the mar parquet datasets")
  }
}

# Run fun(con) on a mar parquet connection, closed afterwards
mar_with_parquet <- function(connect, fun) {
  mar_require_parquet()
  con <- connect()
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  fun(con)
}

# Read a pax lookup dataset (data/*.txt)
mar_pax_data <- function(name) {
  env <- new.env()
  utils::data(list = name, package = "pax", envir = env)
  env[[name]]
}

# ICES area of the register's fishing areas (veidisvaedi), as mar's old
# kvoti_veidisv()
mar_ices_area_of <- function(fishing_area) {
  dplyr::case_when(
    fishing_area %in% c('M', 'E', 'Q', 'I', 'F', 'A', 'N', 'P', 'K', 'S') ~ '5a',
    fishing_area %in% c('D', 'G') ~ '14b',
    fishing_area %in% c('O') ~ '5b',
    fishing_area %in% c('H', 'J') ~ '2',
    fishing_area %in% c('Y', 'B', 'X') ~ '1',
    fishing_area %in% c('R') ~ '12',
    TRUE ~ NA_character_
  )
}

# Fishing year label as the old advice code: calendar years before 1991
# (and January-August 1991), then September-August, e.g. "2024/2025"
mar_fishing_year_of <- function(year, month) {
  dplyr::case_when(
    year < 1991 ~ as.character(year),
    year == 1991 & month < 9 ~ as.character(year),
    month < 9 ~ paste(year - 1, year, sep = "/"),
    TRUE ~ paste(year, year + 1, sep = "/")
  )
}

# Quota period (timabil) as mar's old lods_oslaegt(), e.g. "20242025"
mar_period_of_month <- function(year, month) {
  ifelse(month < 9, paste0(year - 1, year), paste0(year, year + 1))
}

# The old landings gear code (veidarfaeri, as biota.gear_mapping) of the
# parquet's harmonised gear code gear_no and original gear name gear_name,
# from the landings_gear dataset: AGF by code and species, else by code;
# the others by source and name, else by the name in the register of
# 1992-2007, else by the AGF code
mar_landings_gear_id <- function(source, gear_no, gear_name, species) {
  n <- max(length(source), length(gear_no), length(gear_name), length(species))
  source <- rep_len(source, n)
  gear_no <- rep_len(gear_no, n)
  gear_name <- rep_len(gear_name, n)
  species <- rep_len(species, n)
  g <- mar_pax_data("landings_gear")
  key <- function(...) paste(..., sep = "\r")
  agf <- g[g$source == "agf" & is.na(g$species), ]
  agf_species <- g[g$source == "agf" & !is.na(g$species), ]
  other <- g[g$source != "agf", ]
  lods <- g[g$source == "lods", ]
  by_species <- ifelse(
    source == "agf",
    agf_species$gear_id[match(
      key(gear_no, species),
      key(agf_species$gear_no, agf_species$species)
    )],
    NA
  )
  by_source <- other$gear_id[match(
    key(source, gear_no, gear_name),
    key(other$source, other$gear_no, other$gear_name)
  )]
  by_name <- lods$gear_id[match(
    key(gear_no, gear_name),
    key(lods$gear_no, lods$gear_name)
  )]
  by_code <- agf$gear_id[match(gear_no, agf$gear_no)]
  as.numeric(ifelse(
    source == "agf",
    dplyr::coalesce(by_species, by_code),
    dplyr::coalesce(by_source, by_name, by_code)
  ))
}

#' @return \subsection{mar_landings_register}{The landings register (see
#'   above), summed by month, one row per ``source`` (``"vigtarskra"``,
#'   ``"fiskifelag"``, ``"lods"``), ``agf`` (``TRUE`` for the AGF landings of
#'   the landing base, from September 2007), ``year``, ``month``,
#'   ``vessel_id``, ``port``, ``gear_no`` and ``gear_name`` (the parquet's
#'   harmonised gear code and gear name), ``gear_id`` (the old landings gear
#'   code ``veidarfaeri``, see [landings_gear]), ``area``
#'   (fishing area as in the dataset), ``fishing_area`` (the register's code,
#'   e.g. ``"I"``), ``species``, ``stock`` (stock code as in the dataset),
#'   ``stock_code`` (the register's stock code ``kfteg``, -1 for
#'   ``fiskifelag`` as mar's old fiskifelag_oslaegt()), ``stock_name`` (the
#'   AGF stock name), ``period``, ``fishing_year`` and ``catch`` (ungutted,
#'   kg)}
#' @noRd
mar_landings_register <- function(
  species,
  year_start = NULL,
  year_end = NULL,
  sources = c("vigtarskra", "fiskifelag", "lods")
) {
  # NSE variables
  species_no <- weight <- weight_unit <- registration_no <- port <- NULL
  gear_no <- area <- stock <- year <- month <- source <- g_id <- l_id <- NULL
  landing_date <- landing_port <- l_source <- agf <- gear_name <- NULL

  years <- function(tbl) {
    if (!is.null(year_start)) {
      tbl <- dplyr::filter(tbl, year >= local(year_start))
    }
    if (!is.null(year_end)) {
      tbl <- dplyr::filter(tbl, year <= local(year_end))
    }
    tbl
  }
  num <- c("year", "month", "vessel_id", "port", "gear_no", "species",
    "stock", "catch")
  std <- function(x) {
    dplyr::mutate(
      x,
      dplyr::across(dplyr::all_of(num), as.numeric),
      area = as.character(area),
      gear_name = as.character(gear_name)
    )
  }

  out <- list()
  agg_sources <- c(
    vigtarskra = "fiskifelagid.vigtarskra66_81",
    fiskifelag = "fiskifelagid.landed_catch_pre94"
  )
  agg_sources <- agg_sources[names(agg_sources) %in% sources]
  if (length(agg_sources) > 0) {
    out$agg <- mar_with_parquet(mar::connect_landings_agg, function(con) {
      mar::ln_agg(con) |>
        dplyr::filter(
          species_no %in% local(species),
          source %in% local(unname(agg_sources))
        ) |>
        years() |>
        dplyr::group_by(
          source,
          year,
          month,
          vessel_id = registration_no,
          port,
          gear_no,
          gear_name,
          area,
          species = species_no,
          stock
        ) |>
        dplyr::summarise(catch = sum(weight, na.rm = TRUE), .groups = "drop") |>
        dplyr::collect()
    }) |>
      dplyr::mutate(
        source = names(agg_sources)[match(source, agg_sources)],
        agf = FALSE
      ) |>
      std()
  }
  if ("lods" %in% sources) {
    out$lods <- mar_with_parquet(mar::connect_landings, function(con) {
      mar::ln_catch(con) |>
        dplyr::filter(species_no %in% local(species)) |>
        dplyr::select(g_id, species_no, stock, weight, weight_unit) |>
        dplyr::inner_join(
          mar::ln_gear(con) |>
            dplyr::select(g_id, l_id, gear_no, gear_name, area),
          by = "g_id"
        ) |>
        dplyr::inner_join(
          mar::ln_landings(con) |>
            dplyr::select(
              l_id,
              landing_date,
              registration_no,
              landing_port,
              l_source = source
            ),
          by = "l_id"
        ) |>
        dplyr::mutate(
          year = as.integer(year(landing_date)),
          month = as.integer(month(landing_date)),
          agf = substr(l_source, 1, 4) == "agf.",
          weight = weight * ifelse(weight_unit %in% "t", 1000, 1)
        ) |>
        years() |>
        dplyr::group_by(
          agf,
          year,
          month,
          vessel_id = registration_no,
          port = landing_port,
          gear_no,
          gear_name,
          area,
          species = species_no,
          stock
        ) |>
        dplyr::summarise(catch = sum(weight, na.rm = TRUE), .groups = "drop") |>
        dplyr::collect()
    }) |>
      dplyr::mutate(source = "lods") |>
      std()
  }
  out <- dplyr::bind_rows(out)
  if (nrow(out) == 0) {
    out$fishing_area <- character(0)
    out$stock_code <- numeric(0)
    out$stock_name <- character(0)
    out$period <- character(0)
    out$fishing_year <- character(0)
    return(out)
  }

  agf_area <- mar_pax_data("agf_fishing_area")
  agf_stock <- mar_pax_data("agf_stock")
  out |>
    dplyr::mutate(
      gear_id = mar_landings_gear_id(
        ifelse(agf, "agf", source),
        gear_no,
        gear_name,
        species
      ),
      fishing_area = dplyr::case_when(
        agf ~ agf_area$fishing_area[match(area, agf_area$agf_area)],
        # vigtarskra66_81 has no fishing area, Icelandic waters as mar's old
        # landadur_afli()
        source == "vigtarskra" ~ "I",
        TRUE ~ area
      ),
      stock_code = dplyr::case_when(
        # The register's "no stock" code 0 where the AGF stock has no code
        agf ~ dplyr::coalesce(
          as.numeric(agf_stock$stock_code[match(stock, agf_stock$stock)]),
          0
        ),
        source == "lods" ~ stock,
        TRUE ~ -1
      ),
      stock_name = ifelse(
        agf,
        agf_stock$stock_name[match(stock, agf_stock$stock)],
        NA_character_
      ),
      period = ifelse(
        source == "lods",
        mar_period_of_month(year, month),
        as.character(year)
      ),
      fishing_year = mar_fishing_year_of(year, month)
    )
}

# Run fun(con) on a connection to the MFRI AWS Postgres database (the vessel
# register since October 2026), closed afterwards
mar_with_aws <- function(fun) {
  if (!requireNamespace("mar", quietly = TRUE)) {
    stop("mar package not available, cannot import from DB")
  }
  if (!exists("connect_aws", envir = asNamespace("mar"))) {
    stop(
      "The installed mar reads the vessel register from Oracle, update mar ",
      "(github::mfriops/mar, from October 2026)"
    )
  }
  con <- mar::connect_aws()
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  fun(con)
}

#' @return \subsection{mar_vessel_register}{The vessel register (AWS
#'   Postgres, ``mar::vessel()`` and the current-state tables joined on
#'   ``vessel_id``), one row per vessel, ``vessel_id`` being the registration
#'   number. Owner names, addresses and identity numbers are left out}
#' @noRd
mar_vessel_register <- function() {
  # NSE variables
  vessel_id <- valid_from <- registration_no <- name <- status <- NULL
  usage_category_no <- usage_category <- power_kw <- construction_year <- NULL
  region_acronym <- region_no <- home_port_no <- operational_category_no <- NULL
  operational_category <- length <- brutto_grt <- brutto_weight_tons <- NULL

  mar_with_aws(function(con) {
    mar::vessel(con) |>
      dplyr::select(
        vessel_id,
        registration_no,
        name,
        status,
        usage_category_no,
        usage_category,
        power_kw,
        construction_year
      ) |>
      dplyr::left_join(
        mar::vessel_identification(con) |>
          dplyr::select(vessel_id, region_acronym, region_no, home_port_no),
        by = "vessel_id"
      ) |>
      dplyr::left_join(
        mar::vessel_operational(con) |>
          dplyr::select(vessel_id, operational_category_no, operational_category),
        by = "vessel_id"
      ) |>
      dplyr::left_join(
        mar::vessel_measurement(con) |>
          dplyr::select(vessel_id, length, brutto_grt, brutto_weight_tons),
        by = "vessel_id"
      ) |>
      dplyr::collect()
  }) |>
    dplyr::select(-vessel_id) |>
    dplyr::rename(vessel_id = registration_no) |>
    dplyr::mutate(
      dplyr::across(
        c(
          "vessel_id", "usage_category_no", "operational_category_no",
          "home_port_no", "power_kw", "length", "brutto_grt",
          "brutto_weight_tons", "construction_year"
        ),
        as.numeric
      ),
      region_no = as.character(region_no)
    ) |>
    dplyr::distinct(vessel_id, .keep_all = TRUE)
}

# ICES official catches (biota.ices_landed_catch, Oracle) of the species, as
# mar's old ices_landed_catch() and its part of landadur_afli(): the
# Icelandic catches in 5a after 1965 and all 5a catches after 2013 left out
# (the landings register has them). Columns year, land (country), ices_svaedi,
# species, erlent_skip and catch (kg)
mar_ices_landed_catch <- function(mar, species, year_start = NULL, year_end = NULL) {
  # NSE variables
  tegund_nr <- fao_3alpha <- species_code <- landings <- area <- NULL
  subarea <- country <- year <- NULL

  tegund <- mar::les_tegund(mar) |>
    dplyr::select(tegund_nr, fao_3alpha) |>
    dplyr::collect() |>
    dplyr::mutate(
      species_code = dplyr::case_when(
        tegund_nr == 5 ~ 'RED',
        tegund_nr == 12 ~ 'SYR',
        tegund_nr == 15 ~ 'SKT',
        tegund_nr == 44 ~ 'CPZ',
        tegund_nr == 50 ~ 'REG',
        tegund_nr == 52 ~ 'HGS',
        tegund_nr == 67 ~ 'GDG',
        tegund_nr == 1057 ~ 'SEH',
        tegund_nr == 1100 ~ 'HBB',
        tegund_nr == 1150 ~ 'LRD',
        tegund_nr == 1140 ~ 'TZH',
        tegund_nr == 1110 ~ 'ITV',
        TRUE ~ fao_3alpha
      )
    ) |>
    dplyr::filter(!is.na(species_code)) |>
    dplyr::select(tegund_nr, species_code)
  ices <- mar::tbl_mar(mar, 'biota.ices_landed_catch') |>
    dplyr::filter(!is.na(landings)) |>
    dplyr::collect() |>
    dplyr::inner_join(
      tegund |> dplyr::filter(tegund_nr %in% local(species)),
      by = 'species_code',
      relationship = "many-to-many"
    ) |>
    dplyr::mutate(area = trimws(area), subarea = trimws(subarea)) |>
    dplyr::filter(
      !(country %in% 'Iceland' & year > 1965 & area %in% '5') | year < 1966,
      !(year > 2013 & area %in% '5' & startsWith(subarea, 'a') %in% TRUE)
    ) |>
    dplyr::transmute(
      year = as.numeric(year),
      land = country,
      ices_svaedi = trimws(paste0(area, ifelse(is.na(subarea), "", subarea))),
      species = as.numeric(tegund_nr),
      erlent_skip = ifelse(country %in% 'Iceland', 0, 1),
      catch = 1000 * landings
    )
  if (!is.null(year_start)) {
    ices <- dplyr::filter(ices, year >= year_start)
  }
  if (!is.null(year_end)) {
    ices <- dplyr::filter(ices, year <= year_end)
  }
  ices
}
