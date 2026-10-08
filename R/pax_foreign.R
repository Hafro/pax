#' Import foreign (non-MFRI) data
#'
#' Functions to read data of other countries (Greenland, the Faroe Islands)
#' into a pax database, in the same way as the MFRI data: from the tables in
#' mar where they are kept today (the ``ops$will`` schema), or from the files
#' delivered by the other institutes, read from a path given when the
#' database is built. The data are never part of the package. Like the
#' [pax_mar] functions, they return tables decorated for [pax_import()]; see
#' the ``foreign_tables`` and ``foreign_files`` arguments of
#' [pax_from_mar()].
#'
#' @name pax_foreign
NULL

#' @return \subsection{pax_foreign_mar_tables}{A data.frame of the foreign
#'   data tables in mar that [pax_mar_foreign()] reads: ``name`` (the pax
#'   table name), ``mar_table`` and ``description``}
#' @rdname pax_foreign
pax_foreign_mar_tables <- function() {
  # fmt: skip
  out <- data.frame(
    name = c(
      "ghl_catch", "ghl_comm_stations", "ghl_comm_samples",
      "ghl_greenland_stations", "ghl_greenland_lengths",
      "ghl_faroes_survey_stations", "ghl_faroes_survey_length",
      "ghl_faroes_survey_age", "ghl_strata_mapping",
      "bli_gss_greenland_stations", "bli_gss_greenland_lengths",
      "bli_gss_strata_mapping_smh", "bli_gss_strata_mapping_smb",
      "mdas_catch", "reb_landings_1950_2010"
    ),
    mar_table = c(
      "ghl_catch", "ghl_comm_stations", "ghl_comm_samples",
      "ghl_greenland_stations", "ghl_greenland_lengths",
      "ghl_faroes_survey_stations", "ghl_faroes_survey_length",
      "ghl_faroes_survey_age", "ghl_strata_mapping",
      "bli_gss_greenland_stations", "bli_gss_greenland_lengths",
      "bli_gss_strata_mapping_smh", "bli_gss_strata_mapping_smb",
      "mdas_catch", "REB_landings_1950-2010"
    ),
    description = c(
      "Greenland halibut logbooks by tow: Iceland (IS, compiled logbooks of species 22 and 999), Greenland (GR) and the Faroe Islands (FO)",
      "Greenland halibut commercial samples of Greenland and the Faroe Islands: stations",
      "Greenland halibut commercial samples of Greenland and the Faroe Islands: length counts",
      "Greenland (GINR) Greenland halibut survey: stations",
      "Greenland (GINR) Greenland halibut survey: raised length counts",
      "Faroese deep-water survey: tows",
      "Faroese deep-water survey: length counts",
      "Faroese deep-water survey: individual lengths, weights and ages",
      "Icelandic autumn survey stations (synis_id) to the Greenland-Iceland strata of strata_ghl_strata",
      "Greenland (GINR) surveys GHLE and SFE: stations, with Greenland-Iceland stratum",
      "Greenland (GINR) surveys GHLE and SFE: measured length counts, all species",
      "Icelandic autumn survey stations (synis_id) to the Greenland-Iceland strata, for blue ling",
      "Icelandic spring survey stations (synis_id) to the Greenland-Iceland strata, for blue ling",
      "Logbooks of Iceland (compiled logbooks of species 7 and 19) and Greenland by tow, for blue ling",
      "Beaked redfish landings by year, country and ICES area, 1950-2010"
    ),
    stringsAsFactors = FALSE
  )
  out
}

#' @param mar A MAR database connection, as returned by ``mar::connect_mar()``
#' @param table A ``name`` of [pax_foreign_mar_tables()]
#' @param schema The schema in mar that holds the table
#' @return \subsection{pax_mar_foreign}{The whole table, collected, with the
#'   column names of mar (dots replaced by underscores), decorated with the
#'   pax table name}
#' @rdname pax_foreign
pax_mar_foreign <- function(mar, table, schema = "ops$will") {
  if (!requireNamespace("mar", quietly = TRUE)) {
    stop("mar package not available, cannot import from DB")
  }
  tables <- pax_foreign_mar_tables()
  if (!(table %in% tables$name)) {
    stop("Unknown foreign table: ", table)
  }
  mar_table <- tables$mar_table[tables$name == table]
  out <- mar::tbl_mar(mar, paste0(schema, '."', mar_table, '"')) |>
    dplyr::collect() |>
    as.data.frame()
  colnames(out) <- gsub("[.]", "_", colnames(out))
  pax_decorate(
    out,
    cite = paste0("pax_mar_foreign(mar, '", schema, '."', mar_table, '"', "')"),
    name = table
  )
}

#' @return \subsection{pax_foreign_file_types}{The types of foreign files
#'   [pax_foreign_file()] reads}
#' @rdname pax_foreign
pax_foreign_file_types <- function() {
  c("greenland_logbooks", "greenland_catch", "faroese_logbooks")
}

#' @param path Path(s) of the file(s)
#' @param type The type of file, one of [pax_foreign_file_types()]:
#'   \describe{
#'     \item{``"greenland_logbooks"``}{the logbooks of the Greenland Institute
#'       of Natural Resources, all species (CSV with columns ``code, year,
#'       gear, time1, time2, country, area, area_detail, catch_t,
#'       trawltime_h, lon, lat, eez_grl, eez_ice``); pax table
#'       ``greenland_logbooks``, the columns as in the file, ``time1`` and
#'       ``time2`` date-times (UTC)}
#'     \item{``"greenland_catch"``}{Greenland catch by year, month and area
#'       (CSV with columns ``year, month, area, catch_tot``, e.g. the cod
#'       ``data_GrL_catch.csv``); pax table ``greenland_catch``}
#'     \item{``"faroese_logbooks"``}{Faroese logbook records (the Vorn
#'       ``.xlsx`` extracts with ``STARTLATT``, ``STARTLONG`` in ddmm,
#'       ``DAYDATE``, ``LOGBOOKTYPE``, ``WEIGHTKG``, and the newer ``.csv``
#'       extracts with ``GEARSHOT_LATITUDE``, ``GEARSHOT_LONGITUDE``,
#'       ``GEARSHOT_DATETIME``, ``LOGBOOKINFO_TYPE``, ``WEIGHT_KG``); pax
#'       table ``faroese_logbooks`` with columns ``file``, ``date``
#'       (date-time, UTC), ``year``, ``month``, ``lat``, ``lon`` (decimal
#'       degrees, west negative), ``logbook_type`` (``"LongLine"``,
#'       ``"Trawl"``, ...) and ``catch`` (kg). Needs the readxl package for
#'       the ``.xlsx`` files}
#'   }
#' @return \subsection{pax_foreign_file}{A data.frame decorated with the pax
#'   table name}
#' @rdname pax_foreign
pax_foreign_file <- function(path, type) {
  type <- match.arg(type, pax_foreign_file_types())
  missing <- path[!file.exists(path)]
  if (length(missing) > 0) {
    stop("Foreign data file(s) not found: ", paste(missing, collapse = ", "))
  }
  out <- switch(
    type,
    greenland_logbooks = foreign_greenland_logbooks(path),
    greenland_catch = foreign_greenland_catch(path),
    faroese_logbooks = foreign_faroese_logbooks(path)
  )
  pax_decorate(
    out,
    cite = paste0(
      "pax_foreign_file(",
      paste(basename(path), collapse = ", "),
      ", '",
      type,
      "')"
    ),
    name = type
  )
}

foreign_greenland_logbooks <- function(path) {
  out <- do.call(
    rbind,
    lapply(path, function(p) {
      utils::read.csv(p, stringsAsFactors = FALSE, na.strings = c("", "NA"))
    })
  )
  out$time1 <- foreign_datetime(out$time1)
  out$time2 <- foreign_datetime(out$time2)
  out
}

# Date-times (UTC) of "YYYY-mm-dd HH:MM[:SS]" or "YYYY-mm-dd" (midnight)
foreign_datetime <- function(x) {
  x <- as.character(x)
  out <- as.POSIXct(x, format = "%Y-%m-%d %H:%M:%OS", tz = "UTC")
  i <- is.na(out)
  out[i] <- as.POSIXct(x[i], format = "%Y-%m-%d %H:%M", tz = "UTC")
  i <- is.na(out)
  out[i] <- as.POSIXct(x[i], format = "%Y-%m-%d", tz = "UTC")
  out
}

foreign_greenland_catch <- function(path) {
  do.call(
    rbind,
    lapply(path, function(p) {
      utils::read.csv(p, stringsAsFactors = FALSE, na.strings = c("", "NA"))
    })
  )
}

# Degrees and minutes (ddmm.mm) to decimal degrees
foreign_ddmm <- function(x) {
  (x / 100 - floor(x / 100)) / 0.60 + floor(x / 100)
}

foreign_faroese_logbooks <- function(path) {
  lct <- Sys.getlocale("LC_TIME")
  Sys.setlocale("LC_TIME", "C")
  on.exit(Sys.setlocale("LC_TIME", lct), add = TRUE)

  one <- function(p) {
    if (grepl("\\.xlsx$", p, ignore.case = TRUE)) {
      if (!requireNamespace("readxl", quietly = TRUE)) {
        stop("readxl package not available, cannot read ", p)
      }
      x <- as.data.frame(readxl::read_excel(p, guess_max = 1e6))
      date <- as.POSIXct(x$DAYDATE, tz = "UTC")
      data.frame(
        file = basename(p),
        date = date,
        lat = foreign_ddmm(as.numeric(x$STARTLATT)),
        # NB: Positive (west) in the extracts
        lon = -foreign_ddmm(as.numeric(x$STARTLONG)),
        logbook_type = as.character(x$LOGBOOKTYPE),
        catch = as.numeric(x$WEIGHTKG),
        stringsAsFactors = FALSE
      )
    } else {
      x <- utils::read.csv(p, stringsAsFactors = FALSE)
      date <- as.POSIXct(
        gsub("\\.", ":", x$GEARSHOT_DATETIME),
        format = "%d-%b-%y %H:%M:%OS",
        tz = "UTC"
      )
      data.frame(
        file = basename(p),
        date = date,
        lat = as.numeric(x$GEARSHOT_LATITUDE),
        lon = as.numeric(x$GEARSHOT_LONGITUDE),
        logbook_type = ifelse(
          x$LOGBOOKINFO_TYPE %in% c("LINAGARNASNELLA"),
          "LongLine",
          "Trawl"
        ),
        catch = as.numeric(x$WEIGHT_KG),
        stringsAsFactors = FALSE
      )
    }
  }
  out <- do.call(rbind, lapply(path, one))
  out$year <- as.integer(format(out$date, "%Y"))
  out$month <- as.integer(format(out$date, "%m"))
  out[, c("file", "date", "year", "month", "lat", "lon", "logbook_type", "catch")]
}
