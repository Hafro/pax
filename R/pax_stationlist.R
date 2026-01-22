# Fetch stationlist data from published PDFs
update_stationlist <- function(
  name,
  pdf_url = NULL,
  pages = NULL,
  col_names = NULL
) {
  # Convert DegreesMinutesSeconds to decimal
  degrees_to_decimal <- function(col) {
    # Chop 6 digit string into 3 pairs of digits, then sum each triple together
    vapply(
      strsplit(sprintf("%06d", col), "(?<=\\d{2})", perl = TRUE),
      function(parts) {
        as.numeric(parts[[1]]) +
          as.numeric(parts[[2]]) / 60 +
          as.numeric(parts[[3]]) / 3600 +
          0
      },
      numeric(1)
    )
  }

  if (!requireNamespace("tabulapdf", quietly = TRUE)) {
    stop("This requires the tabulapdf package")
  }

  if (is.null(pdf_url)) {
    pdf_url <- list(
      hafro_smb = "https://www.hafogvatn.is/static/research/files/rallhandbok_2025-enska-1.pdf",
      hafro_smh_shelf = "https://www.hafogvatn.is/static/research/files/smh_manual_2025.pdf",
      hafro_smh_deep = "https://www.hafogvatn.is/static/research/files/smh_manual_2025.pdf",
      end = NULL
    )[[name]]
  }

  if (is.null(pages)) {
    pages <- list(
      hafro_smb = c(28:32, 34:36, 38:40),
      hafro_smh_shelf = c(36:39),
      hafro_smh_deep = c(39:44),
      end = NULL
    )[[name]]
  }

  page_areas <- list(
    hafro_smb = list(),
    hafro_smh_shelf = list(
      # Extract top table from page
      "39" = c(
        top = 64.6513234781129,
        left = 65.5006169737019,
        bottom = 586.946846857474,
        right = 557.388317519765
      )
    ),
    hafro_smh_deep = list(
      # Extract bottom table from page
      "39" = c(
        top = 690.098396676486,
        left = 69.1281073907082,
        bottom = 764.193171898592,
        right = 572.623777271192
      )
    ),
    end = NULL
  )[[name]]

  gear_type <- list(
    hafro_smb = 73,
    hafro_smh_shelf = 77,
    hafro_smh_deep = 78,
    end = NULL
  )[[name]]

  if (is.null(col_names)) {
    col_names <- list(
      hafro_smb = c(
        "reitur",
        "tow_nr",
        "smareitur",
        "start_lat",
        "start_lon",
        "end_lat",
        "end_lon",
        "start_tow_depth",
        "end_tow_depth",
        "sweep",
        "warp",
        "tow_dir",
        "tow_length"
      ),
      hafro_smh_shelf = c(
        "reitur",
        "tow_nr",
        "smareitur",
        "start_lat",
        "start_lon",
        "end_lat",
        "end_lon",
        "start_tow_depth",
        "end_tow_depth",
        "warp",
        "tow_length",
        "tow_vert_open",
        "tow_horiz_open"
      ),
      hafro_smh_deep = c(
        "reitur",
        "tow_nr",
        "smareitur",
        "start_lat",
        "start_lon",
        "end_lat",
        "end_lon",
        "start_tow_depth",
        "end_tow_depth",
        "warp",
        "tow_length",
        "tow_vert_open",
        "tow_horiz_open"
      ),
      end = NULL
    )[[name]]
  }

  tempdir_path <- tempfile(pattern = "tabulapdf")
  dir.create(tempdir_path, recursive = TRUE)
  on.exit(unlink(tempdir_path, recursive = TRUE), add = TRUE)

  # Pre-fetch PDF
  if (startsWith(pdf_url, "https:")) {
    pdf_path <- file.path(tempdir_path, basename(pdf_url))
    utils::download.file(pdf_url, pdf_path, mode = "wb")
  } else {
    pdf_path <- pdf_url
  }

  # Decode each page as CSV
  for (p in pages) {
    # NB: Do page at a time so we can use guess when area not specified
    dir.create(file.path(tempdir_path, p), recursive = TRUE)
    page_areas[as.character(p)]
    tabulapdf::extract_tables(
      pdf_path,
      pages = p,
      # guess if area not specified
      area = if (p %in% page_areas) page_areas[as.character(p)] else NULL,
      guess = !(p %in% page_areas),
      output = "csv",
      outdir = file.path(tempdir_path, p)
    )
  }

  station_df <- lapply(
    list.files(
      tempdir_path,
      pattern = "\\.csv$",
      recursive = TRUE,
      full.names = TRUE
    ),
    function(csv_path) {
      # Read entire table
      df <- read.csv(csv_path, header = FALSE)

      # Remove non-numeric rows (i.e. header)
      df <- df[grepl("^\\d", df[, 1]), ]

      # Unmerge merged columns
      for (i in which(grepl("^\\d+\\s+\\d+$", df[1, ]))) {
        old_name <- names(df)[[i]]
        # Column with number-space-number, extract either half and add
        sep <- t(do.call(cbind, strsplit(df[, i], "\\s+")))
        df <- cbind(
          df[, seq_len(i - 1)],
          sep[, 1],
          sep[, 2],
          df[, seq(i + 1, ncol(df))]
        )
      }

      # Try to convert to numeric, remove any NA columns
      df <- as.data.frame(lapply(df, function(col) {
        suppressWarnings(as.numeric(col))
      }))
      df <- df[, sapply(df, function(x) any(is.finite(x)))]

      # Should be able to apply col_names now
      names(df) <- col_names

      return(df)
    }
  )

  # Combine list of data.frames together
  station_df <- dplyr::bind_rows(station_df)

  # Convert lat/lon values to decimal
  station_df[, "start_lat"] <- degrees_to_decimal(station_df[, "start_lat"])
  station_df[, "start_lon"] <- -degrees_to_decimal(station_df[, "start_lon"])
  station_df[, "end_lat"] <- degrees_to_decimal(station_df[, "end_lat"])
  station_df[, "end_lon"] <- -degrees_to_decimal(station_df[, "end_lon"])
  station_df[, "gridcell"] <- 10 *
    station_df[, "reitur"] +
    station_df[, "smareitur"]
  station_df[, "station"] <-
    station_df[, "reitur"] *
    10000 +
    ifelse(is.na(station_df[, "tow_nr"]), 0, station_df[, "tow_nr"]) * 100 +
    gear_type

  write.table(station_df, file = paste0("pax/data/stationlist_", name, ".txt"))
  invisible(station_df)
}
if (FALSE) {
  pax:::update_stationlist("hafro_smb")
  pax:::update_stationlist("hafro_smh_shelf")
  pax:::update_stationlist("hafro_smh_deep")
}
