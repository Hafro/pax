# https://docs.ropensci.org/targets/reference/tar_format.html
# https://docs.ropensci.org/targets/reference/tar_target.html
# https://duckdb.org/docs/stable/clients/r
# TODO: Refresh on regular interval? https://github.com/ropensci/targets/discussions/429
pax_tar_format_duckdb <- function() {
  targets::tar_format(
    read = function(path) {
      # TODO: Attach a memory database & copy, so write makes sense?
      pax::pax_connect(path, read_only = TRUE)
    },
    write = function(object, path) {
      # https://duckdb.org/docs/stable/guides/snippets/copy_in-memory_database_to_file
      DBI::dbExecute(
        object,
        dbplyr::build_sql("ATTACH ", path, " AS out_db;", con = object)
      )
      DBI::dbExecute(object, "COPY FROM DATABASE memory TO out_db;")
      DBI::dbDisconnect(object)
    },
    marshal = function(object) {
      # TODO: Flush object, load object$dbdir into memory
      stop("Not Implemented: marshal")
    },
    unmarshal = function(object) {
      stop("Not Implemented: unmarshal")

      # TODO: Dump (object) to temporary file
      pcon <- pax::pax_connect()
      DBI::dbExecute(
        pcon,
        dbplyr::build_sql("ATTACH ", tmp_path, " AS in_db;", con = object)
      )
      DBI::dbExecute(pcon, "COPY FROM DATABASE in_db TO memory;")
      return(pcon)
    },
  )
}

# tarchetypes::tar_format_nanoparquet, but with column filtering
pax_tar_format_parquet <- function() {
  rlang::check_installed("nanoparquet")
  read <- function(path) {
    tarchetypes::tar_nanoparquet_read(path, class = "tbl")
  }
  write <- function(object, path) {
    tarchetypes::tar_nanoparquet_write(object, path, compression = "snappy")
  }
  convert <- function(object) {
    cols <- colnames(object)
    # TODO: Deselect anything that isn't going to fit? Special h3_cells serialisation format?
    if ("geom" %in% cols) {
      object <- dplyr::select(object, -geom)
    }
    if ("h3_cells" %in% cols) {
      object <- dplyr::select(object, -h3_cells)
    }
    # TODO: Why do we need to as.data.frame() it?
    as.data.frame(object)
  }
  targets::tar_format(read = read, write = write, convert = convert)
}
