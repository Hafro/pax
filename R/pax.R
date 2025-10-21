pax_new <- function(dbcon, class_name) {
  pcon <- list(dbcon = dbcon)
  class(pcon) <- c(class_name, "pax", class(pcon))
  return(pcon)
}

pax_disconnect <- function(pcon) UseMethod("pax_disconnect")

pax_tbl <- function(pcon, tbl) {
  stopifnot("tbl_sql" %in% class(tbl))

  tbl$src$pcon <- pcon
  class(tbl) <- c("tbl_pax", class(tbl))
  return(tbl)
}

as_pax <- function(pcon_or_tbl) {
  if ("pax" %in% class(pcon_or_tbl)) {
    return(pcon_or_tbl)
  }

  if ("tbl_pax" %in% class(pcon_or_tbl)) {
    # Pull out embedded pax object
    return(pcon_or_tbl$src$pcon)
  }

  stop(
    sys.call(-1)[[1]],
    " needs either a pax connection or pax table, got ",
    deparse1(class(pcon_or_tbl))
  )
}

pax_temptbl <- function(pcon_or_tbl, df) {
  pcon <- as_pax(pcon_or_tbl)

  # Small tables just get copied inline
  if (nrow(df) < 1000) {
    return(pax_tbl(pcon, dbplyr::copy_inline(pcon$dbcon, df)))
  }

  stop("TODO: Use copy_to() into a temporary table")
}
