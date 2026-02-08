pax_checkcols <- function(tbl, ...) {
  expected_cols <- as.character(list(...))
  actual_cols <- colnames(tbl)
  missing_cols <- setdiff(expected_cols, actual_cols)

  if (length(missing_cols) == 0) {
    # Everything fine, carry on
    return(invisible(NULL))
  }

  stop(
    "Table is missing colums: ",
    paste(missing_cols, collapse = ", "),
    "\nFull column list: ",
    paste(actual_cols, collapse = ", "),
    ""
  )
}
