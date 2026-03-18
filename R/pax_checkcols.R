pax_checkcols <- function(tbl, ..., expected = NULL) {
  expected_cols <- as.character(unlist(list(...)))
  actual_cols <- colnames(tbl)
  missing_cols <- setdiff(expected_cols, actual_cols)

  if (length(missing_cols) == 0) {
    # Everything fine, carry on
    return(invisible(NULL))
  }
  callee <- deparse1(sys.call(which = -1)[[1]])

  rlang::abort(c(
    paste0("Table is missing columns required by ", callee),
    i = if (is.null(expected)) {
      NULL
    } else if (endsWith(expected, "()")) {
      paste0("Expecting ", expected, " output")
    } else {
      paste0("Expecting a '", expected, "' table")
    },
    i = paste0("Missing cols: ", paste(missing_cols, collapse = ", ")),
    i = paste0("Table has columns: ", paste(actual_cols, collapse = ", "))
  ))
}
