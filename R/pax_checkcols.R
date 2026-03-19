#' Check that a table contains required columns
#'
#' Validates that a data frame or table contains all expected columns.
#' If any columns are missing, an informative error is raised naming the
#' missing columns, the calling function, and the full list of columns
#' actually present.
#'
#' @param tbl A data frame or object with \code{colnames()}.
#' @param ... One or more column name strings that must be present in \code{tbl}.
#' @param expected Either a table name or function call (ending with ()), that will be shown as a hint to the correct data source.
#'
#' @return \code{invisible(NULL)} if all expected columns are present.
#'   Otherwise, \code{\link{stop}} is called.
#'
#' @examples
#' df <- data.frame(a = 1, b = 2)
#' pax_checkcols(df, "a", "b")   # passes silently
#' \dontrun{
#' pax_checkcols(df, "a", "c")   # error: missing column "c"
#' }
#'
#' @keywords internal
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
