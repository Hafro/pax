ut_tbl <- function(pcon, df) {
  tbl_name <- basename(tempfile(pattern = "uttbl_"))
  pax_import(pcon, df, name = tbl_name, overwrite = TRUE)
  return(dplyr::tbl(pcon, tbl_name))
}

ut_as_sort_df <- function(tbl) {
  dplyr::arrange(tbl, dplyr::pick(everything())) |>
    (function(x) {
      i <- sapply(x, is.factor)
      x[i] <- lapply(x[i], as.character)
      return(x)
    })() |>
    as.data.frame()
}
