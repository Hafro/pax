if (!interactive()) {
  options(warn = 2, error = function() {
    sink(stderr())
    traceback(3)
    q(status = 1)
  })
}
library(unittest)

library(pax)

ok_group("pax_tar_format_duckdb: source database names that need quoting", {
  if (!requireNamespace("targets", quietly = TRUE)) {
    ok(TRUE, "# skip: targets not installed")
    return()
  }
  dir <- tempfile("tar_")
  dir.create(dir)
  old_wd <- setwd(dir)
  on.exit(setwd(old_wd), add = TRUE)

  # A pax_db copy as PAX_SOURCE_DB: the attached database takes its name
  # from the file, here one starting with a digit and containing "-"
  for (src_name in c("21-hal-pax_db", "pax db's")) {
    src <- file.path(dir, paste0(src_name, ".duckdb"))
    pcon <- pax_connect(src)
    pax_import(pcon, data.frame(val = 1:3), name = "pretend", cite = "test")
    DBI::dbDisconnect(pcon)

    unlink("_targets", recursive = TRUE)
    targets::tar_script(
      list(
        targets::tar_target(
          pax_db,
          pax::pax_connect(Sys.getenv("PAX_SOURCE_DB")),
          format = pax::pax_tar_format_duckdb()
        )
      ),
      ask = FALSE
    )
    old_env <- Sys.getenv("PAX_SOURCE_DB", unset = NA)
    Sys.setenv(PAX_SOURCE_DB = src)
    err <- tryCatch(
      {
        targets::tar_make(callr_function = NULL, reporter = "silent")
        NULL
      },
      error = function(e) conditionMessage(e)
    )
    if (is.na(old_env)) {
      Sys.unsetenv("PAX_SOURCE_DB")
    } else {
      Sys.setenv(PAX_SOURCE_DB = old_env)
    }
    ok(ut_cmp_identical(err, NULL), paste("pax_db built from", src_name))
    if (!is.null(err)) {
      next
    }
    out <- targets::tar_read(pax_db)
    ok(ut_cmp_equal(
      DBI::dbGetQuery(out, "SELECT val FROM pretend ORDER BY val")$val,
      1:3
    ), paste("Tables copied from", src_name))
    DBI::dbDisconnect(out)
  }
})
