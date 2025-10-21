hpax_connect <- function() {
  pax::pax_new(
    dbcon = mar::connect_mar(),
    class = "hafropax"
  )
}

pax_disconnect.hafropax <- function(pcon) {
  DBI::dbDisconnect(pcon$dbcon)
}
