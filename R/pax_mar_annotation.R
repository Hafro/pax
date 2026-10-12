#' Video annotations from MAR (Hafmynd)
#'
#' The annotations of the underwater video surveys in
#' ``HAFMYND.VIDEO_TAXON_CONCEPT_V`` (e.g. the Nephrops UWTV survey, project
#' ``"Humar"``): one row per annotation, with the concept (e.g. ``"Hole"``,
#' ``"Hole_Neph"``, ``"Trollfar"``), the taxon, the cruise and station, the
#' ``final`` flag and the annotator.
#'
#' The annotator is stored only as a pseudonymous id
#' ([pax_annotator_id()]), not as the database user name: an assessment may
#' need to select one annotator's rows (the Nephrops index uses one
#' annotator's final annotations), but the name is personal data and is not
#' needed for that. The free-text comments and the file paths of the videos
#' are left out.
#'
#' @param mar A MAR database connection, as returned by ``mar::connect_mar()``
#' @param project Names of the Hafmynd projects to import (``project_name``,
#'   e.g. ``"Humar"``), ``NULL`` for all
#' @return \subsection{pax_mar_video_annotation}{A data frame with columns
#'   ``annotation_id``, ``video_id``, ``project``, ``trip`` (cruise),
#'   ``station_number``, ``year`` (from the cruise name), ``time``
#'   (``real_time``), ``video_timestamp``, ``concept_id``, ``concept``,
#'   ``taxon_id``, ``taxon``, ``confidence``, ``individuals``, ``cover``,
#'   ``seabed_type``, ``coordinates`` (position in the frame, text),
#'   ``final`` (integer 0/1) and ``annotator`` (pseudonymous id)}
#' @name pax_mar_annotation
pax_mar_video_annotation <- function(mar, project = NULL) {
  if (!requireNamespace("mar", quietly = TRUE)) {
    stop("mar package not available, cannot import from DB")
  }
  # NSE variables
  project_name <- NULL

  out <- mar::tbl_mar(mar, "hafmynd.video_taxon_concept_v")
  if (!is.null(project)) {
    out <- dplyr::filter(out, project_name %in% local(project))
  }
  out |>
    dplyr::collect() |>
    video_annotation_tidy() |>
    decorate_mar()
}

# Columns, types and the pseudonymous annotator of the collected
# HAFMYND.VIDEO_TAXON_CONCEPT_V rows (lower case names, as mar::tbl_mar()).
# The driver returns most numbers as text, so convert them here
video_annotation_tidy <- function(df) {
  names(df) <- tolower(names(df))
  num <- function(x) suppressWarnings(as.numeric(x))
  tibble::tibble(
    annotation_id = num(df$id),
    video_id = num(df$video_id),
    project = as.character(df$project_name),
    trip = as.character(df$cruise_name),
    station_number = num(df$station_number),
    year = num(stringr::str_extract(df$cruise_name, "\\d{4}")),
    time = df$real_time,
    video_timestamp = num(df$video_timestamp),
    concept_id = num(df$concept_id),
    concept = as.character(df$concept_name),
    taxon_id = num(df$taxon_id),
    taxon = as.character(df$taxon_name),
    confidence = num(df$confidence),
    individuals = num(df$individuals),
    cover = num(df$cover),
    seabed_type = as.character(df$seabed_type_name),
    coordinates = as.character(df$coordinates),
    final = as.integer(num(df$final)),
    annotator = pax_annotator_id(df$insert_user)
  )
}

#' @param user Database user names
#' @return \subsection{pax_annotator_id}{Pseudonymous ids of the users: the
#'   first 10 hexadecimal digits of the SHA-256 hash of the user name in
#'   upper case without the Oracle ``OPS$`` prefix (``"OPS$XYZ"`` and
#'   ``"XYZ"`` are the same user and get the same id), ``NA`` for a missing
#'   name. The id is stable between imports, so a stock can select an
#'   annotator by id without storing the name. It is a pseudonym, not
#'   anonymous: anyone who knows a user name can compute its id}
#' @rdname pax_mar_annotation
#' @export
pax_annotator_id <- function(user) {
  user <- as.character(user)
  key <- toupper(trimws(sub("^OPS\\$", "", user, ignore.case = TRUE)))
  id <- vapply(
    key,
    function(k) substr(digest::digest(k, algo = "sha256", serialize = FALSE), 1, 10),
    character(1),
    USE.NAMES = FALSE
  )
  id[is.na(user)] <- NA_character_
  id
}
