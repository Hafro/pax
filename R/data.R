#' Gridcell / division / subdivision mapping
#'
#' @name gridcell
#' @docType data
#' @keywords data
NULL

#' Lookup table of MFDB gear code descriptions
#'
#' @name mfdb_gear_code_desc
#' @docType data
#' @keywords data
NULL

#' Cached ocean bathymetry for default area bounds
#'
#' @name raw_ocean_depth_defbounds
#' @docType data
#' @keywords data
NULL

#' Lookup table of sampling type descriptions
#'
#' @name sampling_type_desc
#' @docType data
#' @keywords data
NULL

#' Station tables from the SMB survey handbook
#'
#' @name stationlist_hafro_smb
#' @docType data
#' @keywords data
#' @references \url{https://www.hafogvatn.is/static/research/files/rallhandbok_2025-enska-1.pdf}
NULL

#' Station tables from the SMH survey handbook - deep survey
#'
#' @name stationlist_hafro_smh_deep
#' @docType data
#' @keywords data
#' @references \url{https://www.hafogvatn.is/static/research/files/smh_manual_2025.pdf}
NULL

#'Station tables from the SMH survey handbook - shelf survey
#'
#' @name stationlist_hafro_smh_shelf
#' @docType data
#' @keywords data
#' @references \url{https://www.hafogvatn.is/static/research/files/smh_manual_2025.pdf}
NULL

#' Statistical rectangle mapping (reitmapping)
#'
#' The full gridcell (subrectangle) to division and subdivision mapping, as
#' used for the region maps of the tech reports. [gridcell] is the subset of
#' rows with a gridcell, division, subdivision and position.
#'
#' @format A data.frame with columns ``id``, ``gridcell`` (10 x rectangle +
#'   subrectangle), ``division``, ``subdivision``, ``lat`` and ``lon`` (centre
#'   of the gridcell) and ``size`` (area, square nautical miles)
#' @source ``ops$bthe."reitmapping"`` in mar, extracted 8 October 2026 by
#'   ``pax:::data_update_reitmapping()``
#' @name reitmapping
#' @docType data
#' @keywords data
NULL

#' Gear code (veidarfaeri) to MFDB gear code mapping
#'
#' The mapping of biota and landings gear codes (``veidarfaeri``, the
#' ``gear_id`` of the pax station and landings tables) to MFDB gear codes.
#'
#' @format A data.frame with columns
#'   \describe{
#'     \item{gear_id}{Gear code (``veidarfaeri``)}
#'     \item{mfdb_gear_code}{MFDB gear code, as ``biota.gear_mapping``, with
#'       gear 91 (anglerfish gillnet) added as ``GIL``}
#'     \item{source}{``"biota.gear_mapping"``, or ``"pax"`` for gear 91,
#'       which is missing from ``biota.gear_mapping``. Rows with
#'       ``source == "biota.gear_mapping"`` reproduce that table exactly}
#'     \item{mfdb_gear_code_weight}{MFDB gear code of
#'       ``ops$bthe."gear_mapping"``, used by the old cod code for the weights
#'       at length of the commercial samples}
#'   }
#' @source ``biota.gear_mapping`` and ``ops$bthe."gear_mapping"`` in mar,
#'   extracted 8 October 2026 by ``pax:::data_update_gear_mapping()``
#' @name gear_mapping
#' @docType data
#' @keywords data
NULL

#' NOAA bathymetry grid
#'
#' Ocean depth from NOAA on a regular grid of 4 x 4 minutes, 80 W - 50 E
#' and 30 N - 80 N (as the NOAA grids of ``marmap::getNOAA.bathy()``), with
#' the statistical
#' rectangle and subrectangle (gridcell) of each point. A lookup grid, used
#' e.g. to fill missing depths with the mean depth of the gridcell and for
#' depth contours on maps.
#'
#' @format A data.frame with columns ``x`` (longitude), ``y`` (latitude),
#'   ``z`` (depth, m, negative below sea level), ``reitur`` (rectangle) and
#'   ``smareitur`` (gridcell, 10 x rectangle + subrectangle)
#' @source ``ops$bthe."noaa_bathymetry"`` in mar, extracted 8 October 2026
#'   by ``pax:::data_update_noaa_bathymetry()``
#' @name noaa_bathymetry
#' @docType data
#' @keywords data
NULL

#' Diel (time of day) correction of the golden redfish survey indices
#'
#' Multipliers of the catch of golden redfish by time of the tow and length
#' group in the spring (``fleet == "smb"``) and autumn (``"smh"``) surveys,
#' fitted for the 2005 benchmark (``Surveys/Data/DielVariation_05.rdata``).
#' The survey length distributions are divided by ``scaledmult``, matched by
#' the tow start time (rounded to 0.1 h) and length group.
#'
#' @format A data.frame with columns ``fleet``, ``year`` (first year of the
#'   fit), ``time`` (tow start, hours, 0-24 by 0.1), ``length_group``,
#'   ``lengths`` (length range of the group, cm, e.g. ``"33-34"``),
#'   ``mult``, ``meanmult`` and ``scaledmult``
#' @source ``ops$krik."predressmb_05"`` and ``ops$krik."predressmh_05"`` in
#'   mar, extracted 8 October 2026 by
#'   ``pax:::data_update_diel_correction_reg()``
#' @name diel_correction_reg
#' @docType data
#' @keywords data
NULL
