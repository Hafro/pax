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
#' refitted in 2022 on the surveys up to 2021 (spring survey from 1985,
#' autumn survey from 1996), probably by
#' ``DAG/05-redfish/Surveys/R/05_ICE_Surveys_DielVariation.R``, and saved as
#' ``Surveys/Data/DielVariation_05.rdata`` ("05" is the species code). The
#' model is the one of the 2012 analysis (``BWKNORTH2023/DielVariation``):
#' for each length group a quasi-Poisson GLM of the numbers per tow on year,
#' fixed station and a periodic spline of the tow start time (7 df, period
#' 0-24 h); the prediction over the day is scaled to mean 1 (``scaledmult``).
#' A refit on the pax data to 2021 reproduces the table to 0.035 (spring
#' survey) and 0.145 (autumn survey) in ``scaledmult``.
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

#' Strata of the inshore shrimp surveys
#'
#' The fjord sub-areas (``fjardarreitur``) of the inshore shrimp surveys
#' (sampling type 37, and 10 and 11 in Isafjardardjup) and their areas, as
#' the old assessment scripts: Arnarfjordur (``advice/getData.R``, survey
#' area 52), Isafjardardjup (``Data/flatarmal_isafj.txt``, survey area 53,
#' areas by year; the 2026 assessment used sub-areas 1, 2, 3, 4, 6, 8, 11,
#' 12, 13 and 14) and Snaefellsnes (``advice/getData.r``, survey areas 31,
#' 32 and 34; the old script matched stations on the sub-area only). Use
#' with [pax_si_station_strata()].
#'
#' ``"isafjardardjup_february"`` holds the areas of the old February survey
#' script of Isafjardardjup (``Data/leidangur_februar.r``), which differ
#' slightly from the autumn ones: sub-areas 1-6 and 8 to 2004, and 2-6, 8
#' and 10-14 from 2005 (the old table listed only the years with a February
#' survey, 1988-2004, 2013, 2014, 2016, 2018 and 2022).
#'
#' @format A data.frame with columns ``stock``, ``survey_area`` (biota
#'   ``skiki``), ``survey_subarea`` (``fjardarreitur``), ``year_start`` and
#'   ``year_end`` (years the area holds for, ``NA`` for no limit), ``area``
#'   (km², as the old scripts, which divided by the swept area of a 1 nm tow
#'   in km², 0.0149 x 1.852) and ``stratum`` (the sub-area)
#' @source The old IAG repositories ``41-raekjaarnarfjordur``,
#'   ``41-raekjaisafjardardjup`` and ``41-raekjasnaefellsnes`` (MFRI GitLab),
#'   October 2026
#' @name shrimp_strata
#' @docType data
#' @keywords data
NULL

#' Strata of the offshore shrimp survey
#'
#' The 42 statistical rectangles of the offshore shrimp survey (sampling
#' type 31) with their areas and survey areas (``skiki``), as the old
#' ``41-offshore-shrimp/advice/getData.R`` (13,604 in total; 11,320 in the
#' survey areas 10-19 that the index uses). The old script divided by
#' 9.17927 for the swept area, so check the units before use.
#'
#' @format A data.frame with columns ``stock``, ``survey_area``,
#'   ``rectangle`` (``reitur``) and ``area`` (as the old script)
#' @source ``41-offshore-shrimp/advice/getData.R`` (MFRI GitLab), October
#'   2026
#' @name shrimp_offshore_strata
#' @docType data
#' @keywords data
NULL
