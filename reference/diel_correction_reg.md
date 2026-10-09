# Diel (time of day) correction of the golden redfish survey indices

Multipliers of the catch of golden redfish by time of the tow and length
group in the spring (`fleet == "smb"`) and autumn (`"smh"`) surveys,
refitted in 2022 on the surveys up to 2021 (spring survey from 1985,
autumn survey from 1996), probably by
`DAG/05-redfish/Surveys/R/05_ICE_Surveys_DielVariation.R`, and saved as
`Surveys/Data/DielVariation_05.rdata` ("05" is the species code). The
model is the one of the 2012 analysis (`BWKNORTH2023/DielVariation`):
for each length group a quasi-Poisson GLM of the numbers per tow on
year, fixed station and a periodic spline of the tow start time (7 df,
period 0-24 h); the prediction over the day is scaled to mean 1
(`scaledmult`). A refit on the pax data to 2021 reproduces the table to
0.035 (spring survey) and 0.145 (autumn survey) in `scaledmult`. The
survey length distributions are divided by `scaledmult`, matched by the
tow start time (rounded to 0.1 h) and length group.

## Format

A data.frame with columns `fleet`, `year` (first year of the fit),
`time` (tow start, hours, 0-24 by 0.1), `length_group`, `lengths`
(length range of the group, cm, e.g. `"33-34"`), `mult`, `meanmult` and
`scaledmult`

## Source

`ops$krik."predressmb_05"` and `ops$krik."predressmh_05"` in mar,
extracted 8 October 2026 by `pax:::data_update_diel_correction_reg()`
