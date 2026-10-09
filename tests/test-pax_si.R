if (!interactive()) {
  options(warn = 2, error = function() {
    sink(stderr())
    traceback(3)
    q(status = 1)
  })
}
library(unittest)

library(pax)

pcon <- pax_connect(":memory:")
pax_import(pcon, suppressWarnings(pax_def_strata("new_strata_spring")))


ok_group("pax_si_strata_summary/pax_si_year_summary", {
  # at_age |> head(20) |> dplyr::select(-geom, -h3_cells) |> write.csv()
  at_age <- pax:::ut_tbl(
    pcon,
    read.csv(
      text = '
"","sample_id","year","month","station","trip","sampling_type","gridcell","begin_lat","begin_lon","end_lat","end_lon","mfdb_gear_code","gear_id","tow_depth","tow_number","tow_length","tow_start","tow_end","fixed","species","length","sex","si_abund","si_biomass"
"1","387923",2012,9,6223777,"B11-2012",35,6223,66.0345,-22.5228333333333,66.0658333333333,-22.5555,"BMT",77,75,37,2,2012-09-28 00:01:00,2012-09-28 00:33:00,0,2,27,1,0.435764705882353,0.0826639199936721
"2","387917",2012,9,6233177,"B11-2012",35,6233,66.207,-23.85,66.1651666666667,-23.9545,"BMT",77,47,31,3,2012-09-27 01:27:00,2012-09-27 02:15:00,0,2,51,NA,1.40096702317291,1.82077613385927
"3","387946",2012,9,5713577,"B11-2012",35,5713,65.6673333333333,-21.5395,65.6875,-21.5993333333333,"BMT",77,131,35,2,2012-09-30 09:27:00,2012-09-30 09:59:00,0,2,55,NA,5.74540909090909,9.38372948039432
"4","387931",2012,9,6710277,"B11-2012",35,6712,66.8856666666667,-21.1676666666667,66.893,-21.041,"BMT",77,128,2,3,2012-09-28 22:32:00,2012-09-28 23:20:00,0,2,45,NA,0.616493709622577,0.548628127254323
"5","387931",2012,9,6710277,"B11-2012",35,6712,66.8856666666667,-21.1676666666667,66.893,-21.041,"BMT",77,128,2,3,2012-09-28 22:32:00,2012-09-28 23:20:00,0,2,46,NA,0.205497903207526,0.195451814121685
"6","387912",2012,9,6730377,"B11-2012",35,6732,66.9413333333333,-23.4848333333333,66.8965,-23.4325,"BMT",77,220,3,3,2012-09-26 09:25:00,2012-09-26 10:11:00,0,2,45,NA,0.0363137254901961,0.0323161954426278
"7","390003",2012,10,5133577,"B11-2012",35,5133,65.1978333333333,-13.748,65.2018333333333,-13.6696666666667,"BMT",77,89,35,2,2012-10-08 02:14:00,2012-10-08 02:46:00,0,2,56,NA,0.163411764705882,0.281848868721426
"8","390003",2012,10,5133577,"B11-2012",35,5133,65.1978333333333,-13.748,65.2018333333333,-13.6696666666667,"BMT",77,89,35,2,2012-10-08 02:14:00,2012-10-08 02:46:00,0,2,58,NA,0.217882352941176,0.417896939158508
"9","390003",2012,10,5133577,"B11-2012",35,5133,65.1978333333333,-13.748,65.2018333333333,-13.6696666666667,"BMT",77,89,35,2,2012-10-08 02:14:00,2012-10-08 02:46:00,0,2,66,NA,0.163411764705882,0.463372225827701
"10","390366",2012,9,6731377,"B11-2012",35,6734,66.7486666666667,-23.229,66.7366666666667,-23.3523333333333,"BMT",77,116,13,3,2012-09-26 19:48:00,2012-09-26 20:35:00,0,2,14,NA,0.326823529411765,0.00849749711296102
"11","390366",2012,9,6731377,"B11-2012",35,6734,66.7486666666667,-23.229,66.7366666666667,-23.3523333333333,"BMT",77,116,13,3,2012-09-26 19:48:00,2012-09-26 20:35:00,0,2,44,NA,0.0726274509803922,0.0603834917195425
"12","390366",2012,9,6731377,"B11-2012",35,6734,66.7486666666667,-23.229,66.7366666666667,-23.3523333333333,"BMT",77,116,13,3,2012-09-26 19:48:00,2012-09-26 20:35:00,0,2,53,NA,0.108941176470588,0.159063288621199
"13","389967",2012,10,6163377,"B11-2012",35,6161,66.377,-16.7256666666667,66.4106666666667,-16.8183333333333,"BMT",77,194,33,3,2012-10-04 05:30:00,2012-10-04 06:18:00,0,2,23,NA,0.0363137254901961,0.00424058218114049
"14","389967",2012,10,6163377,"B11-2012",35,6161,66.377,-16.7256666666667,66.4106666666667,-16.8183333333333,"BMT",77,194,33,3,2012-10-04 05:30:00,2012-10-04 06:18:00,0,2,37,NA,0.0726274509803922,0.0357452734221026
"15","387957",2012,10,6181177,"B11-2012",35,6182,66.3908333333333,-18.4493333333333,66.3403333333333,-18.4533333333333,"BMT",77,141,11,3,2012-10-02 00:46:00,2012-10-02 01:33:00,0,2,36,2,0.0363137254901961,0.0164506523645344
"16","389977",2012,10,6640277,"B11-2012",35,6643,66.5475,-14.8026666666667,66.5321666666667,-14.6816666666667,"BMT",77,156,2,3,2012-10-05 06:09:00,2012-10-05 06:58:00,0,2,43,NA,0.399450980392157,0.309791828335549
"17","390028",2012,10,3713177,"B11-2012",35,3714,63.6936666666667,-21.4255,63.692,-21.5378333333333,"BMT",77,91,31,3,2012-10-11 09:18:00,2012-10-11 10:03:00,0,2,54,NA,0.217882352941176,0.336638384008523
"18","387932",2012,9,6700477,"B11-2012",35,6701,66.9838333333333,-20.8126666666667,66.983,-20.6891666666667,"BMT",77,191,4,3,2012-09-29 00:25:00,2012-09-29 01:15:00,0,2,46,NA,0.0363137254901961,0.0345384717497973
"19","390011",2012,10,4143677,"B11-2012",35,4142,64.3113333333333,-14.248,64.2788333333333,-14.2536666666667,"BMT",77,90,36,2,2012-10-09 14:15:00,2012-10-09 14:45:00,0,2,11,NA,0.0544705882352941,0.000682691067137736
"20","390011",2012,10,4143677,"B11-2012",35,4142,64.3113333333333,-14.248,64.2788333333333,-14.2536666666667,"BMT",77,90,36,2,2012-10-09 14:15:00,2012-10-09 14:45:00,0,2,44,NA,0.0544705882352941,0.0452876187896569
'
    )
  )
  # at_age |> pax::pax_si_scale_by_strata("new_strata_spring") |> pax::pax_si_strata_summary() |> pax:::ut_as_sort_df() |> write.csv(row.names = FALSE)
  out <- at_age |>
    pax::pax_si_scale_by_strata("new_strata_spring") |>
    pax::pax_si_strata_summary()
  ok(
    ut_cmp_equal(
      pax:::ut_as_sort_df(out),
      read.csv(
        text = '
"species","year","stratum","sampling_type","area","si_N","si_abund","si_abund_sd","si_biomass","si_biomass_sd"
2,2012,3,35,2225.73746666103,1,484.948916265437,NA,749.268664003993,NA
2,2012,8,35,3872.4142305464,1,421.865362057172,NA,178.016082072079,NA
2,2012,18,35,2484.92471399387,1,90.2368739277774,NA,80.3033127176418,NA
2,2012,32,35,2275.77241561081,2,165.283549557303,116.873118710551,64.2183240332338,37.873181074979
2,2012,33,35,1273.79228165641,1,7318.45775495856,NA,11952.922185278,NA
2,2012,34,35,154.523275287005,1,67.3357896074196,NA,12.7734996654852,NA
2,2012,35,35,2088.82544450427,1,2926.37556491498,NA,3803.28351715136,NA
2,2012,39,35,5983.9902773192,1,2390.31078293229,NA,1853.79128875286,NA
2,2012,40,35,3587.91953208505,3,1634.53354580632,1419.00018065999,1203.8219792661,1316.04683540872
2,2012,41,35,216.183813669122,1,117.756594975063,NA,251.447292274247,NA
'
      ),
      tolerance = 1e-6
    ),
    "pax_si_strata_summary: Matches baseline"
  )

  out <- out |> pax::pax_si_year_summary()
  # out |> pax:::ut_as_sort_df() |> write.csv(row.names = FALSE)
  ok(
    ut_cmp_equal(
      pax:::ut_as_sort_df(out),
      read.csv(
        text = '
"species","sampling_type","year","si_N","si_abund","si_abund_cv","si_biomass","si_biomass_cv"
2,35,2012,10,15617.1047350023,1.94233123180338,20149.846145215,2.51310977740264
'
      ),
      tolerance = 1e-6
    ),
    "pax_si_strata_summary: Matches baseline"
  )
})

ok_group("pax_si_scale_by_strata: strata from a fixed station list", {
  by_pos <- at_age |>
    pax::pax_si_scale_by_strata("new_strata_spring") |>
    dplyr::collect()
  # Station list with the strata the positions gave, but one station moved
  ss <- by_pos |>
    dplyr::ungroup() |>
    dplyr::distinct(sampling_type, station, stratum) |>
    as.data.frame()
  ss$stratum[ss$station == 6223777] <- 8
  by_list <- at_age |>
    pax::pax_si_scale_by_strata("new_strata_spring", strata_stations = ss) |>
    dplyr::collect()
  ok(
    ut_cmp_equal(
      by_list |>
        dplyr::ungroup() |>
        dplyr::distinct(station, stratum) |>
        dplyr::arrange(station) |>
        as.data.frame(),
      ss |> dplyr::select(station, stratum) |> dplyr::arrange(station)
    ),
    "Stations get the stratum of the list"
  )
  s8 <- by_list |> dplyr::filter(stratum == 8)
  s8_in <- at_age |>
    dplyr::filter(station %in% local(unique(s8$station))) |>
    dplyr::collect()
  ok(
    ut_cmp_equal(
      sort(s8$si_abund),
      sort(s8_in$si_abund * s8$area[[1]] / 2)
    ),
    "Stratum 8 now holds 2 stations, scaled by area / 2"
  )
  no_st <- at_age |>
    pax::pax_si_scale_by_strata(
      "new_strata_spring",
      strata_stations = ss[ss$station != 6223777, ]
    ) |>
    dplyr::filter(station == 6223777) |>
    dplyr::collect()
  ok(
    ut_cmp_equal(no_st$stratum, NA_real_) && ut_cmp_equal(no_st$area, NA_real_),
    "Stations not in the list get no stratum, as stations outside the strata"
  )
})

ok_group("pax_si_scale_by_landings: landings with unknown month", {
  si <- pax:::ut_tbl(
    pcon,
    data.frame(
      species = 1,
      year = 2000,
      tgroup = c("t1", "t2"),
      gear_name = "BMT",
      region = "all",
      si_abund = 1,
      si_biomass = 2
    )
  )
  landings <- pax:::ut_tbl(
    pcon,
    data.frame(
      species = 1,
      year = 2000,
      month = c(3, 9, NA),
      mfdb_gear_code = c("BMT", "BMT", NA),
      catch = c(1000, 3000, 5000)
    )
  )
  msg <- NULL
  out <- withCallingHandlers(
    pax_si_scale_by_landings(si, 1, landings_tbl = landings) |>
      dplyr::arrange(tgroup) |>
      dplyr::collect(),
    message = function(m) {
      msg <<- c(msg, conditionMessage(m))
      invokeRestart("muffleMessage")
    }
  )
  ok(
    ut_cmp_equal(out$si_biomass, c(1000, 3000)),
    "Default: landings with unknown month left out"
  )
  ok(any(grepl("unknown month \\(5 t\\)", msg)), "...with a message")
  out <- pax_si_scale_by_landings(
    si,
    1,
    landings_tbl = landings,
    month_na = 6,
    gear_na = "BMT"
  ) |>
    dplyr::arrange(tgroup) |>
    dplyr::collect()
  ok(
    ut_cmp_equal(out$si_biomass, c(6000, 3000)),
    "month_na = 6, gear_na = 'BMT': in t1 bottom trawl"
  )
})

ok_group("pax_si_scale_by_landings: landings with gears outside the groups", {
  si <- pax:::ut_tbl(
    pcon,
    data.frame(
      species = 1,
      year = 2000,
      tgroup = "t1",
      gear_name = c("BMT", "Other"),
      region = "all",
      si_abund = 1,
      si_biomass = 2
    )
  )
  landings <- pax:::ut_tbl(
    pcon,
    data.frame(
      species = 1,
      year = 2000,
      month = 3,
      mfdb_gear_code = c("BMT", "GIL"),
      catch = c(1000, 7000)
    )
  )
  msg <- NULL
  out <- withCallingHandlers(
    pax_si_scale_by_landings(
      si,
      1,
      landings_tbl = landings,
      gear_group = list(BMT = "BMT"),
      tgroup = list(t1 = 1:12)
    ) |>
      dplyr::collect(),
    message = function(m) {
      msg <<- c(msg, conditionMessage(m))
      invokeRestart("muffleMessage")
    }
  )
  ok(
    any(grepl("outside the gear groups \\(7 t\\)", msg)),
    "Message with the landings left out"
  )
  msg <- NULL
  out <- withCallingHandlers(
    pax_si_scale_by_landings(
      si,
      1,
      landings_tbl = landings,
      gear_group = list(BMT = "BMT", Other = pax_add_other()),
      tgroup = list(t1 = 1:12)
    ) |>
      dplyr::arrange(gear_name) |>
      dplyr::collect(),
    message = function(m) {
      msg <<- c(msg, conditionMessage(m))
      invokeRestart("muffleMessage")
    }
  )
  ok(!any(grepl("outside the gear groups", msg)), "No message with a default group")
  ok(ut_cmp_equal(out$si_biomass, c(1000, 7000)), "Other gears in the default group")
})


ok_group("pax_si_scale_winsorize", {
  # Two years, one species; stations with two length rows each, plus one
  # empty station (biomass 0), which takes no part in the quantile
  st <- data.frame(
    year = rep(c(2000, 2001), each = 10),
    sample_id = 1:20,
    species = 1,
    b = c(1:9, 100, 11:19, 1000)
  )
  rows <- rbind(
    transform(st, length = 10, si_biomass = b / 4, si_abund = b / 2),
    transform(st, length = 20, si_biomass = 3 * b / 4, si_abund = b / 2)
  )
  rows <- rbind(rows, data.frame(
    year = 2000, sample_id = 21, species = 1, b = 0,
    length = 0, si_biomass = 0, si_abund = 0
  ))
  out <- pax:::ut_tbl(pcon, rows[, names(rows) != "b"]) |>
    pax_si_scale_winsorize(q = 0.9) |>
    dplyr::group_by(year, sample_id) |>
    dplyr::summarise(
      si_biomass = sum(si_biomass, na.rm = TRUE),
      si_abund = sum(si_abund, na.rm = TRUE), .groups = "drop"
    ) |>
    dplyr::arrange(sample_id) |>
    as.data.frame()

  q <- tapply(st$b, st$year, stats::quantile, probs = 0.9)
  sc <- pmin(1, q[as.character(st$year)] / st$b)
  ok(ut_cmp_equal(out$si_biomass[1:20], unname(st$b * sc)),
     "Stations above the 0.9 quantile of their year are scaled down to it")
  ok(ut_cmp_equal(out$si_abund[1:20], unname(st$b * sc)),
     "si_abund is scaled by the same factor as si_biomass")
  ok(ut_cmp_equal(out$si_biomass[c(10, 20)], as.vector(q)),
     "The largest station of each year now has the quantile as biomass")
  ok(ut_cmp_equal(out$si_biomass[-c(10, 20, 21)], st$b[-c(10, 20)]),
     "Stations below the quantile are unchanged")
  ok(ut_cmp_equal(out$si_biomass[21], 0), "Empty station is kept, unchanged")
})
