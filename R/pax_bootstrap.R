#' Spatial bootstrap of survey and commercial samples
#'
#' A port of the spatial bootstrap of \code{mfdb::mfdb_bootstrap_group()}, as
#' the MFRI Gadget assessments used it (e.g. blue ling, Greenland halibut and
#' beaked redfish): the subdivisions of each area group are resampled with
#' replacement, and the samples (stations) of a subdivision drawn k times
#' count k times in the replicate, so that length, age and maturity
#' distributions and survey indices computed from the resampled stations vary
#' as they would between surveys of the same design.
#'
#' Every replicate is reproducible: replicate \code{i} is determined by
#' \code{seed} and \code{i} alone. It is the \code{i}-th draw after
#' \code{set.seed(seed)} with the random number generators mfdb used
#' (Mersenne-Twister, Inversion, and by default the "Rounding" sampler), so
#' \code{pax_bootstrap_group(group, i, seed)} gives the same subdivisions as
#' \code{mfdb::mfdb_bootstrap_group(n, group, seed)[[i]]} for any \code{n >=
#' i}. The random number state of the session is restored afterwards.
#' Replicate 0 is the original group (each subdivision once), so the same code
#' gives the base data.
#'
#' @param group Named list of area groups: area name to a vector of
#'   subdivisions (e.g. \code{list("1" = unique(gridcell$subdivision))}), or
#'   an \code{mfdb::mfdb_group()}. Groups of one subdivision are not
#'   resampled, as in mfdb.
#' @param replicate Replicate numbers (1, 2, ...; 0 for the original group).
#'   \code{pax_bootstrap_resample()} takes one.
#' @param seed Seed of the bootstrap (an integer).
#' @param sample_kind Sampler of \code{set.seed()}: "Rounding" (default) gives
#'   the draws of mfdb (R < 3.6 sampler, as mfdb sets it), "Rejection" the
#'   current R default.
#' @return \subsection{pax_bootstrap_group}{List with one element per
#'   replicate, each a named list like \code{group} with the drawn
#'   subdivisions}
#' @examples
#' g <- list("1" = c(1011, 1012, 1013, 1021, 1022))
#' pax_bootstrap_group(g, 1:2, seed = 2298)
#' pax_bootstrap_table(g, 1, seed = 2298)
#' @name pax_bootstrap
#' @export
pax_bootstrap_group <- function(
  group,
  replicate,
  seed,
  sample_kind = c("Rounding", "Rejection")
) {
  sample_kind <- match.arg(sample_kind)
  group <- bootstrap_check_group(group)
  replicate <- bootstrap_check_replicate(replicate, several = TRUE)
  stopifnot(length(seed) == 1, !is.na(seed))

  n <- max(replicate)
  draws <- if (n > 0) {
    bootstrap_with_seed(seed, sample_kind, {
      lapply(seq_len(n), function(i) {
        lapply(group, function(g) {
          if (length(g) == 1) g else sample(g, replace = TRUE)
        })
      })
    })
  } else {
    list()
  }
  lapply(replicate, function(i) if (i == 0) group else draws[[i]])
}

#' @return \subsection{pax_bootstrap_table}{data.frame with columns
#'   \code{replicate}, \code{area} (group name), \code{subdivision} and
#'   \code{boot_copy}: one row for each time a subdivision was drawn (a
#'   subdivision drawn twice has rows with \code{boot_copy} 1 and 2;
#'   subdivisions not drawn have no row)}
#' @rdname pax_bootstrap
#' @export
pax_bootstrap_table <- function(
  group,
  replicate,
  seed,
  sample_kind = c("Rounding", "Rejection")
) {
  replicate <- bootstrap_check_replicate(replicate, several = TRUE)
  groups <- pax_bootstrap_group(group, replicate, seed, sample_kind)
  out <- do.call(
    rbind,
    lapply(seq_along(replicate), function(r) {
      g <- groups[[r]]
      do.call(
        rbind,
        lapply(names(g), function(a) {
          s <- g[[a]]
          if (length(s) == 0) {
            return(NULL)
          }
          data.frame(
            replicate = as.integer(replicate[[r]]),
            area = a,
            subdivision = s,
            boot_copy = stats::ave(seq_along(s), s, FUN = seq_along),
            stringsAsFactors = FALSE
          )
        })
      )
    })
  )
  out <- out[order(out$replicate, out$area, out$subdivision, out$boot_copy), ]
  rownames(out) <- NULL
  out
}

#' @param tbl Query (a pax table) or data.frame of samples, with a
#'   \code{gridcell} or a \code{subdivision} column, e.g. stations. Each row
#'   is repeated as many times as its subdivision was drawn in the replicate,
#'   and rows of subdivisions not drawn (or not in \code{group}) are dropped,
#'   so that everything joined to the result by sample counts the same number
#'   of times.
#' @param division_tbl Mapping of \code{gridcell} to \code{subdivision}
#'   (data.frame or table), used when \code{tbl} has no \code{subdivision}
#'   column. Defaults to the internal [gridcell] mapping; give mfdb's
#'   reitmapping (with lower-case column names) to use the subdivisions of
#'   the old mfdb models.
#' @param area_col Name of the column that gets the area group name, or NULL
#'   for none.
#' @return \subsection{pax_bootstrap_resample}{\code{tbl} with its rows
#'   repeated by the number of draws of their subdivision, and the columns
#'   \code{subdivision}, \code{boot_copy} and \code{area_col}. Same type as
#'   \code{tbl} (a query stays a query)}
#' @rdname pax_bootstrap
#' @export
pax_bootstrap_resample <- function(
  tbl,
  group,
  replicate,
  seed,
  division_tbl = NULL,
  area_col = "area",
  sample_kind = c("Rounding", "Rejection")
) {
  replicate <- bootstrap_check_replicate(replicate, several = FALSE)
  bt <- pax_bootstrap_table(group, replicate, seed, sample_kind)
  bt$replicate <- NULL
  if (is.null(area_col)) {
    bt$area <- NULL
  } else {
    names(bt)[names(bt) == "area"] <- area_col
    if (area_col %in% colnames(tbl)) {
      stop("tbl already has a column '", area_col, "', choose another area_col")
    }
  }
  if ("boot_copy" %in% colnames(tbl)) {
    stop("tbl already has a boot_copy column (resampled twice?)")
  }
  lazy <- inherits(tbl, "tbl_sql")

  # NSE variables
  gridcell <- NULL
  subdivision <- NULL

  out <- tbl
  if (!("subdivision" %in% colnames(tbl))) {
    if (!("gridcell" %in% colnames(tbl))) {
      stop("tbl needs a gridcell or a subdivision column")
    }
    if (lazy) {
      pcon <- dbplyr::remote_con(tbl)
      dtbl <- pax_temptbl(
        pcon,
        if (is.null(division_tbl)) "paxdat_gridcell" else division_tbl
      )
    } else {
      dtbl <- if (is.null(division_tbl)) {
        env <- new.env(parent = emptyenv())
        utils::data(list = "gridcell", package = "pax", envir = env)
        env$gridcell
      } else {
        dplyr::collect(division_tbl)
      }
    }
    dtbl <- dplyr::distinct(dplyr::select(dtbl, gridcell, subdivision))
    out <- dplyr::inner_join(out, dtbl, by = "gridcell")
  }
  if (lazy) {
    bt <- pax_temptbl(dbplyr::remote_con(tbl), bt)
    out <- dplyr::inner_join(out, bt, by = "subdivision")
  } else {
    out <- dplyr::inner_join(
      out,
      bt,
      by = "subdivision",
      relationship = "many-to-many"
    )
  }
  out
}

#' @param x Numeric vector of index values (e.g. a survey index by year) for
#'   \code{pax_bootstrap_lognormal()}, for indices that are not computed from
#'   resampled stations.
#' @param sdlog Standard deviation of the log-normal error (one value, or one
#'   per value of \code{x}); for a CV, \code{sqrt(log(1 + cv^2))}.
#' @param mean_unbiased If TRUE (default) the errors have mean 1 (log mean
#'   \code{-sdlog^2/2}), otherwise median 1.
#' @return \subsection{pax_bootstrap_lognormal}{\code{x} times seeded
#'   log-normal errors. Replicate \code{i} is determined by \code{seed},
#'   \code{i} and \code{length(x)}; replicate 0 returns \code{x} unchanged}
#' @rdname pax_bootstrap
#' @export
pax_bootstrap_lognormal <- function(
  x,
  sdlog,
  replicate,
  seed,
  mean_unbiased = TRUE
) {
  replicate <- bootstrap_check_replicate(replicate, several = FALSE)
  stopifnot(length(sdlog) %in% c(1, length(x)), all(sdlog >= 0, na.rm = TRUE))
  if (replicate == 0) {
    return(x)
  }
  n <- length(x)
  z <- bootstrap_with_seed(seed, "Rejection", stats::rnorm(n * replicate))
  z <- z[(n * (replicate - 1) + 1):(n * replicate)]
  x * exp(z * sdlog - if (mean_unbiased) sdlog^2 / 2 else 0)
}

## Helpers ---------------------------------------------------------------------

bootstrap_check_group <- function(group) {
  if (!is.list(group) || is.null(names(group)) || any(names(group) == "")) {
    stop("group should be a named list of subdivisions (or an mfdb_group)")
  }
  # mfdb_group: drop the class, keep the list of vectors
  structure(lapply(group, function(g) as.vector(g)), names = names(group))
}

bootstrap_check_replicate <- function(replicate, several = TRUE) {
  if (
    !is.numeric(replicate) ||
      length(replicate) == 0 ||
      anyNA(replicate) ||
      any(replicate < 0) ||
      any(replicate != round(replicate))
  ) {
    stop("replicate should be whole numbers >= 0 (0: the original data)")
  }
  if (!several && length(replicate) != 1) {
    stop("Give one replicate")
  }
  as.integer(replicate)
}

# Evaluate expr with set.seed(seed) and the generators mfdb used, then restore
# the session's generators and random number state
bootstrap_with_seed <- function(seed, sample_kind, expr) {
  old_kind <- RNGkind()
  had_seed <- exists(".Random.seed", envir = globalenv(), inherits = FALSE)
  if (had_seed) {
    old_seed <- get(".Random.seed", envir = globalenv(), inherits = FALSE)
  }
  on.exit({
    suppressWarnings(RNGkind(old_kind[1], old_kind[2], old_kind[3]))
    if (had_seed) {
      assign(".Random.seed", old_seed, envir = globalenv())
    } else if (exists(".Random.seed", envir = globalenv(), inherits = FALSE)) {
      rm(".Random.seed", envir = globalenv())
    }
  })
  # "Rounding" warns that it is not uniform: it is used only to repeat mfdb
  suppressWarnings(set.seed(
    seed,
    kind = "Mersenne-Twister",
    normal.kind = "Inversion",
    sample.kind = sample_kind
  ))
  force(expr)
}
