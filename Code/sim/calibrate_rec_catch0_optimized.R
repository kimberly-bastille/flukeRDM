################################################################################
################################################################################
# Script:       calibrate_rec_catch0_optimized.R
# Purpose:      Calibration PASS 0 - simulate the calibration year exactly as
#               the regulations were written, with NO adjustment for illegal
#               harvest or voluntary release, and measure how far the result
#               lands from MRIP. For each state x mode x draw it expands each
#               simulated trip's catch to individual fish, draws each fish a
#               length from the baseline catch-at-length distribution, applies
#               the minimum size and bag limit to classify it kept or
#               released, computes trip utility and the probability the trip
#               is taken, expands to population totals, and tabulates
#               model-vs-MRIP differences by species and disposition.
#               The gaps it measures are the input to PASS 1
#               (calibrate_rec_catch1_final.R), which reallocates harvest and
#               discards until those gaps close.
# Inputs:       simulated_catch_totals.dta, baseline_catch_at_length.csv,
#               calib_catch_draws_<ST>_<i>.fst
# Outputs:      calibration_comparison.fst
# Dependencies: The objects input_data_cd, iterative_input_data_cd and
#               n_simulations must already exist in the calling environment -
#               they are set by "R code wrapper.R", which sources this file as
#               STEP 1. Packages data.table, arrow, readr, haven.
# Pipeline:     First of the three R calibration/projection steps. Its sibling
#               calibrate_rec_catch1_final.R reuses the same modeling logic
#               with reallocation added.
# Dev paths:    1 hardcoded absolute path to a developer's local machine
#               (E:\), at line 308.
#
# Why "optimized": an earlier version (Code/archive/calibrate_rec_catch0.R)
# performed the same computation but re-read the catch-at-length file inside
# the innermost loop. This version preloads it once and leans on data.table
# grouped operations. The fish-level expansion was deliberately RETAINED -
# bag limits bind on individual fish in size order, so they cannot be applied
# to trip-level totals without changing the answer.
#
# NOTE - preference coefficients are hardcoded here, not read from Stata.
# The beta_* values in the simulation loop are literal numbers (e.g.
# beta_sqrt_sf_keep mean 0.827, sd 1.267) rather than reads from
# preference_params.dta, the file estimate_angler_preferences.do exists to
# produce. Two consequences worth understanding: (1) the same coefficient
# means are used on every draw, so sampling uncertainty in the estimated
# preferences is NOT propagated through the calibration, only heterogeneity
# across simulated anglers is; (2) if the choice model is ever re-estimated,
# these literals must be updated by hand or the calibration will silently
# keep using the old preferences. The sd = 0 entries correspond exactly to
# the parameters estimate_angler_preferences.do zeroes out as insignificant.
################################################################################
################################################################################

# calibration-year trip simulation WITHOUT any adjustments for illegal harvest or voluntary release
# rewritten for speed/efficiency while RETAINING fish-level expansion

library(data.table)
library(arrow)
library(readr)
library(haven)

################################################################################
################################################################################
# Section A: Helper functions
################################################################################
################################################################################

#' @title Divide, returning NA instead of Inf on a zero denominator
#' @description Used for the reallocation proportions at the end of
#'   build_compare_table(). A stratum can legitimately have zero modeled
#'   releases or zero modeled harvest, and the resulting Inf would propagate
#'   into the reallocation step as a nonsensical instruction. NA marks the
#'   stratum as "no reallocation possible" instead.
#' @param num Numerator.
#' @param den Denominator; zero or NA yields NA.
#' @return Numeric vector of quotients, NA wherever the denominator was zero
#'   or missing.
safe_divide <- function(num, den) {
  ifelse(is.na(den) | den == 0, NA_real_, num / den)
}

#' @title Probability of choosing the trip over opting out
#' @description Binary logit probability, computed in a numerically stable
#'   way. The naive form 1/(1+exp(-z)) overflows to Inf for large negative z,
#'   which matters here because opt-out utilities can be far from trip
#'   utilities for unattractive regulation scenarios. The function therefore
#'   branches: for z >= 0 it uses 1/(1+exp(-z)), and for z < 0 it uses
#'   exp(z)/(1+exp(z)). Both are algebraically identical to the logit but each
#'   only ever exponentiates a non-positive number, so neither can overflow.
#' @param v_trip Deterministic utility of taking the trip.
#' @param v_optout Deterministic utility of the opt-out (no-trip) alternative.
#' @return Numeric vector of probabilities in [0, 1] that the trip is taken.
# stable binary logit probability for the trip alternative
calc_prob_trip <- function(v_trip, v_optout) {
  z <- v_trip - v_optout
  out <- numeric(length(z))
  pos <- z >= 0
  out[pos] <- 1 / (1 + exp(-z[pos]))
  ez <- exp(z[!pos])
  out[!pos] <- ez / (1 + ez)
  out
}

#' @title Apply bag and size limits to one species, fish by fish
#' @description Turns a trip-level catch count into kept and released counts
#'   by simulating individual fish. Each trip's catch is expanded to one row
#'   per fish, each fish is assigned a length drawn from the baseline
#'   catch-at-length distribution, and fish at or above the minimum size are
#'   marked legally keepable. The bag limit is then applied in encounter
#'   order via a running count of keepable fish: the first `bag` keepable fish
#'   are kept and everything after is released.
#'
#'   Fish-level expansion is the reason this function exists rather than a
#'   trip-level formula. Whether a given fish is kept depends on how many
#'   keepable fish preceded it on that trip, which cannot be recovered from
#'   trip totals. Trips with zero catch skip the expansion entirely and are
#'   rejoined at the end, which keeps the expanded table as small as possible.
#' @param catch_dt Trip-level data.table with one row per
#'   date x mode x tripid x catch_draw, carrying catch counts and the
#'   regulations in force.
#' @param catch_col Name of the column holding this species' catch count.
#' @param bag_col Name of the column holding the bag limit in force.
#' @param min_col Name of the column holding the minimum size in force.
#' @param size_dt Catch-at-length lookup with columns `length` and
#'   `fitted_prob`, giving the sampling distribution of fish lengths.
#' @param species_prefix One of "sf", "bsb" or "scup"; determines the names of
#'   the two output columns (tot_keep_<prefix>_new, tot_rel_<prefix>_new).
#' @return A data.table keyed on date, mode, tripid and catch_draw with kept
#'   and released counts for this species, covering both the zero-catch and
#'   positive-catch trips.
#' @examples
#' \dontrun{
#' simulate_species(catch_dt, "sf_cat", "fluke_bag", "fluke_min",
#'                  size_lookup, "sf")
#' }
simulate_species <- function(catch_dt,
                             catch_col,
                             bag_col,
                             min_col,
                             size_dt,
                             species_prefix = c("sf", "bsb", "scup")) {

  species_prefix <- match.arg(species_prefix)

  keep_col <- paste0("tot_keep_", species_prefix, "_new")
  rel_col  <- paste0("tot_rel_",  species_prefix, "_new")

  key_cols <- c("date", "mode", "tripid", "catch_draw")

  pos_dt <- catch_dt[get(catch_col) > 0,
                     .(date, mode, tripid, catch_draw,
                       catch_n = get(catch_col),
                       bag     = get(bag_col),
                       min_sz  = get(min_col))]

  zero_dt <- catch_dt[get(catch_col) == 0,
                      .(date, mode, tripid, catch_draw)]

  trip_out_zero <- copy(zero_dt)
  trip_out_zero[, c(keep_col, rel_col) := .(0L, 0L)]

  if (nrow(pos_dt) == 0L) {
    setcolorder(trip_out_zero, c(key_cols, keep_col, rel_col))
    setkeyv(trip_out_zero, key_cols)
    return(trip_out_zero)
  }

  fish_dt <- pos_dt[rep(seq_len(.N), catch_n)]
  fish_dt[, fishid := seq_len(.N)]

  fish_dt[, fitted_length := sample(size_dt$length,
                                    .N,
                                    replace = TRUE,
                                    prob = size_dt$fitted_prob)]

  fish_dt[, posskeep := fifelse(fitted_length >= min_sz, 1L, 0L)]

  setorder(fish_dt, date, mode, tripid, catch_draw, fishid)
  fish_dt[, csum_keep := cumsum(posskeep), by = key_cols]
  # csum_keep counts keepable fish encountered so far on this trip, so
  # csum_keep <= bag is what enforces the bag limit in encounter order. The
  # bag > 0 test handles closed seasons, where the bag is 0 and nothing may be
  # kept regardless of size.
  fish_dt[, keep := fifelse(bag > 0 & posskeep == 1L & csum_keep <= bag, 1L, 0L)]
  fish_dt[, release := fifelse(keep == 0L, 1L, 0L)]

  trip_out_pos <- fish_dt[, .(
    keep_n = sum(keep),
    rel_n  = sum(release)
  ), by = key_cols]

  setnames(trip_out_pos, c("keep_n", "rel_n"), c(keep_col, rel_col))

  trip_out <- rbindlist(list(trip_out_pos, trip_out_zero), use.names = TRUE, fill = TRUE)
  setkeyv(trip_out, key_cols)
  trip_out[]
}

#' @title Tabulate model-vs-MRIP differences and the implied reallocation
#' @description Reshapes the simulated totals and the MRIP estimates to a
#'   common long form, joins them on mode x species x disposition, and
#'   computes absolute and percent differences. It then derives what PASS 1
#'   needs in order to close the gap: a direction flag and a proportion.
#'
#'   The direction logic reads as follows. If the model harvests LESS than
#'   MRIP (diff_keep < 0), some fish the model released must in reality have
#'   been kept, so rel_to_keep is flagged and p_rel_to_keep gives the share of
#'   modeled releases that would have to be converted. If the model harvests
#'   MORE than MRIP (diff_keep > 0), the reverse: keep_to_rel is flagged and
#'   p_keep_to_rel gives the share of modeled harvest to convert. These
#'   proportions are the "illegal harvest / voluntary release" adjustments
#'   that PASS 0 deliberately omits.
#' @param summed_results Population-expanded simulated totals for one draw,
#'   with one row per mode and columns like sf_keep, sf_rel, sf_catch.
#' @param MRIP_comparison_draw The MRIP survey totals for the same stratum,
#'   with matching column names.
#' @param md The mode label to stamp on the result.
#' @return A data.table with one row per mode x species carrying the MRIP
#'   value, model value, difference and percent difference for each of keep,
#'   release and catch, plus the reallocation direction flags and proportions.
build_compare_table <- function(summed_results, MRIP_comparison_draw, md) {

  metric_cols <- c(
    "sf_keep", "sf_rel", "sf_catch",
    "bsb_keep", "bsb_rel", "bsb_catch",
    "scup_keep", "scup_rel", "scup_catch"
  )

  model_metrics  <- intersect(metric_cols, names(summed_results))
  mrip_metrics   <- intersect(metric_cols, names(MRIP_comparison_draw))
  common_metrics <- intersect(model_metrics, mrip_metrics)

  if (length(common_metrics) == 0L) {
    stop("No common metric columns found between summed_results and MRIP_comparison_draw.")
  }

  model_long <- melt(
    as.data.table(summed_results)[, c("mode", common_metrics), with = FALSE],
    id.vars = "mode",
    measure.vars = common_metrics,
    variable.name = "metric",
    value.name = "model"
  )

  mrip_long <- melt(
    as.data.table(MRIP_comparison_draw)[, c("mode", common_metrics), with = FALSE],
    id.vars = "mode",
    measure.vars = common_metrics,
    variable.name = "metric",
    value.name = "MRIP"
  )

  model_long[, model := as.numeric(model)]
  mrip_long[, MRIP := as.numeric(MRIP)]

  cmp <- merge(model_long, mrip_long, by = c("mode", "metric"), all = FALSE)
  cmp[, c("species", "disposition") := tstrsplit(metric, "_", fixed = TRUE, keep = 1:2)]
  cmp[, diff := model - MRIP]
  cmp[, pct_diff := fifelse(MRIP != 0, 100 * diff / MRIP, NA_real_)]
  cmp[, abs_diff_val := abs(diff)]
  cmp[, abs_pct_diff_val := fifelse(MRIP != 0, abs(100 * diff / MRIP), NA_real_)]
  cmp[, mode := md]

  cmp <- cmp[
    species %in% c("sf", "bsb", "scup") &
      disposition %in% c("keep", "rel", "catch"),
    .(species, disposition, mode, MRIP, model, diff, pct_diff,
      abs_diff_val, abs_pct_diff_val)
  ]

  compare_k <- cmp[disposition == "keep",
                   .(mode, species,
                     MRIP_keep = MRIP,
                     model_keep = model,
                     diff_keep = diff,
                     pct_diff_keep = pct_diff)]

  compare_c <- cmp[disposition == "catch",
                   .(mode, species,
                     MRIP_catch = MRIP,
                     model_catch = model,
                     diff_catch = diff,
                     pct_diff_catch = pct_diff)]

  compare_r <- cmp[disposition == "rel",
                   .(mode, species,
                     MRIP_rel = MRIP,
                     model_rel = model,
                     diff_rel = diff,
                     pct_diff_rel = pct_diff)]

  out <- merge(compare_r, compare_k, by = c("mode", "species"), all = TRUE)
  out <- merge(out, compare_c, by = c("mode", "species"), all = TRUE)

  out[, rel_to_keep := fifelse(diff_keep < 0, 1, 0)]
  out[, keep_to_rel := fifelse(diff_keep > 0, 1, 0)]
  out[, p_rel_to_keep := abs(safe_divide(diff_keep, model_rel))]
  out[, p_keep_to_rel := abs(safe_divide(diff_keep, model_keep))]

  out[]
}

################################################################################
################################################################################
# Section B: Load MRIP targets and catch-at-length, then simulate
################################################################################
################################################################################

message("calibrate_rec_catch0_optimized.R: starting calibration pass 0 over 9 states x 3 modes x ", n_simulations, " draws. This is a long-running step.")

# Hardcoded absolute path, unlike the rest of this script, which resolves paths
# through input_data_cd / iterative_input_data_cd.
MRIP_comparison <- read_dta("E:/Lou_projects/flukeRDM/flukeRDM_iterative_data/archive/calib_catch_draws/simulated_catch_totals.dta") |>
  as.data.table()

setnames(
  MRIP_comparison,
  old = c("tot_dtrip_sim", "tot_sf_cat_sim", "tot_bsb_cat_sim", "tot_scup_cat_sim",
          "tot_sf_keep_sim", "tot_bsb_keep_sim", "tot_scup_keep_sim",
          "tot_sf_rel_sim", "tot_bsb_rel_sim", "tot_scup_rel_sim"),
  new = c("estimated_trips", "sf_catch", "bsb_catch", "scup_catch",
          "sf_keep", "bsb_keep", "scup_keep",
          "sf_rel", "bsb_rel", "scup_rel"),
  skip_absent = TRUE
)

states <- c("MA", "RI", "CT", "NY", "NJ", "DE", "MD", "VA", "NC")
mode_draw <- c("sh", "pr", "fh")
draws <- 1:n_simulations

# preload catch-at-length once instead of reading inside the innermost loop
size_lookup_raw <- as.data.table(
  read_csv(file.path(input_data_cd, "baseline_catch_at_length.csv"),
           show_col_types = FALSE)
)

size_lookup_raw <- size_lookup_raw[
  !is.na(fitted_prob),
  .(state, draw, species, fitted_prob, length)
]
setkey(size_lookup_raw, state, draw, species)

calib_comparison <- vector("list", length(states) * length(mode_draw) * length(draws))
k <- 1L

# s<-"MA"
# md<-"pr"
# i<-1

for (s in states) {

  dtrip_state <- as.data.table(
    read_fst(file.path(
      iterative_input_data_cd,
      paste0("archive/directed_trips_calibration/directed_trips_calibration_", s, ".fst")
    ))
  )

  catch_state <- NULL

  for (i in draws) {

    dtrip_draw <- dtrip_state[
      draw == i,
      .(mode, date, dtrip, bsb_bag, bsb_min, fluke_bag, fluke_min, scup_bag, scup_min)
    ]

    catch_draw_dt <- as.data.table(
      read_fst(file.path(
        iterative_input_data_cd,
        paste0("archive/calib_catch_draws/calib_catch_draws_", s, "_", i, ".fst")
      ))
    )

    sf_size_data   <- size_lookup_raw[list(s, i, "sf"),   .(fitted_prob, length)]
    bsb_size_data  <- size_lookup_raw[list(s, i, "bsb"),  .(fitted_prob, length)]
    scup_size_data <- size_lookup_raw[list(s, i, "scup"), .(fitted_prob, length)]

    for (md in mode_draw) {

      dtripz <- dtrip_draw[mode == md]
      catch_data <- merge(
        catch_draw_dt[mode == md],
        dtripz,
        by = c("mode", "date"),
        all.x = TRUE
      )

      if (nrow(catch_data) == 0L) {
        MRIP_comparison_draw <- MRIP_comparison[
          draw == i & state == s & mode == md,
          .(mode, sf_keep, sf_rel, sf_catch,
            bsb_keep, bsb_rel, bsb_catch,
            scup_keep, scup_rel, scup_catch)
        ]

        if (nrow(MRIP_comparison_draw) == 0L) {
          MRIP_comparison_draw <- data.table(
            mode = md,
            sf_keep = NA_real_, sf_rel = NA_real_, sf_catch = NA_real_,
            bsb_keep = NA_real_, bsb_rel = NA_real_, bsb_catch = NA_real_,
            scup_keep = NA_real_, scup_rel = NA_real_, scup_catch = NA_real_
          )
        }

        summed_results <- data.table(
          mode = md,
          sf_catch = 0, sf_keep = 0, sf_rel = 0,
          bsb_catch = 0, bsb_keep = 0, bsb_rel = 0,
          scup_catch = 0, scup_keep = 0, scup_rel = 0,
          estimated_trips = 0, n_choice_occasions = 0
        )

        compare_out <- build_compare_table(summed_results, MRIP_comparison_draw, md)
        compare_out[, `:=`(draw = i, state = s)]
        calib_comparison[[k]] <- compare_out
        k <- k + 1L
        next
      }

      angler_dems <- unique(
        catch_data[, .(date, mode, tripid, total_trips_12, age, cost)]
      )

      sf_trip_data <- simulate_species(
        catch_dt = catch_data,
        catch_col = "sf_cat",
        bag_col   = "fluke_bag",
        min_col   = "fluke_min",
        size_dt   = sf_size_data,
        species_prefix = "sf"
      )
      # catch_dt = catch_data
      # catch_col = "sf_cat"
      # bag_col   = "fluke_bag"
      # min_col   = "fluke_min"
      # size_dt   = sf_size_data
      # species_prefix = "sf"
      
      bsb_trip_data <- simulate_species(
        catch_dt = catch_data,
        catch_col = "bsb_cat",
        bag_col   = "bsb_bag",
        min_col   = "bsb_min",
        size_dt   = bsb_size_data,
        species_prefix = "bsb"
      )

      scup_trip_data <- simulate_species(
        catch_dt = catch_data,
        catch_col = "scup_cat",
        bag_col   = "scup_bag",
        min_col   = "scup_min",
        size_dt   = scup_size_data,
        species_prefix = "scup"
      )

      key_cols <- c("date", "mode", "tripid", "catch_draw")
      setkeyv(sf_trip_data, key_cols)
      setkeyv(bsb_trip_data, key_cols)
      setkeyv(scup_trip_data, key_cols)

      trip_data <- merge(sf_trip_data, bsb_trip_data, by = key_cols, all = TRUE)
      trip_data <- merge(trip_data, scup_trip_data, by = key_cols, all = TRUE)

      zero_fill_cols <- intersect(
        c("tot_keep_sf_new", "tot_rel_sf_new",
          "tot_keep_bsb_new", "tot_rel_bsb_new",
          "tot_keep_scup_new", "tot_rel_scup_new"),
        names(trip_data)
      )
      for (cc in zero_fill_cols) {
        set(trip_data, which(is.na(trip_data[[cc]])), cc, 0L)
      }

      trip_data[, `:=`(
        tot_scup_catch = tot_keep_scup_new + tot_rel_scup_new,
        tot_bsb_catch  = tot_keep_bsb_new + tot_rel_bsb_new,
        tot_sf_catch   = tot_keep_sf_new + tot_rel_sf_new
      )]

      parameters <- unique(trip_data[, .(date, mode, tripid)])

      # These coefficient means and standard deviations are the fitted mixed
      # logit results, transcribed as literals rather than read from
      # preference_params.dta. See the note in the file header: the same values
      # are used on every draw, so preference SAMPLING uncertainty is not
      # propagated here, only across-angler heterogeneity within a draw. The
      # sd = 0 entries are the parameters whose estimated dispersion was not
      # significant at the 10% level.
      parameters[, `:=`(
        beta_sqrt_sf_keep     = rnorm(.N, mean = 0.827, sd = 1.267),
        beta_sqrt_sf_release  = rnorm(.N, mean = 0.065, sd = 0.325),
        beta_sqrt_bsb_keep    = rnorm(.N, mean = 0.353, sd = 0.129),
        beta_sqrt_bsb_release = rnorm(.N, mean = 0.074, sd = 0),
        beta_sqrt_sf_bsb_keep = rnorm(.N, mean = -0.056, sd = 0.196),
        beta_sqrt_scup_catch  = rnorm(.N, mean = 0.018, sd = 0),
        beta_opt_out          = rnorm(.N, mean = -2.056, sd = 1.977),
        beta_opt_out_avidity  = rnorm(.N, mean = -0.010, sd = 0),
        beta_opt_out_age      = rnorm(.N, mean = 0.010, sd = 0),
        beta_cost             = -0.012
      )]

      setkey(parameters, date, mode, tripid)
      setkey(angler_dems, date, mode, tripid)
      trip_data <- merge(trip_data, parameters, by = c("date", "mode", "tripid"), all.x = TRUE)
      trip_data <- merge(trip_data, angler_dems, by = c("date", "mode", "tripid"), all.x = TRUE)

      setorder(trip_data, date, mode, tripid, catch_draw)

      # The utility specification. Catch enters as square roots, which imposes
      # diminishing marginal utility - the second fish is worth less than the
      # first. The sf x bsb interaction term allows the value of keeping one
      # species to depend on how much of the other was kept. The opt-out
      # utility shifts with angler age and avidity, so more avid anglers are
      # less easily deterred from fishing.
      trip_data[, `:=`(
        vA_trip =
          beta_sqrt_sf_keep * sqrt(tot_keep_sf_new) +
          beta_sqrt_sf_release * sqrt(tot_rel_sf_new) +
          beta_sqrt_bsb_keep * sqrt(tot_keep_bsb_new) +
          beta_sqrt_bsb_release * sqrt(tot_rel_bsb_new) +
          beta_sqrt_sf_bsb_keep * (sqrt(tot_keep_sf_new) * sqrt(tot_keep_bsb_new)) +
          beta_sqrt_scup_catch * sqrt(tot_scup_catch) +
          beta_cost * cost,

        vA_optout =
          beta_opt_out +
          beta_opt_out_age * age +
          beta_opt_out_avidity * total_trips_12
      )]

      mean_trip_data <- copy(trip_data)

      drop_cols <- intersect(
        c("beta_cost", "beta_opt_out", "beta_opt_out_age",
          "beta_opt_out_avidity", "beta_sqrt_bsb_keep", "beta_sqrt_bsb_release",
          "beta_sqrt_scup_catch", "beta_sqrt_sf_bsb_keep",
          "beta_sqrt_sf_keep", "beta_sqrt_sf_release",
          "age", "cost", "total_trips_12"),
        names(mean_trip_data)
      )
      if (length(drop_cols)) mean_trip_data[, (drop_cols) := NULL]

      keep_vars <- setdiff(names(mean_trip_data), c("date", "mode", "tripid"))
      mean_trip_data <- mean_trip_data[, lapply(.SD, mean),
                                       by = .(date, mode, tripid),
                                       .SDcols = keep_vars]

      mean_trip_data[, probA := calc_prob_trip(vA_trip, vA_optout)]
      mean_trip_data[, c("vA_trip", "vA_optout", "catch_draw") := NULL]

      wt_cols <- c(
        "tot_keep_sf_new", "tot_rel_sf_new", "tot_sf_catch",
        "tot_keep_bsb_new", "tot_rel_bsb_new", "tot_bsb_catch",
        "tot_keep_scup_new", "tot_rel_scup_new", "tot_scup_catch"
      )

      mean_trip_data[, (wt_cols) := lapply(.SD, function(x) x * probA), .SDcols = wt_cols]

      mean_trip_data <- merge(mean_trip_data, dtripz, by = c("mode", "date"), all.x = TRUE)
      drop_reg_cols <- intersect(
        c("bsb_bag", "bsb_min", "fluke_bag", "fluke_min", "scup_bag", "scup_min"),
        names(mean_trip_data)
      )
      if (length(drop_reg_cols)) mean_trip_data[, (drop_reg_cols) := NULL]

      mean_trip_data[, mean_prob := mean(probA), by = .(mode, date)]
      mean_trip_data[is.na(mean_prob) | mean_prob == 0, mean_prob := NA_real_]
      mean_trip_data[, sims := fifelse(!is.na(mean_prob), round(dtrip / mean_prob), 0)]
      mean_trip_data[, expand := sims / n_draws]
      mean_trip_data[, n_choice_occasions := 1]

      expand_cols <- c(wt_cols, "n_choice_occasions", "probA")
      mean_trip_data[, (expand_cols) := lapply(.SD, function(x) x * expand), .SDcols = expand_cols]

      for (j in names(mean_trip_data)) setattr(mean_trip_data[[j]], "label", NULL)

      aggregate_trip_data <- mean_trip_data[, lapply(.SD, sum),
                                            by = .(date, mode),
                                            .SDcols = expand_cols]

      setnames(
        aggregate_trip_data,
        old = c("probA", "tot_sf_catch", "tot_bsb_catch", "tot_scup_catch",
                "tot_keep_sf_new", "tot_keep_bsb_new", "tot_keep_scup_new",
                "tot_rel_sf_new", "tot_rel_bsb_new", "tot_rel_scup_new"),
        new = c("estimated_trips", "sf_catch", "bsb_catch", "scup_catch",
                "sf_keep", "bsb_keep", "scup_keep",
                "sf_rel", "bsb_rel", "scup_rel"),
        skip_absent = TRUE
      )

      list_names <- c("bsb_catch", "bsb_keep", "bsb_rel",
                      "scup_catch", "scup_keep", "scup_rel",
                      "sf_catch", "sf_keep", "sf_rel",
                      "estimated_trips", "n_choice_occasions")

      summed_results <- aggregate_trip_data[, lapply(.SD, sum),
                                            by = .(mode),
                                            .SDcols = list_names]

      MRIP_comparison_draw <- MRIP_comparison[
        draw == i & state == s & mode == md,
        .(mode, sf_keep, sf_rel, sf_catch,
          bsb_keep, bsb_rel, bsb_catch,
          scup_keep, scup_rel, scup_catch)
      ]

      compare_out <- build_compare_table(summed_results, MRIP_comparison_draw, md)
      compare_out[, `:=`(draw = i, state = s)]

      calib_comparison[[k]] <- compare_out
      k <- k + 1L
    }
  }
}

calib_comparison_combined <- rbindlist(calib_comparison, use.names = TRUE, fill = TRUE)
setcolorder(calib_comparison_combined, c("state", "mode", "species", "draw",
                                         setdiff(names(calib_comparison_combined),
                                                 c("state", "mode", "species", "draw"))))

fst::write_fst(calib_comparison_combined,
                   file.path(iterative_input_data_cd,
                   paste0("archive/miscellaneous/calibration_comparison.fst")))
                   
