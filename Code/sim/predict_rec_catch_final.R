
################################################################################
################################################################################
# Script:       predict_rec_catch_final.R
# Purpose:      Projection-year simulation. Re-runs the trip model under the
#               PROJECTION year's regulations and catch-at-length, carrying
#               forward the reallocation proportions the calibration settled
#               on, and compares the result to the calibration-year baseline.
#               The output is the difference table the model exists to
#               produce: for each state, mode and species, projected versus
#               baseline harvest, releases, dead discards and angler welfare,
#               with absolute and percent differences.
#
#               Carrying the calibrated proportions forward is the central
#               modeling assumption. The calibration inferred how far angler
#               behavior departs from strict compliance in the base year; the
#               projection assumes that same departure persists under the new
#               regulations, rather than re-estimating it.
# Inputs:       projected_catch_at_length.csv, calibrated_model_stats.fst,
#               L_W_Conversion.csv, proj_catch_draws_<ST>_<i>.fst,
#               base_outcomes_<ST>_<MODE>_<DRAW>.fst,
#               n_choice_occasions_<ST>_<MODE>_<DRAW>.fst
# Outputs:      The final_compare table of projected-vs-baseline differences.
# Dependencies: Objects iterative_input_data_cd and input_data_cd from the
#               calling environment - "R code wrapper.R" sources this file as
#               STEP 3. Packages data.table, fst, readr.
# Pipeline:     Last of the three R steps. Deliberately mirrors
#               calibrate_rec_catch1_final.R so that baseline and projection
#               are computed the same way and their difference is meaningful.
# Dev paths:    1 hardcoded absolute path to a developer's local machine
#               (E:\), at line 61.
#
# NOTE - THE CONFIGURATION BLOCK BELOW IS SET FOR TESTING, NOT PRODUCTION.
# states is c("MA", "RI") rather than all nine, and n_simulations is 3. It
# also RE-DECLARES n_simulations, overwriting whatever "R code wrapper.R" set
# (10) - so sourcing this file silently changes that object for anything that
# runs afterwards in the same session. iterative_input_data_cd is likewise
# re-hardcoded here to an absolute E: path, overriding the caller's value.
# Left as-is per this session's scope.
#
# Duplicated helpers: safe_divide(), calc_prob_trip() and parse_date_any() are
# verbatim copies of the definitions in calibrate_rec_catch0_optimized.R /
# calibrate_rec_catch1_final.R. Only the helpers new to this file are
# documented below.
################################################################################
################################################################################

# Efficient projection-year simulation for summer flounder, black sea bass, and scup.
# Designed to mirror calibrate_rec_catch1_final.R while carrying forward calibrated
# reallocation parameters from calibrated_model_stats.csv.

# Three parts to this file:
  # globals
  # helper functions
  # main functions
  # projection 

library(data.table)
library(fst)
library(readr)

## Globals
iterative_input_data_cd="E:/Lou_projects/flukeRDM/flukeRDM_iterative_data"

n_choice_occ_cd<-file.path(iterative_input_data_cd, "archive/n_choice_occasion")
base_outcome_cd<-file.path(iterative_input_data_cd, "archive/base_outcomes")
misc_cd<-file.path(iterative_input_data_cd, "archive/miscellaneous")
proj_catch_cd<-file.path(iterative_input_data_cd, "archive/proj_catch_draws")


states <- c("MA", "RI")
modes = c("sh", "pr", "fh") 
ndraws<-50
n_simulations<-3
draws<-1:n_simulations
n_sims<-max(draws)

## Helper functions 
safe_divide <- function(num, den) {
  ifelse(is.na(den) | den == 0, NA_real_, num / den)
}

#' @title First non-missing value, or a fallback
#' @description Used when pulling a scalar attribute out of a filtered table
#'   that may be empty or may lead with NA - for instance a discard mortality
#'   rate for a stratum with no records. Returns the supplied default rather
#'   than propagating NA into a rate calculation.
#' @param x Vector to take the first usable value from.
#' @param default Value returned when x is empty or starts with NA.
#' @return A length-one value.
first_non_na <- function(x, default) {
  if (length(x) == 0L || is.na(x[1])) default else x[1]
}

calc_prob_trip <- function(v_trip, v_optout) {
  z <- v_trip - v_optout
  out <- numeric(length(z))
  pos <- z >= 0
  out[pos] <- 1 / (1 + exp(-z[pos]))
  ez <- exp(z[!pos])
  out[!pos] <- ez / (1 + ez)
  out
}

parse_date_any <- function(x) {
  data.table::as.IDate(as.Date(
    x,
    tryFormats = c("%d%b%Y", "%Y-%m-%d", "%m/%d/%Y", "%d/%m/%Y")))
}

#' @title Read the first .fst file that exists from a list of candidates
#' @description Tolerates the repo's inconsistent file naming. The same
#'   logical input has been written under more than one path convention across
#'   pipeline vintages (with and without a "_new" infix, in different
#'   subfolders), so callers pass every plausible path and take whichever is
#'   present. Errors listing all candidates when none exist, which is more
#'   useful than a bare "file not found".
#' @param path_candidates Character vector of paths to try, in priority order.
#' @return The first existing file, read as a data.table.
read_fst_any <- function(path_candidates) {
  path <- path_candidates[file.exists(path_candidates)][1]
  if (is.na(path)) stop("None of these files exist: ", paste(path_candidates, collapse = "; "))
  as.data.table(fst::read_fst(path))
}


# length-weight varies by month, so create one row per month.
months_dt <- data.table::data.table(month = 1:12)


## Main functions
# Pull common input data 
#' @title Load the inputs shared across every state and draw
#' @description Reads the projected catch-at-length lookup, the calibrated
#'   reallocation statistics and the length-weight conversions once, so the
#'   per-draw loop does not re-read them. This hoisting is the main
#'   performance difference between this version and the archived predecessor
#'   in Code/archive.
#' @param iterative_input_data_cd Directory holding simulation-generated data.
#' @param states State codes to retain; NULL keeps all.
#' @param draws Draw numbers to retain.
#' @param size_lookup_file Filename of the projected catch-at-length table.
#' @param calib_file Path to the calibrated model statistics, which carry the
#'   reallocation proportions the projection reuses.
#' @param lw_file Path to the length-weight conversion table.
#' @return A named list of the shared tables, to be passed into the per-draw
#'   projection.
read_projection_common_inputs <- function(iterative_input_data_cd,
                                          states,
                                          draws,
                                          size_lookup_file = "projected_catch_at_length.csv",
                                          calib_file = file.path(misc_cd, "calibrated_model_stats.fst"),
                                          lw_file = file.path(misc_cd, "L_W_Conversion.csv")) {
  
  l_w_conversion <- data.table::as.data.table(readr::read_csv(lw_file, show_col_types = FALSE))
  if (!is.null(states) && "state" %in% names(l_w_conversion)) {
    l_w_conversion <- l_w_conversion[state %in% states]
  }
  data.table::setkeyv(l_w_conversion, intersect(c("state", "species", "month"), names(l_w_conversion)))
  
  size_lookup <- data.table::as.data.table(
    readr::read_csv(file.path(misc_cd, size_lookup_file), show_col_types = FALSE))
  
  if (!"mode" %in% names(size_lookup)) size_lookup[, mode := NA_character_]
  size_lookup <- size_lookup[!is.na(fitted_prob), .(state, draw, species, mode, fitted_prob, length)]
  if (!is.null(states)) size_lookup <- size_lookup[state %in% states]
  if (!is.null(draws))  size_lookup <- size_lookup[draw %in% draws]
  data.table::setkey(size_lookup, state, draw, species, mode)
  size_lookup[, month := NA_integer_]
  
  size_lookup_wt <- size_lookup[, .(month = 1:12),
                                by = .(state, draw, species, mode, fitted_prob, length) ]
  
  size_lookup_wt <- merge(
    size_lookup_wt,
    l_w_conversion,
    by = c("species", "month", "state"),
    all.x = TRUE  )
  
  size_lookup_wt[, fish_weight_lb := data.table::fcase(
    species == "scup",
    exp(ln_a + b * log(length)) * 2.20462262185,
    species %chin% c("sf", "bsb"),
    a * length^b * 2.20462262185,
    default = NA_real_)]
  
  calib <- data.table::as.data.table(fst::read_fst(calib_file))
  if (!"all_keep_to_rel" %in% names(calib)) {
    calib[, all_keep_to_rel := as.integer(p_keep_to_rel >= 1)]
  }
  
  calib[is.na(all_keep_to_rel), all_keep_to_rel := as.integer(p_keep_to_rel >= 1)]
  calib <- calib[, .(state, mode, draw, species, floor_used_in, keep_to_rel, rel_to_keep,
                     p_rel_to_keep, p_keep_to_rel, all_keep_to_rel)]
  
  data.table::setkey(calib, state, draw, species, mode)
  

  directed_trips <- data.table::rbindlist(
    lapply(states, function(st) {
      dt <- data.table::as.data.table(
        fst::read_fst(file.path(
          misc_cd,
          paste0("directed_trips_calibration_", st, ".fst"))))
      dt[, state := st]
      dt}),
    fill = TRUE,
    use.names = TRUE)
  
  calendar_adjustments <- data.table::rbindlist(
    lapply(states, function(st) {
      dt <- data.table::as.data.table(
        readr::read_csv(
          file.path(
            misc_cd,
            paste0("proj_year_calendar_adjustments_", st, ".csv")),
          show_col_types = FALSE))
      dt[state == st]
    }),
    fill = TRUE,
    use.names = TRUE)
  
  list(size_lookup = size_lookup,
       size_lookup_wt = size_lookup_wt,
       calib = calib, 
       l_w_conversion = l_w_conversion, 
       directed_trips = directed_trips, 
       calendar_adjustments = calendar_adjustments)
}


# Simulate trip-level outcomes
# One generic simulator replaces the old simulate_mode_sf/bsb/scup functions.
# Returns trip totals and long length counts at the same time.
#' @title Simulate one species under the projection-year regulations
#' @description The projection counterpart of simulate_species_realloc() in
#'   calibrate_rec_catch1_final.R, and deliberately parallel to it: expand
#'   catch to individual fish, draw lengths from the PROJECTION year's
#'   catch-at-length distribution, apply the projection year's minimum size
#'   and bag limit in encounter order, then apply the reallocation
#'   proportions inherited from the calibration rather than searching for new
#'   ones.
#'
#'   Keeping the two functions structurally identical is what makes the
#'   baseline-versus-projection difference attributable to the regulation
#'   change rather than to a difference in method.
#' @param catch_dt Trip-level catch and regulations for one stratum and draw.
#' @param catch_col,bag_col,min_col Column names for this species' catch
#'   count, projection-year bag limit and projection-year minimum size.
#' @param size_dt Projection-year catch-at-length lookup.
#' @param species_prefix One of "sf", "bsb", "scup".
#' @return Per-trip kept, released and weight columns for this species, with
#'   the separate utility columns maintained for summer flounder and black sea
#'   bass as in the calibration.
simulate_species_project <- function(catch_dt,
                                     size_dt,
                                     calib_row,
                                     cfg,
                                     mode_value = NULL) {
  
  key_cols <- c("date_parsed", "mode", "tripid", "catch_draw")
  sp <- cfg$species
  
  if (!is.null(mode_value)) catch_dt <- catch_dt[mode == mode_value]
  
  pos_dt <- catch_dt[
    get(cfg$catch_col) > 0,
    c(key_cols, cfg$catch_col, cfg$bag_col, cfg$min_col),  with = FALSE ]
  
  if (nrow(pos_dt) == 0L || nrow(size_dt) == 0L || sum(size_dt$fitted_prob, na.rm = TRUE) <= 0) {
    return(list(trip = data.table::data.table(), length = data.table::data.table()))
  }
  
  setnames(pos_dt,
           old = c(cfg$catch_col, cfg$bag_col, cfg$min_col),
           new = c("catch_n", "bag", "min_sz"))
  
  fish_dt <- pos_dt[rep(seq_len(.N), catch_n)]
  fish_dt[, fishid := seq_len(.N)]
  
  fish_dt[, fitted_length := sample(
    size_dt$length,
    .N,
    replace = TRUE,
    prob = size_dt$fitted_prob)]
  
  fish_dt[, month := data.table::month(parse_date_any(date_parsed))]
  
  fish_dt[
    size_dt,
    fish_weight_lb := i.fish_weight_lb,
    on = .(fitted_length = length, month) ]
  
  fish_dt[, posskeep := fifelse(fitted_length >= min_sz, 1L, 0L)]
  setorder(fish_dt, date_parsed, mode, tripid, catch_draw, fishid)
  fish_dt[, csum_keep := cumsum(posskeep), by = key_cols]
  fish_dt[, keep := fifelse(bag > 0 & posskeep == 1L & csum_keep <= bag, 1L, 0L)]
  fish_dt[, release := fifelse(keep == 0L, 1L, 0L)]
  fish_dt[, kept_to_released_flag := 0L]
  
  floor_sublegal_cm <- min(fish_dt$min_sz, na.rm = TRUE) - cfg$floor_below_min_in * 2.54
  fish_dt[, subl_harv_indicator := fifelse(release == 1L & fitted_length >= floor_sublegal_cm, 1L, 0L)]
  
  rel_to_keep <- calib_row$rel_to_keep
  keep_to_rel <- calib_row$keep_to_rel
  p_rel_to_keep <- calib_row$p_rel_to_keep
  p_keep_to_rel <- calib_row$p_keep_to_rel
  all_keep_to_rel <- fifelse(is.na(calib_row$all_keep_to_rel), as.integer(p_keep_to_rel >= 1), calib_row$all_keep_to_rel)
  
  rel_to_keep <- fifelse(is.na(rel_to_keep), 0, rel_to_keep)
  keep_to_rel <- fifelse(is.na(keep_to_rel), 0, keep_to_rel)
  p_rel_to_keep <- fifelse(is.na(p_rel_to_keep), 0, p_rel_to_keep)
  p_keep_to_rel <- fifelse(is.na(p_keep_to_rel), 0, p_keep_to_rel)
  
  if (rel_to_keep == 1 && fish_dt[, sum(release)] > 0) {
    realloc_dt <- fish_dt[subl_harv_indicator == 1L]
    base_dt <- fish_dt[subl_harv_indicator == 0L]
    
    if (nrow(realloc_dt) > 0L) {
      realloc_dt[, u := runif(.N)]
      setorder(realloc_dt, u)
      realloc_dt[, idx := seq_len(.N)]
      n_to_keep <- round(p_rel_to_keep * nrow(realloc_dt))
      n_to_keep <- max(0L, min(n_to_keep, nrow(realloc_dt)))
      realloc_dt[, keep_new := fifelse(idx <= n_to_keep, 1L, 0L)]
      realloc_dt[, rel_new := fifelse(keep_new == 0L, 1L, 0L)]
      realloc_dt[, `:=`(keep = keep_new, release = rel_new)]
      realloc_dt[, c("u", "idx", "keep_new", "rel_new") := NULL]
      fish_dt <- rbindlist(list(realloc_dt, base_dt), use.names = TRUE, fill = TRUE)
    }
  }
  
  if (keep_to_rel == 1 && fish_dt[, sum(keep)] > 0) {
    if (all_keep_to_rel == 1) {
      fish_dt[, kept_to_released_flag := keep]
      fish_dt[, release := keep + release]
      fish_dt[, keep := 0L]
    } else {
      realloc_dt <- fish_dt[keep == 1L]
      base_dt <- fish_dt[keep == 0L]
      
      if (nrow(realloc_dt) > 0L) {
        realloc_dt[, u := runif(.N)]
        setorder(realloc_dt, u)
        realloc_dt[, idx := seq_len(.N)]
        n_to_release <- round(p_keep_to_rel * nrow(realloc_dt))
        n_to_release <- max(0L, min(n_to_release, nrow(realloc_dt)))
        realloc_dt[, rel_new := fifelse(idx <= n_to_release, 1L, 0L)]
        realloc_dt[, keep_new := fifelse(rel_new == 0L, 1L, 0L)]
        realloc_dt[, kept_to_released_flag := fifelse(rel_new == 1L, 1L, 0L)]
        realloc_dt[, `:=`(keep = keep_new, release = rel_new)]
        realloc_dt[, c("u", "idx", "keep_new", "rel_new") := NULL]
        fish_dt <- rbindlist(list(realloc_dt, base_dt), use.names = TRUE, fill = TRUE)
      }
    }
  }
  
  if (isTRUE(cfg$utility_adjust)) {
    fish_dt[, keep_util := fifelse(keep == 1L | kept_to_released_flag == 1L, 1L, 0L)]
    fish_dt[, release_util := fifelse(release == 1L & kept_to_released_flag == 0L, 1L, 0L)]
  } else {
    fish_dt[, keep_util := keep]
    fish_dt[, release_util := release]
  }
  
  fish_dt[, species := cfg$species]
  
  fish_dt[, keep_weight_lb := keep * fish_weight_lb]
  fish_dt[, release_weight_lb := release * fish_weight_lb]
  
  trip_pos <- fish_dt[, .(
    tot_keep = sum(keep),
    tot_rel = sum(release),
    tot_keep_util = sum(keep_util),
    tot_rel_util = sum(release_util),
    keep_weight_lb = sum(keep_weight_lb, na.rm = TRUE),
    release_weight_lb = sum(release_weight_lb, na.rm = TRUE)
  ), by = c("date_parsed", "mode", "tripid", "catch_draw")]
  
  data.table::setnames(
    trip_pos,
    old = c(
      "tot_keep", "tot_rel",
      "tot_keep_util", "tot_rel_util",
      "keep_weight_lb", "release_weight_lb"
    ),
    new = c(
      paste0("tot_keep_", sp, "_new"),
      paste0("tot_rel_", sp, "_new"),
      paste0("tot_keep_", sp, "_util"),
      paste0("tot_rel_", sp, "_util"),
      paste0("tot_keep_", sp, "_weight_lb_new"),
      paste0("tot_rel_", sp, "_weight_lb_new")) )
  
  trip_out <- trip_pos
  data.table::setkeyv(trip_out, c("date_parsed", "mode", "tripid", "catch_draw"))
  
  list(trip = trip_out)
}


species_config <- data.table(
  species = c("sf", "bsb", "scup"),
  catch_col = c("sf_cat", "bsb_cat", "scup_cat"),
  bag_col = c("fluke_bag", "bsb_bag", "scup_bag"),
  min_col = c("fluke_min", "bsb_min", "scup_min"),
  keep_col = c("tot_keep_sf_new", "tot_keep_bsb_new", "tot_keep_scup_new"),
  rel_col = c("tot_rel_sf_new", "tot_rel_bsb_new", "tot_rel_scup_new"),
  catch_total_col = c("tot_cat_sf_new", "tot_cat_bsb_new", "tot_cat_scup_new"),
  keep_util_col = c("tot_keep_sf_util", "tot_keep_bsb_util", "tot_keep_scup_util"),
  rel_util_col = c("tot_rel_sf_util", "tot_rel_bsb_util", "tot_rel_scup_util"),
  utility_adjust = c(TRUE, TRUE, FALSE),
  disc_mort = c(0.10, 0.15, 0.15)
)

### Final Projection call

common_inputs <- read_projection_common_inputs(
  iterative_input_data_cd = iterative_input_data_cd,
  states = states,
  draws = draws)

k<-1
predictions_list<-list()

system.time({
  
  for (st in states){
    for (dr in draws){
    
    # State-level directed trips, filtered to draw.
    directed_trips <- common_inputs$directed_trips[draw == dr & state==st]
    
    # Projection catch draws.
    catch_data <- data.table::as.data.table(
      fst::read_fst(file.path(
        proj_catch_cd,
        paste0("proj_catch_draws_", st, "_", dr, ".fst")
      ))
    )
    data.table::setkey(directed_trips, mode, date_parsed)
    data.table::setkey(catch_data, mode, date_parsed)
    
    catch_data <- directed_trips[catch_data]
    
    # Common input filtering only; do not reread.
    size_lookup <- common_inputs$size_lookup[
      state == st & draw == dr & !is.na(fitted_prob),
      .(state, draw, species, mode, fitted_prob, length)
    ]
    
    size_lookup_wt <- common_inputs$size_lookup_wt[state == st & draw == dr]
    

    calib <- common_inputs$calib[
      state == st & draw == dr,
      .(state, mode, draw, species, floor_used_in, keep_to_rel, rel_to_keep,
        p_rel_to_keep, p_keep_to_rel, all_keep_to_rel)]
    
    # Base outcomes and n-choice occasions.
    base_list <- list()
    nchoice_list <- list()
    for (md in modes) {
      base_list[[md]] <- fst::read_fst(
        file.path(base_outcome_cd,
                  paste0("base_outcomes_", st, "_", md, "_", dr, ".fst")))
      
      nchoice_list[[md]] <- fst::read_fst(
        file.path(n_choice_occ_cd,
                  paste0("n_choice_occasions_", st, "_", md, "_", dr, ".fst")))
    }
    
    base_outcomes <- data.table::rbindlist(base_list, fill = TRUE, use.names = TRUE)
    n_choice_occasions <- data.table::rbindlist(nchoice_list, fill = TRUE, use.names = TRUE)
    
    util_cols <- grep("^tot_(keep|rel)_.*_util$", names(base_outcomes), value = TRUE)
    if (length(util_cols)) data.table::setnames(base_outcomes, old = util_cols, new = paste0(util_cols, "_base"))
    
    # 5. Simulate species/mode catch. Split by mode once to avoid rescanning full catch_data.
    catch_by_mode <- split(catch_data, by = "mode", keep.by = TRUE)
    sim_results <- vector("list", nrow(species_config) * length(modes))
    kk <- 0L
    for (md in modes) {
      catch_md <- catch_by_mode[[md]]
      if (is.null(catch_md) || !nrow(catch_md)) next
      
      for (rr in seq_len(nrow(species_config))) {
        cfg <- species_config[rr]
        
        floor_val <- first_non_na(
          calib[mode == md & species == cfg$species, floor_used_in],
          cfg$floor_below_min_in
        )
        cfg[, floor_below_min_in := floor_val]
        
        size_dt <- size_lookup_wt[species == cfg$species & (is.na(mode) | mode == md)]
        if (nrow(size_dt) == 0L) size_dt <- size_lookup_wt[species == cfg$species]
        
        if (nrow(size_dt) == 0L) size_dt <- size_lookup[species == cfg$species]
        calib_row <- calib[mode == md & species == cfg$species]
        if (!nrow(calib_row)) {
          calib_row <- data.table::data.table(rel_to_keep = 0, keep_to_rel = 0, p_rel_to_keep = 0,
                                              p_keep_to_rel = 0, all_keep_to_rel = 0)
        }
        kk <- kk + 1L
        # catch_md is already mode-specific, so no mode_value scan needed here.
        sim_results[[kk]] <- simulate_species_project(
          catch_dt = catch_md,
          size_dt = size_dt,
          calib_row = calib_row,
          cfg = cfg,
          mode_value = NULL )
      }
    }
    sim_results <- sim_results[!vapply(sim_results, is.null, logical(1))]
    
    # 6. Build trip_data.
    trip_parts <- lapply(sim_results, `[[`, "trip")
    trip_parts <- Filter(function(x) !is.null(x) && nrow(x) > 0L, trip_parts)
    key_cols <- c("date_parsed", "mode", "tripid", "catch_draw")
    
    trip_data <- data.table::rbindlist(trip_parts, fill = TRUE, use.names = TRUE)
    
    fill_cols <- grep("^tot_", names(trip_data), value = TRUE)
    
    trip_data[, (fill_cols) := lapply(.SD, function(x) {
                x[is.na(x)] <- 0
                x
              }),
              .SDcols = fill_cols ]  
    
    trip_data <- trip_data[, lapply(.SD, sum),
                           by = key_cols,
                           .SDcols = fill_cols]
    
    trip_data[, `:=`(
      tot_cat_sf_new = tot_keep_sf_new + tot_rel_sf_new,
      tot_cat_bsb_new = tot_keep_bsb_new + tot_rel_bsb_new,
      tot_cat_scup_new = tot_keep_scup_new + tot_rel_scup_new)]
    
    key_cols <- c("date_parsed", "mode", "tripid", "catch_draw")
    
    data.table::setkeyv(base_outcomes, key_cols)
    data.table::setkeyv(trip_data, key_cols)
    
    trip_data <- trip_data[base_outcomes]
    
    fill_cols <- grep(
      "^tot_|_weight_lb_new$|_util$",
      names(trip_data),
      value = TRUE)
    
    trip_data[,(fill_cols) := lapply(.SD, function(x) {
      x[is.na(x)] <- 0
      x
    }), .SDcols = fill_cols]  
    
    
    # 7. Utility and expected trip outcomes.
    trip_data[, `:=`(
      v0_trip =
        beta_sqrt_sf_keep * sqrt(tot_keep_sf_util_base) +
        beta_sqrt_sf_release * sqrt(tot_rel_sf_util_base) +
        beta_sqrt_bsb_keep * sqrt(tot_keep_bsb_util_base) +
        beta_sqrt_bsb_release * sqrt(tot_rel_bsb_util_base) +
        beta_sqrt_sf_bsb_keep * (sqrt(tot_keep_sf_util_base) * sqrt(tot_keep_bsb_util_base)) +
        beta_sqrt_scup_catch * sqrt(tot_cat_scup_base) +
        beta_cost * cost,
      
      vA_trip =
        beta_sqrt_sf_keep * sqrt(tot_keep_sf_util) +
        beta_sqrt_sf_release * sqrt(tot_rel_sf_util) +
        beta_sqrt_bsb_keep * sqrt(tot_keep_bsb_util) +
        beta_sqrt_bsb_release * sqrt(tot_rel_bsb_util) +
        beta_sqrt_sf_bsb_keep * (sqrt(tot_keep_sf_util) * sqrt(tot_keep_bsb_util)) +
        beta_sqrt_scup_catch * sqrt(tot_cat_scup_new) +
        beta_cost * cost,
      
      v_optout = beta_opt_out + beta_opt_out_age * age + beta_opt_out_avidity * total_trips_12)]
    
    mean_drop_cols <- intersect(
      c("beta_opt_out", "beta_opt_out_age", "beta_opt_out_avidity",
        "beta_sqrt_bsb_keep", "beta_sqrt_bsb_release", "beta_sqrt_scup_catch",
        "beta_sqrt_sf_bsb_keep", "beta_sqrt_sf_keep", "beta_sqrt_sf_release",
        "age", "cost", "total_trips_12", "catch_draw"),
      names(trip_data))
    
    mean_vars <- setdiff(
      names(trip_data),
      c("date_parsed", "mode", "tripid", mean_drop_cols))
    
    mean_trip_data <- trip_data[,
      lapply(.SD, mean),
      by = .(date_parsed, mode, tripid),
      .SDcols = mean_vars]
    
    mean_trip_data[, `:=`(
      prob0 = exp(v0_trip) / (exp(v0_trip) + exp(v_optout)),
      probA = exp(vA_trip) / (exp(vA_trip) + exp(v_optout)),
      log_sum_alt = log((exp(vA_trip) + exp(v_optout))),
      log_sum_base = log((exp(v0_trip) + exp(v_optout)))  )]
    
    # CV
    # Here I take the negative of the CS formula for easier interpretability of model output
    mean_trip_data[, `:=`(
      CV = -(1/beta_cost)*(log_sum_alt - log_sum_base))]
    
    
    new_cols <- c("tot_keep_sf_new", "tot_rel_sf_new", "tot_cat_sf_new",
                  "tot_keep_bsb_new", "tot_rel_bsb_new", "tot_cat_bsb_new",
                  "tot_keep_scup_new", "tot_rel_scup_new", "tot_cat_scup_new", 
                  "tot_keep_sf_weight_lb_new","tot_rel_sf_weight_lb_new",
                  "tot_keep_bsb_weight_lb_new","tot_rel_bsb_weight_lb_new",
                  "tot_keep_scup_weight_lb_new","tot_rel_scup_weight_lb_new")
    
    base_cols_drop <- intersect(
      c("tot_keep_sf_base", "tot_rel_sf_base", "tot_cat_sf_base",
        "tot_keep_bsb_base", "tot_rel_bsb_base", "tot_cat_bsb_base",
        "tot_keep_scup_base", "tot_rel_scup_base", "tot_cat_scup_base", 
        "log_sum_alt", "log_sum_base", "beta_cost"),
      names(mean_trip_data))
    
    util_cols <- grep("util", names(mean_trip_data), value = TRUE)
    cols_to_drop <- unique(c(base_cols_drop, util_cols))
    
    mean_trip_data[, (cols_to_drop) := NULL]

    mean_trip_data[, (new_cols) := lapply(.SD, function(x) x * probA), .SDcols = new_cols]

    mean_trip_data <- merge(mean_trip_data, n_choice_occasions, by = c("date_parsed", "mode"), all.x = TRUE)
    
    mean_trip_data<- mean_trip_data %>% 
      dplyr::mutate(month=data.table::month(date_parsed))
    
    #Optional calendar adjustments from common_inputs, if available.
    
    calendar_adjustments <- data.table::copy(common_inputs$calendar_adjustments[state == st & draw == dr])

    drop_cols <- intersect(c("dtrip", "dtrip_y2", "state", "draw"), names(calendar_adjustments))
    if (length(drop_cols)) calendar_adjustments[, (drop_cols) := NULL]

    mean_trip_data <- merge(mean_trip_data, calendar_adjustments, by = c("mode", "month"), all.x = TRUE)
    # mean_trip_data <- mean_trip_data %>% 
    #   dplyr::mutate(expansion_factor=1)
    
    #mean_trip_data[is.na(expansion_factor), expansion_factor := 1]
    mean_trip_data[is.na(n_choice_occasions), n_choice_occasions := 0]
    mean_trip_data[, expand := (n_choice_occasions * expansion_factor) / ndraws]
    
    scale_cols <- c(new_cols, "probA", "prob0", "CV")
    
    mean_trip_data[, (scale_cols) := lapply(.SD, function(x) x * expand), .SDcols = scale_cols]
    data.table::setnames(mean_trip_data, c("probA", "prob0"), c("n_trips_alt", "n_trips_base"))

    # Aggregate outputs.
    trip_metrics <- c("CV", "n_trips_alt","n_trips_base", new_cols)
    
    aggregate_mode <- mean_trip_data[, lapply(.SD, sum), by = .(mode), .SDcols = trip_metrics]
    aggregate_all <- mean_trip_data[, lapply(.SD, sum), .SDcols = trip_metrics][, mode := "all modes"]
    model_output <- data.table::rbindlist(list(aggregate_mode, aggregate_all), use.names = TRUE, fill = TRUE)
    model_output_long <- data.table::melt(
      model_output,
      id.vars = "mode",
      measure.vars = trip_metrics,
      variable.name = "metric",
      value.name = "value")
    
    model_output_long[, iteration := dr]
    model_output_long[, state := st]
    
    predictions_list[[k]]<-model_output_long
    
    k<-k+1
    
    }
  }
})

prediction_draws <- dplyr::bind_rows(predictions_list)

# Final outputting of results - merge to baseline year data and compute differences
calib_file = file.path(misc_cd, "calibrated_model_stats.fst")
calib_read <- data.table::as.data.table(fst::read_fst(calib_file))
calib <- calib_read %>% dplyr::filter(state %in% states & draw<=n_sims)

data.table::setDT(prediction_draws)
data.table::setDT(calib)

# -----------------------------
# 1. Split trip metrics from projected data
# -----------------------------
prediction_long<-prediction_draws[, metric := as.character(metric)]

prediction_long[, species := data.table::fcase(
  grepl("_sf_", metric), "sf",
  grepl("_bsb_", metric), "bsb",
  grepl("_scup_", metric), "scup",
  default = NA_character_
)]

trip_compare <- data.table::dcast(
  prediction_long[metric %in% c("n_trips_alt", "n_trips_base")],
  state + mode + iteration ~ metric,
  value.var = "value"
)

trip_compare <- trip_compare[,.(
    state,
    mode,
    iteration,
    species = NA_character_,
    metric = "trips",
    baseline_value = n_trips_base,
    projected_value = n_trips_alt  )
]

# -----------------------------
# 2. Extract species + clean metric labels
# -----------------------------

prediction_long2 <- prediction_long[
  !metric %in% c("n_trips_alt", "n_trips_base")]

prediction_long2[, species := data.table::fcase(
  grepl("_sf_", metric), "sf",
  grepl("_bsb_", metric), "bsb",
  grepl("_scup_", metric), "scup",
  default = NA_character_)]

prediction_long2[, metric_clean := data.table::fcase(
  metric == "CV", "compensating variation ($)",
  metric == "n_trips_alt","projected trips",
  metric == "n_trips_base", "baseline trips",
  metric == "additional trips", "additional trips",
  grepl("tot_keep_.*weight_lb", metric),  "harvest (lbs.)",
  grepl("tot_rel_.*weight_lb", metric),  "discards (lbs.)",
  grepl("tot_keep_", metric),  "harvest (#s)",
  grepl("tot_rel_", metric),  "discards (#s)",
  grepl("tot_cat_", metric),  "catch (#s)",
  default = metric
)]

prediction_long2[, metric := metric_clean]
prediction_long2[, metric_clean := NULL]

data.table::setnames(prediction_long2, "value", "projected_value")

# -----------------------------
# 3. Keep relevant calib columns
# -----------------------------

calib_keep <- calib[!is.na(state) & !is.na(mode) & !is.na(draw) & !is.na(species),
  .(state,mode, iteration = draw, species, model_keep, model_rel, model_catch, model_keep_lbs, model_rel_lbs)]

# -----------------------------
# 4. Add all-modes calib category
# -----------------------------

calib_all_modes <- calib_keep[, .(
    model_keep  = sum(model_keep, na.rm = TRUE),
    model_rel   = sum(model_rel, na.rm = TRUE),
    model_catch = sum(model_catch, na.rm = TRUE),
    model_keep_lbs  = sum(model_keep_lbs, na.rm = TRUE),
    model_rel_lbs   = sum(model_rel_lbs, na.rm = TRUE)),
  by = .(state, iteration, species)]

calib_all_modes[, mode := "all modes"]

calib_keep <- data.table::rbindlist(
  list(calib_keep, calib_all_modes),
  use.names = TRUE,
  fill = TRUE)

# -----------------------------
# 5. Reshape calib to long
# -----------------------------

calib_long <- data.table::melt(
  calib_keep,
  id.vars = c("state", "mode", "iteration", "species"),
  measure.vars = c("model_keep", "model_rel", "model_keep_lbs", "model_rel_lbs", "model_catch"),
  variable.name = "metric",
  value.name = "baseline_value")

calib_long[, metric := data.table::fcase(
  metric == "model_keep",  "harvest (#s)",
  metric == "model_rel",   "discards (#s)",
  metric == "model_catch", "catch (#s)",
  metric == "model_keep_lbs",  "harvest (lbs.)",
  metric == "model_rel_lbs",   "discards (lbs.)",
  default = as.character(metric))]

# -----------------------------
# 6. Merge projected + baseline
# -----------------------------

final_compare <- merge(
  prediction_long2,
  calib_long,
  by = c("state", "mode", "iteration", "species", "metric"),
  all.x = TRUE)

final_compare <- data.table::rbindlist(
  list(final_compare, trip_compare),
  use.names = TRUE,
  fill = TRUE)

# Add discard mortality rates
dm_rates <- data.table::data.table(
  species = c("sf", "bsb", "scup"),
  dm_rate = c(0.10, 0.15, 0.15)
)

# Pull release rows and convert them to dead discard rows
dead_discards <- merge(
  final_compare[metric %in% c("discards (#s)", "discards (lbs.)")],
  dm_rates,
  by = "species",
  all.x = TRUE
)

dead_discards[, `:=`(
  baseline_value  = baseline_value  * dm_rate,
  projected_value = projected_value * dm_rate,
  metric = data.table::fifelse(
    metric == "discards (#s)",
    "dead discards (#s)",
    "dead discards (lbs.)"
  )
)]

dead_discards[, dm_rate := NULL]

# Append to final_compare
final_compare <- data.table::rbindlist(
  list(final_compare, dead_discards),
  use.names = TRUE,
  fill = TRUE
)

final_compare[, difference := projected_value - baseline_value]
final_compare[, pct_difference := ((projected_value - baseline_value)/baseline_value)*100]
final_compare[, difference := round(difference, 1)]
final_compare[, pct_difference := round(pct_difference, 1)]
final_compare[, projected_value := round(projected_value, 0)]
final_compare[, baseline_value := round(baseline_value, 0)]

data.table::setcolorder(final_compare,
  c("iteration", "state", "mode", "species", "metric",
    "baseline_value", "projected_value",
    "difference", "pct_difference"))

data.table::setorder(final_compare, iteration, state, mode, species, metric)

