
################################################################################
################################################################################
# Script:       calibration_routine_final.R
# Purpose:      Calibration PASS 1 - the search that closes the model-vs-MRIP
#               harvest gap measured by PASS 0. For each state x mode x draw
#               and each species it searches for the reallocation proportion p
#               that brings simulated harvest within tolerance of MRIP, by
#               repeatedly re-running the trip simulation with a candidate p
#               and narrowing a bracket around it. The reallocation itself is
#               performed by calibrate_rec_catch1_final.R, which this script
#               sources inside the loop; this file owns the search logic, the
#               convergence criteria and the bookkeeping.
#
#               What p means: anglers do not perfectly obey size and bag
#               limits, and they voluntarily release legal fish. PASS 0
#               simulates strict compliance and therefore misses MRIP. p is
#               the fraction of modeled releases converted to harvest
#               ("rel_to_keep", when the model under-harvests) or of modeled
#               harvest converted to releases ("keep_to_rel", when it
#               over-harvests). It is a calibration parameter, not an
#               estimated behavioral quantity.
# Inputs:       simulated_catch_totals.dta, calibration_comparison.fst
#               (PASS 0's output), and the per-draw calibration catch files
#               read by the sourced simulation script.
# Outputs:      calibrated_model_stats.fst,
#               n_choice_occasions_<ST>_<MODE>_<DRAW>.fst,
#               base_outcomes_<ST>_<MODE>_<DRAW>.fst
# Dependencies: Objects iterative_input_data_cd and input_data_cd must exist
#               in the calling environment - set by "R code wrapper.R", which
#               sources this file as STEP 2. Sources
#               calibrate_rec_catch1_final.R repeatedly. Packages data.table,
#               arrow, haven, readr, fst.
# Pipeline:     Second of the three R steps. Consumes PASS 0's comparison
#               table; its output feeds the projection stage and is also what
#               "check calibration convergence.do" filters down to 100 usable
#               draws.
#
# HOW THE SEARCH WORKS (the part worth understanding before editing):
#   - Convergence for a species is is_achieved(): harvest within 500 fish OR
#     within 5% of MRIP. Same criterion as "check calibration convergence.do".
#   - When not converged, score_species() ranks candidate p values so the best
#     attempt so far can be kept even if nothing fully converges. The score is
#     keep_score + 0.15 * catch_score, so matching HARVEST dominates and total
#     catch acts only as a tie-breaker. The catch tolerances are also
#     deliberately looser (5x on absolute, 4x on percent).
#   - update_bracket() maintains a [lo, hi] bracket on p in [0, 1] and
#     bisects. max_iter = 25 caps the search; p_tol = 1e-4 decides when p is
#     effectively 0 or 1.
#   - Strata where MRIP harvest is exactly zero are special-cased throughout:
#     a percent difference is undefined there, so only the absolute criterion
#     applies and only keep_to_rel is meaningful.
#   - push_globals() writes the current p and direction into the GLOBAL
#     environment. That is how the sourced simulation script receives them -
#     it reads variables like p_rel_to_keep_sf by name rather than taking
#     arguments. This is the coupling to be careful of when refactoring.
#
# NOTE - THE STATE AND DRAW LISTS ARE RESTRICTED. Below, the full nine-state
# vector is immediately overwritten by c("MA", "RI"), and draws is set to 1:3.
# As committed this calibrates two states and three draws, not the production
# set. This reads as debug configuration left in place; it is deliberately NOT
# changed here.
################################################################################
################################################################################

# iterative calibration routine for fluke / black sea bass / scup
# bounded search with best-so-far selection; no catch-hold adjustment.

library(data.table)
library(arrow)
library(haven)
library(readr)
library(fst)


if (!exists("MRIP_comparison", inherits = FALSE)) {
  MRIP_comparison <- read_dta("E:/Lou_projects/flukeRDM/flukeRDM_iterative_data/archive/calib_catch_draws/simulated_catch_totals.dta") |>
    as.data.table()
}

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

baseline_output0 <- as.data.table(fst::read_fst(
  file.path(iterative_input_data_cd,
            paste0("archive/miscellaneous/calibration_comparison.fst"))))

# Reconstruct catch columns defensively if the step-0 file omitted them
if (!("MRIP_catch" %in% names(baseline_output0)) && all(c("MRIP_keep", "MRIP_rel") %in% names(baseline_output0))) {
  baseline_output0[, MRIP_catch := MRIP_keep + MRIP_rel]
}
if (!("model_catch" %in% names(baseline_output0)) && all(c("model_keep", "model_rel") %in% names(baseline_output0))) {
  baseline_output0[, model_catch := model_keep + model_rel]
}
if (!("diff_catch" %in% names(baseline_output0)) && all(c("model_catch", "MRIP_catch") %in% names(baseline_output0))) {
  baseline_output0[, diff_catch := model_catch - MRIP_catch]
}
if (!("pct_diff_catch" %in% names(baseline_output0)) && all(c("diff_catch", "MRIP_catch") %in% names(baseline_output0))) {
  baseline_output0[, pct_diff_catch := fifelse(MRIP_catch != 0, 100 * diff_catch / MRIP_catch, NA_real_)]
}

# The second assignment overwrites the first: as committed this runs MA and RI
# only, for 3 draws. See the header - this looks like debug configuration.
states <- c("MA", "RI", "CT", "NY", "NJ", "DE", "MD", "VA", "NC")
states <- c("MA", "RI")

mode_draw <- c("sh", "pr", "fh")
draws <- 1:3

# states <- c("MA")
# mode_draw <- c("pr")
# draws <- 1:1

# Search settings. The two tolerances are the agreed definition of "close
# enough to MRIP" and are mirrored in "check calibration convergence.do";
# changing them here without changing that script would desynchronize which
# draws are considered usable. max_iter caps the bisection at 25 re-simulations
# per species x stratum, which is the dominant cost of this step. p_tol is the
# threshold below which a proportion counts as exactly 0 or exactly 1.
tol_abs_fish <- 500
tol_abs_pct  <- 5
max_iter     <- 25
p_tol        <- 1e-4

species_vec <- c("sf", "bsb", "scup")

#' @title Has this species converged for this stratum?
#' @description Applies the calibration tolerance: harvest is close enough
#'   when it is within 500 fish OR within 5 percent of the MRIP estimate. The
#'   OR is what lets small strata pass, where 5 percent would be only a few
#'   fish. Strata whose MRIP harvest is exactly zero are special-cased to the
#'   absolute test alone, since a percent difference is undefined there.
#' @param diff_keep Model harvest minus MRIP harvest, in fish.
#' @param pct_diff_keep The same difference as a percentage of MRIP harvest.
#' @param MRIP_keep MRIP harvest; only inspected to detect the zero-target case.
#' @return TRUE if the stratum is within tolerance.
is_achieved <- function(diff_keep, pct_diff_keep, MRIP_keep = NA_real_) {
  if (is.finite(MRIP_keep) && MRIP_keep == 0) {
    return(is.finite(diff_keep) && abs(diff_keep) < tol_abs_fish)
  }
  
  (is.finite(diff_keep) && abs(diff_keep) < tol_abs_fish) ||
    (is.finite(pct_diff_keep) && abs(pct_diff_keep) < tol_abs_pct)
}

#' @title Score a candidate reallocation, lower is better
#' @description Ranks attempts so the best one can be retained when nothing
#'   fully converges within max_iter. Each component is a distance expressed
#'   in units of its own tolerance, so a score of 1 sits exactly at the
#'   tolerance boundary. The combination is
#'       keep_score + 0.15 * catch_score
#'   which encodes the modeling priority: matching HARVEST is the objective,
#'   and total catch only breaks ties between otherwise similar candidates.
#'   The catch tolerances are additionally loosened (5x on the absolute, 4x on
#'   the percent) because catch is less precisely estimated than harvest.
#'   Non-finite inputs score Inf so they never win.
#' @param diff_keep,pct_diff_keep Harvest difference in fish and percent.
#' @param diff_catch,pct_diff_catch Total catch difference in fish and percent.
#' @param MRIP_keep,MRIP_catch MRIP values; inspected only to detect the
#'   zero-target cases where the percent components are dropped.
#' @return A single non-negative score; smaller means a better match.
score_species <- function(diff_keep, pct_diff_keep, diff_catch, pct_diff_catch,
                          MRIP_keep = NA_real_, MRIP_catch = NA_real_) {
  
  keep_score <- if (is.finite(MRIP_keep) && MRIP_keep == 0) {
    if (is.finite(diff_keep)) abs(diff_keep) / tol_abs_fish else Inf
  } else {
    min(
      if (is.finite(diff_keep)) abs(diff_keep) / tol_abs_fish else Inf,
      if (is.finite(pct_diff_keep)) abs(pct_diff_keep) / tol_abs_pct else Inf
    )
  }
  
  catch_score <- if (is.finite(MRIP_catch) && MRIP_catch == 0) {
    if (is.finite(diff_catch)) abs(diff_catch) / (5 * tol_abs_fish) else Inf
  } else {
    min(
      if (is.finite(diff_catch)) abs(diff_catch) / (5 * tol_abs_fish) else Inf,
      if (is.finite(pct_diff_catch)) abs(pct_diff_catch) / (4 * tol_abs_pct) else Inf
    )
  }
  
  keep_score + 0.15 * catch_score
}

#' @title Pull one species' comparison row, inventing an empty one if absent
#' @description A species can be entirely missing from a stratum's comparison
#'   table when it was never caught there. Rather than letting that propagate
#'   as a zero-row subset, this returns a placeholder row with zero model
#'   values and NA differences, so downstream code always has exactly one row
#'   per species and can distinguish "no data" (NA) from "genuinely zero".
#' @param dt Comparison table for one stratum.
#' @param sp Species code: "sf", "bsb" or "scup".
#' @param md Mode label, used only to populate the placeholder.
#' @param s State code, used only to populate the placeholder.
#' @param i Draw number, used only to populate the placeholder.
#' @return A single-row data.table for this species.
extract_species_row <- function(dt, sp, md, s, i) {
  out <- as.data.table(dt)[species == sp]
  if (nrow(out) == 0L) {
    out <- data.table(
      mode = md, species = sp,
      MRIP_keep = NA_real_, model_keep = 0, diff_keep = NA_real_, pct_diff_keep = NA_real_,
      MRIP_rel = NA_real_, model_rel = 0, diff_rel = NA_real_, pct_diff_rel = NA_real_,
      MRIP_catch = NA_real_, model_catch = 0, diff_catch = NA_real_, pct_diff_catch = NA_real_,
      rel_to_keep_new = 0, keep_to_rel_new = 0, p_rel_to_keep_new = 0, p_keep_to_rel_new = 0,
      draw = i, state = s
    )
  }
  out[1]
}

#' @title Initialize the search state for one species
#' @description Decides which direction the reallocation must go and where the
#'   search starts. Direction comes from PASS 0's flags: rel_to_keep when the
#'   model under-harvested, keep_to_rel when it over-harvested, "none" when it
#'   already matches. The starting proportion is PASS 0's closed-form estimate
#'   of what would close the gap, clamped to [0, 1], and the bracket is opened
#'   to [0, 1] so the search can move either way from it.
#'
#'   Two zero-target cases are handled up front. If MRIP harvest and model
#'   harvest are both zero there is nothing to search and the stratum is
#'   marked converged. If MRIP harvest is zero but the model harvests
#'   something, only keep_to_rel can help, so the direction is forced.
#' @param base_row One species' row from the PASS 0 comparison table.
#' @return A list holding direction, the current proportion p, the bracket
#'   endpoints lo and hi, whether tolerance is already met, a convergence
#'   flag, and slots for the best-scoring attempt seen so far.
make_state <- function(base_row) {
  
  mrip_keep  <- as.numeric(base_row$MRIP_keep)
  model_keep <- as.numeric(base_row$model_keep)
  diff_keep  <- as.numeric(base_row$diff_keep)
  
  # zero-target case
  if (is.finite(mrip_keep) && mrip_keep == 0) {
    
    # if model is also effectively zero, nothing to do
    if (is.finite(model_keep) && model_keep == 0) {
      return(list(
        direction = "none",
        p = 0,
        lo = 0,
        hi = NA_real_,
        achieved = TRUE,
        convergence = 1L,
        best_score = Inf,
        best_row = NULL
      ))
    }
    
    # if model_keep > 0 and MRIP_keep == 0, only keep->rel makes sense
    direction <- "keep_to_rel"
    p0 <- ifelse(is.finite(base_row$p_keep_to_rel), as.numeric(base_row$p_keep_to_rel), 0)
    p0 <- max(0, min(1, p0))
    
    return(list(
      direction = direction,
      p = p0,
      lo = 0,
      hi = 1,
      achieved = FALSE,
      convergence = 1L,
      best_score = Inf,
      best_row = NULL
    ))
  }
  
  direction <- if (isTRUE(base_row$rel_to_keep == 1)) {
    "rel_to_keep"
  } else if (isTRUE(base_row$keep_to_rel == 1)) {
    "keep_to_rel"
  } else {
    "none"
  }
  
  p0 <- if (direction == "rel_to_keep") {
    base_row$p_rel_to_keep
  } else if (direction == "keep_to_rel") {
    base_row$p_keep_to_rel
  } else {
    0
  }
  
  p0 <- max(0, min(1, as.numeric(p0)))
  
  list(
    direction = direction,
    p = p0,
    lo = 0,
    hi = if (p0 > 0 && p0 < 1) 1 else NA_real_,
    achieved = FALSE,
    convergence = 1L,
    best_score = Inf,
    best_row = NULL
  )
}

#' @title Publish the current search state to the global environment
#' @description The simulation script sourced inside the search loop does not
#'   take arguments - it reads the reallocation settings from variables in the
#'   global environment by name. This function writes them there, five per
#'   species: the two direction flags, the two proportions, and an
#'   all_keep_to_rel_<sp> flag marking the degenerate case where every
#'   harvested fish is being converted.
#'
#'   This global coupling is the main structural hazard in this file: the
#'   search and the simulation communicate through names rather than through
#'   a call signature, so renaming a variable here silently breaks the
#'   simulation rather than raising an error.
#' @param states_by_sp Named list of per-species search states, as built by
#'   make_state().
#' @param target_env Environment to assign into; the global environment by
#'   default, which is where the sourced script looks.
#' @return Nothing, called for its side effect.
push_globals <- function(states_by_sp, target_env = .GlobalEnv) {
  for (sp in species_vec) {
    st <- states_by_sp[[sp]]
    assign(paste0("rel_to_keep_", sp),
           as.integer(st$direction == "rel_to_keep"),
           envir = target_env)
    assign(paste0("keep_to_rel_", sp),
           as.integer(st$direction == "keep_to_rel"),
           envir = target_env)
    assign(paste0("p_rel_to_keep_", sp),
           if (st$direction == "rel_to_keep") st$p else 0,
           envir = target_env)
    assign(paste0("p_keep_to_rel_", sp),
           if (st$direction == "keep_to_rel") st$p else 0,
           envir = target_env)
    assign(paste0("all_keep_to_rel_", sp),
           as.integer(st$direction == "keep_to_rel" && st$p >= 1 - p_tol),
           envir = target_env)
  }
}

#' @title Advance the bisection after one simulation attempt
#' @description Given the outcome of simulating at the current proportion,
#'   records whether tolerance was met, scores the attempt and keeps it if it
#'   is the best so far, then narrows the [lo, hi] bracket and proposes the
#'   next p. A converged attempt is stored with a score of -Inf so nothing can
#'   displace it.
#'
#'   Bracket direction depends on the reallocation direction: under
#'   rel_to_keep, raising p raises modeled harvest, so an attempt that still
#'   under-harvests moves the lower bound up; under keep_to_rel the
#'   relationship is inverted.
#' @param st The species' current search state, from make_state().
#' @param row The comparison row produced by simulating at the current p.
#' @return The updated search state, with a new p to try next unless the
#'   search has converged or exhausted its bracket.
update_bracket <- function(st, row) {
  if (st$direction == "none") {
    st$achieved <- TRUE
    return(st)
  }
  
  diff_keep     <- as.numeric(row$diff_keep)
  pct_diff_keep <- as.numeric(row$pct_diff_keep)
  diff_catch    <- as.numeric(row$diff_catch)
  pct_diff_catch<- as.numeric(row$pct_diff_catch)
  MRIP_keep     <- as.numeric(row$MRIP_keep)
  MRIP_catch    <- as.numeric(row$MRIP_catch)
  
  st$achieved <- is_achieved(diff_keep, pct_diff_keep, MRIP_keep)
  
  this_score <- score_species(
    diff_keep      = diff_keep,
    pct_diff_keep  = pct_diff_keep,
    diff_catch     = diff_catch,
    pct_diff_catch = pct_diff_catch,
    MRIP_keep      = MRIP_keep,
    MRIP_catch     = MRIP_catch
  )
  
  if (st$achieved) {
    st$best_row <- copy(row)
    st$best_score <- -Inf
    return(st)
  }
  
  if (this_score < st$best_score) {
    st$best_score <- this_score
    st$best_row <- copy(row)
  }
  
  if (st$direction == "rel_to_keep") {
    # larger p => more keep
    if (is.finite(diff_keep) && diff_keep < 0) {
      st$lo <- max(st$lo, st$p)
    } else if (is.finite(diff_keep) && diff_keep > 0) {
      st$hi <- if (is.na(st$hi)) st$p else min(st$hi, st$p)
    }
  } else if (st$direction == "keep_to_rel") {
    # larger p => fewer keep
    if (is.finite(diff_keep) && diff_keep > 0) {
      st$lo <- max(st$lo, st$p)
    } else if (is.finite(diff_keep) && diff_keep < 0) {
      st$hi <- if (is.na(st$hi)) st$p else min(st$hi, st$p)
    }
  }
  
  old_p <- st$p
  
  if (!is.na(st$hi)) {
    st$p <- (st$lo + st$hi) / 2
  } else {
    st$p <- if (old_p == 0) 0.1 else min(1, max(old_p * 1.5, old_p + 0.05))
  }
  
  st$p <- max(0, min(1, st$p))
  
  if (abs(st$p - old_p) < p_tol && !st$achieved) {
    st$convergence <- 0L
  }
  
  if (st$p >= 1 - p_tol && is.na(st$hi) && !st$achieved) {
    st$convergence <- 0L
  }
  
  st
}
  

calibrated <- vector("list", length(states) * length(mode_draw) * length(draws))
k <- 1L

for (s in states) {
  for (md in mode_draw) {
    for (i in draws) {

      baseline_targets_current <- baseline_output0[state == s & draw == i & mode == md]
      if (nrow(baseline_targets_current) == 0L) next

      if (all(is.na(baseline_targets_current$MRIP_keep))) {
        out <- copy(baseline_targets_current)
        out[, `:=`(
          keep_to_rel_sf = 0, rel_to_keep_sf = 0, p_rel_to_keep_sf = 0, p_keep_to_rel_sf = 0, convergence_sf = NA_real_,
          keep_to_rel_bsb = 0, rel_to_keep_bsb = 0, p_rel_to_keep_bsb = 0, p_keep_to_rel_bsb = 0, convergence_bsb = NA_real_,
          keep_to_rel_scup = 0, rel_to_keep_scup = 0, p_rel_to_keep_scup = 0, p_keep_to_rel_scup = 0, convergence_scup = NA_real_,
          iter_used = 0L
        )]
        calibrated[[k]] <- out
        k <- k + 1L
        next
      }

      states_by_sp <- setNames(vector("list", length(species_vec)), species_vec)
      for (sp in species_vec) {
        base_row <- extract_species_row(baseline_targets_current, sp, md, s, i)
        states_by_sp[[sp]] <- make_state(base_row)
        states_by_sp[[sp]]$best_row <- copy(base_row)
        states_by_sp[[sp]]$best_score <- score_species(
          base_row$diff_keep,
          base_row$pct_diff_keep,
          base_row$diff_catch,
          base_row$pct_diff_catch,
          base_row$MRIP_keep,
          base_row$MRIP_catch
        )
        
        states_by_sp[[sp]]$achieved <- is_achieved(
          base_row$diff_keep,
          base_row$pct_diff_keep,
          base_row$MRIP_keep
        )
      }

      iter_used <- 0L
      last_result <- NULL

      sf_floor_below_min_in   <- 3
      bsb_floor_below_min_in  <- 3
      scup_floor_below_min_in <- 3
      
      repeat {
        

        push_globals(states_by_sp, target_env = environment())
        source(file.path(code_cd, "calibrate_rec_catch1_final.R"), local = environment())
        
        last_result <- copy(as.data.table(calib_comparison1))

        all_done <- TRUE
        for (sp in species_vec) {
          row <- extract_species_row(last_result, sp, md, s, i)
          states_by_sp[[sp]] <- update_bracket(states_by_sp[[sp]], row)

          if (!states_by_sp[[sp]]$achieved && states_by_sp[[sp]]$convergence == 1L) {
            all_done <- FALSE
          }
        }

        iter_used <- iter_used + 1L

        if (all_done || iter_used >= max_iter) break
      }


      floor_used_in_sf   <- sf_floor_below_min_in
      floor_used_in_bsb  <- bsb_floor_below_min_in
      floor_used_in_scup <- scup_floor_below_min_in
      
      
      final_rows <- rbindlist(lapply(species_vec, function(sp) {
        st <- states_by_sp[[sp]]
        row <- if (!is.null(st$best_row)) copy(st$best_row) else extract_species_row(last_result, sp, md, s, i)
        row
      }), use.names = TRUE, fill = TRUE)

      final_rows[, `:=`(
        n_sub_kept_sf      = n_sub_kept_sf,
        prop_sub_kept_sf   = prop_sub_kept_sf,
        n_legal_rel_sf     = n_legal_rel_sf,
        prop_legal_rel_sf  = prop_legal_rel_sf,
        
        n_sub_kept_bsb     = n_sub_kept_bsb,
        prop_sub_kept_bsb  = prop_sub_kept_bsb,
        n_legal_rel_bsb    = n_legal_rel_bsb,
        prop_legal_rel_bsb = prop_legal_rel_bsb,
        
        n_sub_kept_scup     = n_sub_kept_scup,
        prop_sub_kept_scup  = prop_sub_kept_scup,
        n_legal_rel_scup    = n_legal_rel_scup,
        prop_legal_rel_scup = prop_legal_rel_scup, 
        
        floor_used_in_sf   = floor_used_in_sf,
        floor_used_in_bsb  = floor_used_in_bsb,
        floor_used_in_scup = floor_used_in_scup
      )]
      
      # set final convergence based on best-so-far row, not just the last attempted row
      final_rows[species == "sf", `:=`(
        keep_to_rel_sf = as.integer(states_by_sp[["sf"]]$direction == "keep_to_rel"),
        rel_to_keep_sf = as.integer(states_by_sp[["sf"]]$direction == "rel_to_keep"),
        p_rel_to_keep_sf = if (states_by_sp[["sf"]]$direction == "rel_to_keep") states_by_sp[["sf"]]$p else 0,
        p_keep_to_rel_sf = if (states_by_sp[["sf"]]$direction == "keep_to_rel") states_by_sp[["sf"]]$p else 0,
        convergence_sf = as.integer(is_achieved(diff_keep, pct_diff_keep, MRIP_keep))
        )]
      
      final_rows[species == "bsb", `:=`(
        keep_to_rel_bsb = as.integer(states_by_sp[["bsb"]]$direction == "keep_to_rel"),
        rel_to_keep_bsb = as.integer(states_by_sp[["bsb"]]$direction == "rel_to_keep"),
        p_rel_to_keep_bsb = if (states_by_sp[["bsb"]]$direction == "rel_to_keep") states_by_sp[["bsb"]]$p else 0,
        p_keep_to_rel_bsb = if (states_by_sp[["bsb"]]$direction == "keep_to_rel") states_by_sp[["bsb"]]$p else 0,
        convergence_bsb = as.integer(is_achieved(diff_keep, pct_diff_keep, MRIP_keep))
        )]
      
      final_rows[species == "scup", `:=`(
        keep_to_rel_scup = as.integer(states_by_sp[["scup"]]$direction == "keep_to_rel"),
        rel_to_keep_scup = as.integer(states_by_sp[["scup"]]$direction == "rel_to_keep"),
        p_rel_to_keep_scup = if (states_by_sp[["scup"]]$direction == "rel_to_keep") states_by_sp[["scup"]]$p else 0,
        p_keep_to_rel_scup = if (states_by_sp[["scup"]]$direction == "keep_to_rel") states_by_sp[["scup"]]$p else 0,
        convergence_scup = as.integer(is_achieved(diff_keep, pct_diff_keep, MRIP_keep))
        )]

      # fill non-target species columns with zeros where still missing
      fill_zero_cols <- c(
        "keep_to_rel_sf","rel_to_keep_sf","p_rel_to_keep_sf","p_keep_to_rel_sf","convergence_sf",
        "keep_to_rel_bsb","rel_to_keep_bsb","p_rel_to_keep_bsb","p_keep_to_rel_bsb","convergence_bsb",
        "keep_to_rel_scup","rel_to_keep_scup","p_rel_to_keep_scup","p_keep_to_rel_scup","convergence_scup",
        "n_sub_kept_scup","prop_sub_kept_scup","n_legal_rel_scup","prop_legal_rel_scup",
        "n_sub_kept_sf","n_legal_rel_sf","prop_sub_kept_sf","prop_legal_rel_sf",
        "n_sub_kept_bsb","n_legal_rel_bsb","prop_sub_kept_bsb","prop_legal_rel_bsb", 
        "floor_used_in_sf", "floor_used_in_bsb", "floor_used_in_scup")
      
      for (cc in intersect(fill_zero_cols, names(final_rows))) {
        set(final_rows, which(is.na(final_rows[[cc]])), cc, 0)
      }

      final_rows[, iter_used := iter_used]
      setcolorder(final_rows, c("draw","state", "mode","species","MRIP_rel","model_rel","diff_rel","pct_diff_rel",
                                "MRIP_keep","model_keep","diff_keep","pct_diff_keep",
                                "MRIP_catch","model_catch","diff_catch","pct_diff_catch",
                                "rel_to_keep_new","keep_to_rel_new","p_rel_to_keep_new","p_keep_to_rel_new",
                                "floor_used_in_sf", "floor_used_in_bsb", "floor_used_in_scup",
                                setdiff(names(final_rows), c("draw","state", "mode","species","MRIP_rel","model_rel","diff_rel","pct_diff_rel",
                                                             "MRIP_keep","model_keep","diff_keep","pct_diff_keep",
                                                             "MRIP_catch","model_catch","diff_catch","pct_diff_catch",
                                                             "rel_to_keep_new","keep_to_rel_new","p_rel_to_keep_new","p_keep_to_rel_new", 
                                                             "floor_used_in_sf", "floor_used_in_bsb", "floor_used_in_scup"))))

      calibrated[[k]] <- final_rows
      k <- k + 1L
    }
  }
}

calibrated_combined <- rbindlist(calibrated, use.names = TRUE, fill = TRUE)

drop_cols <- c(
  "rel_to_keep_new", "keep_to_rel_new",
  "p_rel_to_keep_new", "p_keep_to_rel_new"
)

drop_cols <- intersect(drop_cols, names(calibrated_combined))
calibrated_combined[, (drop_cols) := NULL]

# one row per state-mode-draw is enough, since the final calibration values are wide
calibrated_combined <- unique(calibrated_combined, by = c("state", "mode", "draw", "species"))

front_cols <- c("state", "mode", "draw")
front_cols <- intersect(front_cols, names(calibrated_combined))
data.table::setcolorder(calibrated_combined, c(front_cols, setdiff(names(calibrated_combined), front_cols)))


# identify all species suffixes
species_levels <- c("sf", "bsb", "scup")

# find all columns that have species suffixes
suffix_pattern <- paste0("(", paste(species_levels, collapse = "|"), ")$")
cols <- names(calibrated_combined)

suffix_cols <- cols[grepl(paste0("_", suffix_pattern), cols)]

# get base variable names (remove suffix)
base_names <- unique(sub(paste0("_", suffix_pattern), "", suffix_cols))

# for each base variable, create a collapsed version
for (v in base_names) {
  
  new_col <- v
  
  calibrated_combined[, (new_col) := fifelse(
    species == "sf",  get(paste0(v, "_sf")),
    fifelse(
      species == "bsb", get(paste0(v, "_bsb")),
      fifelse(
        species == "scup", get(paste0(v, "_scup")),
        NA_real_
      )
    )
  )]
}

# drop the wide columns
calibrated_combined[, (suffix_cols) := NULL]

# reorder columns
setcolorder(calibrated_combined, c("state", "mode", "draw", "species", base_names))


# identify non-coverged cells and re-run with expanded floor_sublegal_harvest
library(data.table)

# assume this is your first-pass output in the CURRENT naming format
# one row per state-mode-draw-species
# columns include:
# state, mode, draw, species, floor_used_in,
# keep_to_rel, rel_to_keep, p_rel_to_keep, p_keep_to_rel, convergence,
# MRIP_keep, model_keep, diff_keep, pct_diff_keep, etc.

calibrated_combined <- data.table::as.data.table(calibrated_combined)

# helper for your current long-format output
#' @title Should this stratum be retried with a wider sublegal size window?
#' @description The rel_to_keep reallocation draws its converted fish from
#'   released fish within X inches below the minimum size, starting at X = 3.
#'   A stratum can run out of eligible fish before it closes the harvest gap -
#'   it needs more converted fish than exist in that window. This detects that
#'   case: still short on harvest, still not converged, and reallocating
#'   upward. The caller responds by widening the window to 4 inches and
#'   re-running.
#' @param rel_to_keep Whether this stratum is converting releases to harvest.
#' @param convergence The stratum's current convergence flag.
#' @param diff_keep,pct_diff_keep Remaining harvest difference, fish and percent.
#' @param MRIP_keep MRIP harvest, for the zero-target case.
#' @return TRUE if the stratum should be re-run with the wider window.
needs_floor4_rerun <- function(rel_to_keep, convergence, diff_keep, pct_diff_keep, MRIP_keep) {
  # only rerun rel_to_keep cases that still did not converge
  if (!isTRUE(rel_to_keep == 1)) return(FALSE)
  if (!isTRUE(convergence == 0)) return(FALSE)
  
  # zero-MRIP keep case: use abs diff only
  if (is.finite(MRIP_keep) && MRIP_keep == 0) {
    return(is.finite(diff_keep) && abs(diff_keep) >= 500)
  }
  
  # otherwise use your usual tolerance logic
  keep_bad_abs <- is.finite(diff_keep) && abs(diff_keep) >= 500
  keep_bad_pct <- is.finite(pct_diff_keep) && abs(pct_diff_keep) >= 5
  
  keep_bad_abs || keep_bad_pct || !is.finite(pct_diff_keep)
}

problem_rows <- calibrated_combined[
  , needs_rerun := mapply(
    needs_floor4_rerun,
    rel_to_keep,
    convergence,
    diff_keep,
    pct_diff_keep,
    MRIP_keep
  )
][needs_rerun == TRUE]

# optional: inspect what will be rerun
print(problem_rows[, .(
  state, mode, draw, species, floor_used_in,
  rel_to_keep, p_rel_to_keep,
  MRIP_keep, model_keep, diff_keep, pct_diff_keep,
  convergence
)])

rerun_results <- vector("list", nrow(problem_rows))

if (nrow(problem_rows) > 0) {
  for (rr in seq_len(nrow(problem_rows))) {
    
    row_i <- problem_rows[rr]
    
    # map current long-format names into the scalar objects expected by the rerun script
    s  <- row_i$state
    md <- row_i$mode
    i  <- row_i$draw
    target_species <- row_i$species
    
          
          baseline_targets_current <- baseline_output0[state == s & draw == i & mode == md]
          if (nrow(baseline_targets_current) == 0L) next
          
          if (all(is.na(baseline_targets_current$MRIP_keep))) {
            out <- copy(baseline_targets_current)
            out[, `:=`(
              keep_to_rel_sf = 0, rel_to_keep_sf = 0, p_rel_to_keep_sf = 0, p_keep_to_rel_sf = 0, convergence_sf = NA_real_,
              keep_to_rel_bsb = 0, rel_to_keep_bsb = 0, p_rel_to_keep_bsb = 0, p_keep_to_rel_bsb = 0, convergence_bsb = NA_real_,
              keep_to_rel_scup = 0, rel_to_keep_scup = 0, p_rel_to_keep_scup = 0, p_keep_to_rel_scup = 0, convergence_scup = NA_real_,
              iter_used = 0L
            )]
            rerun_results[[rr]] <- out
            rr <- rr + 1L
            next
          }
          
          states_by_sp <- setNames(vector("list", length(species_vec)), species_vec)
          for (sp in species_vec) {
            base_row <- extract_species_row(baseline_targets_current, sp, md, s, i)
            states_by_sp[[sp]] <- make_state(base_row)
            states_by_sp[[sp]]$best_row <- copy(base_row)
            states_by_sp[[sp]]$best_score <- score_species(
              base_row$diff_keep,
              base_row$pct_diff_keep,
              base_row$diff_catch,
              base_row$pct_diff_catch,
              base_row$MRIP_keep,
              base_row$MRIP_catch
            )
            
            states_by_sp[[sp]]$achieved <- is_achieved(
              base_row$diff_keep,
              base_row$pct_diff_keep,
              base_row$MRIP_keep
            )
          }
          
          iter_used <- 0L
          last_result <- NULL
          
          floor_below_min_in <- 4
          sf_floor_below_min_in   <- 3
          bsb_floor_below_min_in  <- 3
          scup_floor_below_min_in <- 3
          
          if (target_species == "sf")   sf_floor_below_min_in   <- floor_below_min_in
          if (target_species == "bsb")  bsb_floor_below_min_in  <- floor_below_min_in
          if (target_species == "scup") scup_floor_below_min_in <- floor_below_min_in
          
          repeat {
            
            
            push_globals(states_by_sp, target_env = environment())
            source(file.path(code_cd, "calibrate_rec_catch1_final.R"), local = environment())
            
            last_result <- copy(as.data.table(calib_comparison1))
            
            all_done <- TRUE
            for (sp in species_vec) {
              row <- extract_species_row(last_result, sp, md, s, i)
              states_by_sp[[sp]] <- update_bracket(states_by_sp[[sp]], row)
              
              if (!states_by_sp[[sp]]$achieved && states_by_sp[[sp]]$convergence == 1L) {
                all_done <- FALSE
              }
            }
            
            iter_used <- iter_used + 1L
            
            if (all_done || iter_used >= max_iter) break
          }
          
   
          floor_used_in_sf   <- sf_floor_below_min_in
          floor_used_in_bsb  <- bsb_floor_below_min_in
          floor_used_in_scup <- scup_floor_below_min_in
          
          
          final_rows <- rbindlist(lapply(species_vec, function(sp) {
            st <- states_by_sp[[sp]]
            row <- if (!is.null(st$best_row)) copy(st$best_row) else extract_species_row(last_result, sp, md, s, i)
            row
          }), use.names = TRUE, fill = TRUE)
          
          final_rows[, `:=`(
            n_sub_kept_sf      = n_sub_kept_sf,
            prop_sub_kept_sf   = prop_sub_kept_sf,
            n_legal_rel_sf     = n_legal_rel_sf,
            prop_legal_rel_sf  = prop_legal_rel_sf,
            
            n_sub_kept_bsb     = n_sub_kept_bsb,
            prop_sub_kept_bsb  = prop_sub_kept_bsb,
            n_legal_rel_bsb    = n_legal_rel_bsb,
            prop_legal_rel_bsb = prop_legal_rel_bsb,
            
            n_sub_kept_scup     = n_sub_kept_scup,
            prop_sub_kept_scup  = prop_sub_kept_scup,
            n_legal_rel_scup    = n_legal_rel_scup,
            prop_legal_rel_scup = prop_legal_rel_scup, 
            
            floor_used_in_sf   = floor_used_in_sf,
            floor_used_in_bsb  = floor_used_in_bsb,
            floor_used_in_scup = floor_used_in_scup
          )]
          
          # set final convergence based on best-so-far row, not just the last attempted row
          final_rows[species == "sf", `:=`(
            keep_to_rel_sf = as.integer(states_by_sp[["sf"]]$direction == "keep_to_rel"),
            rel_to_keep_sf = as.integer(states_by_sp[["sf"]]$direction == "rel_to_keep"),
            p_rel_to_keep_sf = if (states_by_sp[["sf"]]$direction == "rel_to_keep") states_by_sp[["sf"]]$p else 0,
            p_keep_to_rel_sf = if (states_by_sp[["sf"]]$direction == "keep_to_rel") states_by_sp[["sf"]]$p else 0,
            convergence_sf = as.integer(is_achieved(diff_keep, pct_diff_keep, MRIP_keep))
          )]
          
          final_rows[species == "bsb", `:=`(
            keep_to_rel_bsb = as.integer(states_by_sp[["bsb"]]$direction == "keep_to_rel"),
            rel_to_keep_bsb = as.integer(states_by_sp[["bsb"]]$direction == "rel_to_keep"),
            p_rel_to_keep_bsb = if (states_by_sp[["bsb"]]$direction == "rel_to_keep") states_by_sp[["bsb"]]$p else 0,
            p_keep_to_rel_bsb = if (states_by_sp[["bsb"]]$direction == "keep_to_rel") states_by_sp[["bsb"]]$p else 0,
            convergence_bsb = as.integer(is_achieved(diff_keep, pct_diff_keep, MRIP_keep))
          )]
          
          final_rows[species == "scup", `:=`(
            keep_to_rel_scup = as.integer(states_by_sp[["scup"]]$direction == "keep_to_rel"),
            rel_to_keep_scup = as.integer(states_by_sp[["scup"]]$direction == "rel_to_keep"),
            p_rel_to_keep_scup = if (states_by_sp[["scup"]]$direction == "rel_to_keep") states_by_sp[["scup"]]$p else 0,
            p_keep_to_rel_scup = if (states_by_sp[["scup"]]$direction == "keep_to_rel") states_by_sp[["scup"]]$p else 0,
            convergence_scup = as.integer(is_achieved(diff_keep, pct_diff_keep, MRIP_keep))
          )]
          
          # fill non-target species columns with zeros where still missing
          fill_zero_cols <- c(
            "keep_to_rel_sf","rel_to_keep_sf","p_rel_to_keep_sf","p_keep_to_rel_sf","convergence_sf",
            "keep_to_rel_bsb","rel_to_keep_bsb","p_rel_to_keep_bsb","p_keep_to_rel_bsb","convergence_bsb",
            "keep_to_rel_scup","rel_to_keep_scup","p_rel_to_keep_scup","p_keep_to_rel_scup","convergence_scup",
            "n_sub_kept_scup","prop_sub_kept_scup","n_legal_rel_scup","prop_legal_rel_scup",
            "n_sub_kept_sf","n_legal_rel_sf","prop_sub_kept_sf","prop_legal_rel_sf",
            "n_sub_kept_bsb","n_legal_rel_bsb","prop_sub_kept_bsb","prop_legal_rel_bsb", 
            "floor_used_in_sf", "floor_used_in_bsb", "floor_used_in_scup")
          
          for (cc in intersect(fill_zero_cols, names(final_rows))) {
            set(final_rows, which(is.na(final_rows[[cc]])), cc, 0)
          }
          
          final_rows[, iter_used := iter_used]
          setcolorder(final_rows, c("draw","state", "mode","species","MRIP_rel","model_rel","diff_rel","pct_diff_rel",
                                    "MRIP_keep","model_keep","diff_keep","pct_diff_keep",
                                    "MRIP_catch","model_catch","diff_catch","pct_diff_catch",
                                    "rel_to_keep_new","keep_to_rel_new","p_rel_to_keep_new","p_keep_to_rel_new",
                                    "floor_used_in_sf", "floor_used_in_bsb", "floor_used_in_scup",
                                    setdiff(names(final_rows), c("draw","state", "mode","species","MRIP_rel","model_rel","diff_rel","pct_diff_rel",
                                                                 "MRIP_keep","model_keep","diff_keep","pct_diff_keep",
                                                                 "MRIP_catch","model_catch","diff_catch","pct_diff_catch",
                                                                 "rel_to_keep_new","keep_to_rel_new","p_rel_to_keep_new","p_keep_to_rel_new", 
                                                                 "floor_used_in_sf", "floor_used_in_bsb", "floor_used_in_scup"))))
          
          rerun_results[[rr]] <- final_rows
          rr <- rr + 1L
        }
      }
    
    
  calibrated_combined2 <- rbindlist(rerun_results, use.names = TRUE, fill = TRUE)
    
    drop_cols <- c(
      "rel_to_keep_new", "keep_to_rel_new",
      "p_rel_to_keep_new", "p_keep_to_rel_new"
    )
    
    drop_cols <- intersect(drop_cols, names(calibrated_combined2))
    calibrated_combined2[, (drop_cols) := NULL]
    
    # one row per state-mode-draw is enough, since the final calibration values are wide
    calibrated_combined2 <- unique(calibrated_combined2, by = c("state", "mode", "draw", "species"))
    
    front_cols <- c("state", "mode", "draw")
    front_cols <- intersect(front_cols, names(calibrated_combined2))
    data.table::setcolorder(calibrated_combined2, c(front_cols, setdiff(names(calibrated_combined2), front_cols)))
    
    
    # identify all species suffixes
    species_levels <- c("sf", "bsb", "scup")
    
    # find all columns that have species suffixes
    suffix_pattern <- paste0("(", paste(species_levels, collapse = "|"), ")$")
    cols <- names(calibrated_combined2)
    
    suffix_cols <- cols[grepl(paste0("_", suffix_pattern), cols)]
    
    # get base variable names (remove suffix)
    base_names <- unique(sub(paste0("_", suffix_pattern), "", suffix_cols))
    
    # for each base variable, create a collapsed version
    for (v in base_names) {
      
      new_col <- v
      
      calibrated_combined2[, (new_col) := fifelse(
        species == "sf",  get(paste0(v, "_sf")),
        fifelse(
          species == "bsb", get(paste0(v, "_bsb")),
          fifelse(
            species == "scup", get(paste0(v, "_scup")),
            NA_real_
          )
        )
      )]
    }
    
    # drop the wide columns
    calibrated_combined2[, (suffix_cols) := NULL]
    
    # reorder columns
    #setcolorder(calibrated_combined2, c("state", "mode", "draw", "species", base_names))
    
    
# replace original problematic rows with the rerun rows
if (nrow(calibrated_combined2) > 0) {
  key_cols <- c("state", "mode", "draw", "species")
  
  calibrated_combined_final <- calibrated_combined[
    !calibrated_combined2,
    on = key_cols
  ]
  
  calibrated_combined_final <- data.table::rbindlist(
    list(calibrated_combined_final, calibrated_combined2),
    use.names = TRUE,
    fill = TRUE
  )
}else{
  calibrated_combined_final<- calibrated_combined
}

# optional final sort
data.table::setorderv(calibrated_combined_final, c("state", "mode", "draw", "species"))

# check for any problem rows 
problem_rows <- calibrated_combined_final[
  , needs_rerun := mapply(
    needs_floor4_rerun,
    rel_to_keep,
    convergence,
    diff_keep,
    pct_diff_keep,
    MRIP_keep
  )
][needs_rerun == TRUE]

fst::write_fst(calibrated_combined_final,
               file.path(iterative_input_data_cd,
                         paste0("archive/miscellaneous/calibrated_model_stats.fst")))