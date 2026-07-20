################################################################################
################################################################################
# Script:       calibrate_rec_catch1_final.R
# Purpose:      Calibration PASS 1 simulation - the same calibration-year trip
#               simulation as PASS 0, but with a reallocation step applied.
#               Where PASS 0 assumes perfect regulatory compliance, this
#               version converts a proportion p of modeled releases into
#               harvest (sublegal fish kept) or a proportion of modeled
#               harvest into releases (legal fish voluntarily released),
#               depending on which direction the model missed MRIP.
#               calibration_routine_final.R sources this file repeatedly,
#               varying p, until simulated harvest lands within tolerance.
# Inputs:       calib_catch_draws_<ST>_<i>.fst, baseline_catch_at_length.csv,
#               L_W_Conversion.csv (read from a hardcoded absolute path), and
#               the MRIP totals, read via the calling environment.
# Outputs:      None written directly. Returns results to the search routine,
#               which persists calibrated_model_stats, base_outcomes and
#               n_choice_occasions files.
# Dependencies: SOURCED, NOT CALLED. calibration_routine_final.R sources this
#               file inside its search loop and passes the current
#               reallocation settings through GLOBAL VARIABLES
#               (rel_to_keep_<sp>, p_rel_to_keep_<sp> and siblings, written by
#               that script's push_globals()). There is no argument-passing
#               contract between the two files. Packages data.table, fst.
# Pipeline:     Inner loop of R calibration STEP 2. Sibling of
#               calibrate_rec_catch0_optimized.R, from which it inherits
#               safe_divide(), calc_prob_trip() and build_compare_table()
#               VERBATIM - those three functions are duplicated across the two
#               files rather than shared. See those definitions in
#               calibrate_rec_catch0_optimized.R for their documentation;
#               only what is new here is documented below.
#
# THE UTILITY ADJUSTMENT - the subtlest thing in this file.
# Reallocation creates a conflict between two uses of the same fish. A fish
# moved from kept to released must count as RELEASED in the harvest and
# discard totals that get compared to MRIP - that is the whole point. But the
# angler in the simulation decided to take the trip believing they would keep
# it, so for the purposes of trip UTILITY it must still count as kept;
# otherwise reallocating fish would silently change anglers' trip-taking
# decisions and the calibration would be adjusting two things at once.
# The file therefore carries two parallel sets of columns:
#   tot_keep_<sp>_new  / tot_rel_<sp>_new   accounting - what MRIP is compared to
#   tot_keep_<sp>_util / tot_rel_<sp>_util  behavioral - what enters utility
# This applies to summer flounder and black sea bass only. Scup enters the
# utility function as TOTAL CATCH (sqrt_scup_catch), so moving a scup between
# kept and released does not change utility and no adjustment is needed.
#
# THE SUBLEGAL WINDOW. Converting releases to harvest is not unrestricted: the
# eligible fish are released fish within `floor_sublegal` inches BELOW the
# minimum size, on the reasoning that an angler who keeps an illegal fish
# keeps a near-legal one, not a tiny one. The window starts at 3 inches and
# the search routine widens it to 4 when a stratum runs out of eligible fish
# (see needs_floor4_rerun() in calibration_routine_final.R).
################################################################################
################################################################################

# calibration-year trip simulation WITH optional trip-level harvest/release reallocation
# optimized for speed/efficiency while retaining fish-level expansion
# utility adjustment: for fluke and black sea bass, fish reallocated from kept->released
# still count as kept in utility calculations, but remain released in harvest/release totals.
# scup enters utility only as total catch, so no keep/release utility adjustment is needed.


# The next three functions - parse_date_any(), safe_divide() and
# calc_prob_trip() - plus build_compare_table() below are duplicated verbatim
# from calibrate_rec_catch0_optimized.R (and parse_date_any() also from
# "R code wrapper.R"). They are documented in those files; a change made here
# will NOT propagate to the copies.
parse_date_any <- function(x) {
  data.table::as.IDate(as.Date(
    x,
    tryFormats = c("%d%b%Y", "%Y-%m-%d", "%m/%d/%Y", "%d/%m/%Y")
  ))
}

safe_divide <- function(num, den) {
  ifelse(is.na(den) | den == 0, NA_real_, num / den)
}

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

build_compare_table <- function(summed_results, MRIP_comparison_draw, md) {
  
  number_metric_cols <- c(
    "sf_keep", "sf_rel", "sf_catch",
    "bsb_keep", "bsb_rel", "bsb_catch",
    "scup_keep", "scup_rel", "scup_catch"
  )
  
  weight_metric_cols <- c(
    "tot_keep_sf_weight_lb_new", "tot_rel_sf_weight_lb_new",
    "tot_keep_bsb_weight_lb_new", "tot_rel_bsb_weight_lb_new",
    "tot_keep_scup_weight_lb_new", "tot_rel_scup_weight_lb_new"
  )
  
  metric_cols <- c(number_metric_cols, weight_metric_cols)
  
  model_metrics  <- intersect(metric_cols, names(summed_results))
  mrip_metrics   <- intersect(number_metric_cols, names(MRIP_comparison_draw))
  
  if (length(model_metrics) == 0L) {
    stop("No model metric columns found in summed_results.")
  }
  
  model_long <- data.table::melt(
    data.table::as.data.table(summed_results)[, c("mode", model_metrics), with = FALSE],
    id.vars = "mode",
    measure.vars = model_metrics,
    variable.name = "metric",
    value.name = "model"
  )
  
  mrip_long <- data.table::melt(
    data.table::as.data.table(MRIP_comparison_draw)[, c("mode", mrip_metrics), with = FALSE],
    id.vars = "mode",
    measure.vars = mrip_metrics,
    variable.name = "metric",
    value.name = "MRIP"
  )
  
  model_long[, model := as.numeric(model)]
  mrip_long[, MRIP := as.numeric(MRIP)]
  
  cmp <- merge(model_long, mrip_long, by = c("mode", "metric"), all.x = TRUE)
  
  cmp[, `:=`(
    species = data.table::fcase(
      grepl("_sf_", metric) | grepl("^sf_", metric), "sf",
      grepl("_bsb_", metric) | grepl("^bsb_", metric), "bsb",
      grepl("_scup_", metric) | grepl("^scup_", metric), "scup",
      default = NA_character_
    ),
    disposition = data.table::fcase(
      grepl("keep", metric), "keep",
      grepl("rel", metric), "rel",
      grepl("catch", metric), "catch",
      default = NA_character_
    ),
    units = data.table::fcase(
      grepl("weight_lb", metric), "lbs",
      default = "numbers"
    )
  )]
  
  cmp[, diff := model - MRIP]
  cmp[, pct_diff := fifelse(!is.na(MRIP) & MRIP != 0, 100 * diff / MRIP, NA_real_)]
  cmp[, abs_diff_val := abs(diff)]
  cmp[, abs_pct_diff_val := fifelse(!is.na(MRIP) & MRIP != 0, abs(100 * diff / MRIP), NA_real_)]
  cmp[, mode := md]
  
  cmp <- cmp[
    species %in% c("sf", "bsb", "scup") &
      disposition %in% c("keep", "rel", "catch"),
    .(species, disposition, units, mode, MRIP, model, diff, pct_diff,
      abs_diff_val, abs_pct_diff_val)
  ]
  
  cmp_num <- cmp[units == "numbers"]
  
  compare_k <- cmp_num[disposition == "keep",
                       .(mode, species,
                         MRIP_keep = MRIP,
                         model_keep = model,
                         diff_keep = diff,
                         pct_diff_keep = pct_diff)
  ]
  
  compare_c <- cmp_num[disposition == "catch",
                       .(mode, species,
                         MRIP_catch = MRIP,
                         model_catch = model,
                         diff_catch = diff,
                         pct_diff_catch = pct_diff)
  ]
  
  compare_r <- cmp_num[disposition == "rel",
                       .(mode, species,
                         MRIP_rel = MRIP,
                         model_rel = model,
                         diff_rel = diff,
                         pct_diff_rel = pct_diff)
  ]
  
  out <- merge(compare_r, compare_k, by = c("mode", "species"), all = TRUE)
  out <- merge(out, compare_c, by = c("mode", "species"), all = TRUE)
  
  cmp_lbs <- cmp[units == "lbs"]
  
  if (nrow(cmp_lbs) > 0L) {
    weight_wide <- data.table::dcast(
      cmp_lbs,
      mode + species ~ disposition,
      value.var = "model"
    )
    
    wt_old <- intersect(c("keep", "rel", "catch"), names(weight_wide))
    wt_new <- paste0("model_", wt_old, "_lbs")
    
    if (length(wt_old) > 0L) {
      data.table::setnames(weight_wide, old = wt_old, new = wt_new)
    }
    
    out <- merge(out, weight_wide, by = c("mode", "species"), all.x = TRUE)
  } else {
    out[, `:=`(
      model_keep_lbs = NA_real_,
      model_rel_lbs = NA_real_
    )]
  }
  
  out[, rel_to_keep_new := fifelse(diff_keep < 0, 1, 0)]
  out[, keep_to_rel_new := fifelse(diff_keep > 0, 1, 0)]
  out[, p_rel_to_keep_new := abs(safe_divide(diff_keep, model_rel))]
  out[, p_keep_to_rel_new := abs(safe_divide(diff_keep, model_keep))]
  
  out[]
}

# initialize defaults if not supplied by the outer routine
species_defaults <- c("sf", "bsb", "scup")
for (sp in species_defaults) {
  if (!exists(paste0("keep_to_rel_", sp), inherits = FALSE)) assign(paste0("keep_to_rel_", sp), 0)
  if (!exists(paste0("rel_to_keep_", sp), inherits = FALSE)) assign(paste0("rel_to_keep_", sp), 0)
  if (!exists(paste0("p_rel_to_keep_", sp), inherits = FALSE)) assign(paste0("p_rel_to_keep_", sp), 0)
  if (!exists(paste0("p_keep_to_rel_", sp), inherits = FALSE)) assign(paste0("p_keep_to_rel_", sp), 0)
  if (!exists(paste0("all_keep_to_rel_", sp), inherits = FALSE)) assign(paste0("all_keep_to_rel_", sp), 0)
}

if (!exists("sf_floor_below_min_in", inherits = FALSE))   sf_floor_below_min_in <- 3
if (!exists("bsb_floor_below_min_in", inherits = FALSE))  bsb_floor_below_min_in <- 3
if (!exists("scup_floor_below_min_in", inherits = FALSE)) scup_floor_below_min_in <- 3

n_sub_kept_scup <- 0L
prop_sub_kept_scup <- 0
n_legal_rel_scup <- 0L
prop_legal_rel_scup <- 0

n_sub_kept_sf <- 0L
prop_sub_kept_sf <- 0
n_legal_rel_sf <- 0L
prop_legal_rel_sf <- 0

n_sub_kept_bsb <- 0L
prop_sub_kept_bsb <- 0
n_legal_rel_bsb <- 0L
prop_legal_rel_bsb <- 0

#' @title Apply bag and size limits, then reallocate harvest or releases
#' @description The PASS 1 counterpart of simulate_species() in
#'   calibrate_rec_catch0_optimized.R. Expands each trip's catch to individual
#'   fish, draws lengths, applies the minimum size and the bag limit in
#'   encounter order - and then moves a proportion of the outcome across the
#'   kept/released line to close the gap against MRIP.
#'
#'   Two directions, mutually exclusive:
#'     rel_to_keep - the model under-harvested. A proportion p_rel_to_keep of
#'       RELEASED fish lying within `floor_sublegal` inches below the minimum
#'       size is reclassified as kept. Restricting to near-legal fish is a
#'       behavioral assumption: an angler who keeps an undersized fish keeps
#'       one just short of legal, not an obviously tiny one. A stratum can
#'       exhaust this pool, which is what triggers the wider-window re-run.
#'     keep_to_rel - the model over-harvested. A proportion p_keep_to_rel of
#'       KEPT fish is reclassified as released, representing voluntary
#'       release of legal fish. all_keep_to_rel short-circuits the degenerate
#'       case where every kept fish is converted.
#'
#'   When utility_adjust is TRUE the function maintains the separate _util
#'   columns described in the file header, so that reallocation changes the
#'   harvest accounting without disturbing anglers' simulated trip decisions.
#' @param catch_dt Trip-level data.table keyed on date, mode, tripid and
#'   catch_draw, carrying catch counts and the regulations in force.
#' @param catch_col,bag_col,min_col Column names holding this species' catch
#'   count, bag limit and minimum size.
#' @param size_dt Catch-at-length lookup with `length` and `fitted_prob`.
#' @param species_prefix One of "sf", "bsb", "scup"; determines output column
#'   names.
#' @param floor_sublegal How many inches below the minimum size a released
#'   fish may be and still be eligible for conversion to harvest. 3 normally,
#'   widened to 4 when a stratum runs short.
#' @param rel_to_keep,keep_to_rel Direction flags; at most one is 1.
#' @param p_rel_to_keep,p_keep_to_rel The proportion to convert, in [0, 1].
#'   Supplied by the search routine through the global environment.
#' @param all_keep_to_rel Flag for the boundary case p = 1 on keep_to_rel.
#' @param utility_adjust Whether to maintain the separate utility columns.
#'   TRUE for summer flounder and black sea bass, FALSE for scup, which enters
#'   utility only as total catch.
#' @return A list with `trip` (the per-trip kept/released/weight columns, plus
#'   the utility columns when requested) and four diagnostics the search uses:
#'   n_sub_kept, n_legal_rel, prop_sub_kept and prop_legal_rel - the counts and
#'   proportions actually converted, which can fall short of what was
#'   requested when the eligible pool runs out.
simulate_species_realloc <- function(catch_dt,
                                     catch_col,
                                     bag_col,
                                     min_col,
                                     size_dt,
                                     species_prefix = c("sf", "bsb", "scup"),
                                     floor_sublegal,
                                     rel_to_keep = 0,
                                     keep_to_rel = 0,
                                     p_rel_to_keep = 0,
                                     p_keep_to_rel = 0,
                                     all_keep_to_rel = 0,
                                     utility_adjust = FALSE) {

  species_prefix <- as.character(species_prefix)[1]
  
  if (!species_prefix %chin% c("sf", "bsb", "scup")) {
    stop("species_prefix must be one of: sf, bsb, scup. Got: ", species_prefix)
  }
  
  key_cols <- c("date_parsed", "mode", "tripid", "catch_draw")
  keep_col <- paste0("tot_keep_", species_prefix, "_new")
  rel_col  <- paste0("tot_rel_", species_prefix, "_new")
  keep_util_col <- paste0("tot_keep_", species_prefix, "_util")
  rel_util_col  <- paste0("tot_rel_", species_prefix, "_util")
  keep_wt_col <- paste0("tot_keep_", species_prefix, "_weight_lb_new")
  rel_wt_col  <- paste0("tot_rel_", species_prefix, "_weight_lb_new")
  
  pos_dt <- catch_dt[get(catch_col) > 0,
                     .(date_parsed, mode, tripid, catch_draw,
                       catch_n = get(catch_col),
                       bag     = get(bag_col),
                       min_sz  = get(min_col))]

  zero_dt <- catch_dt[get(catch_col) == 0,
                      .(date_parsed, mode, tripid, catch_draw)]

  trip_out_zero <- copy(zero_dt)
  
  trip_out_zero[,
    c(keep_col, rel_col, keep_util_col, rel_util_col, keep_wt_col, rel_wt_col) :=
      .(0L, 0L, 0L, 0L, 0, 0)  ]
  
  if (nrow(pos_dt) == 0L) {
    
    setcolorder(trip_out_zero,
      c(key_cols, keep_col, rel_col, keep_util_col, rel_util_col, keep_wt_col, rel_wt_col))
    
    setkeyv(trip_out_zero, key_cols)
    
    return(list(
      trip = trip_out_zero,
      n_sub_kept = 0L,
      n_legal_rel = 0L,
      prop_sub_kept = 0,
      prop_legal_rel = 0
    ))
  }

  fish_dt <- pos_dt[rep(seq_len(.N), catch_n)]
  fish_dt[, fishid := seq_len(.N)]

  fish_dt[, fitted_length := sample(size_dt$length,
                                    .N,
                                    replace = TRUE,
                                    prob = size_dt$fitted_prob)]

  fish_dt[, month := as.integer(format(date_parsed, "%m"))]
  
  fish_dt[
    size_dt,
    fish_weight_lb := i.fish_weight_lb,
    on = .(fitted_length = length, month)
  ]
  
  fish_dt[, posskeep := fifelse(fitted_length >= min_sz, 1L, 0L)]

  setorder(fish_dt, date_parsed, mode, tripid, catch_draw, fishid)
  fish_dt[, csum_keep := cumsum(posskeep), by = key_cols]
  fish_dt[, keep := fifelse(bag > 0 & posskeep == 1L & csum_keep <= bag, 1L, 0L)]
  fish_dt[, release := fifelse(keep == 0L, 1L, 0L)]
  fish_dt[, kept_to_released_flag := 0L]

  fish_dt[, subl_harv_indicator := fifelse(release == 1L & fitted_length >= floor_sublegal, 1L, 0L)]

  n_sub_kept <- 0L
  n_legal_rel <- 0L
  prop_sub_kept <- 0
  prop_legal_rel <- 0

  original_sum_rel <- fish_dt[, sum(release)]
  original_sum_keep <- fish_dt[, sum(keep)]

  if (rel_to_keep == 1 && original_sum_rel > 0) {
    realloc_dt <- fish_dt[subl_harv_indicator == 1L]
    base_dt    <- fish_dt[subl_harv_indicator == 0L]

    original_rel_eligible <- realloc_dt[, sum(release)]

    if (nrow(realloc_dt) > 0L) {
      realloc_dt[, u := runif(.N)]
      setorder(realloc_dt, u)

      n_row_realloc <- nrow(realloc_dt)
      n_sub_kept <- round(p_rel_to_keep * n_row_realloc)
      n_sub_kept <- max(0L, min(n_sub_kept, n_row_realloc))

      realloc_dt[, idx := seq_len(.N)]
      realloc_dt[, keep_new := fifelse(idx <= n_sub_kept, 1L, 0L)]
      realloc_dt[, rel_new  := fifelse(keep_new == 0L, 1L, 0L)]

      n_sub_kept <- realloc_dt[, sum(keep_new)]
      prop_sub_kept <- safe_divide(n_sub_kept, original_rel_eligible)

      realloc_dt[, `:=`(keep = keep_new, release = rel_new)]
      realloc_dt[, c("u", "idx", "keep_new", "rel_new") := NULL]

      fish_dt <- rbindlist(list(realloc_dt, base_dt), use.names = TRUE)
    }
  }

  if (keep_to_rel == 1 && original_sum_keep > 0) {
    if (all_keep_to_rel == 1) {
      n_legal_rel <- fish_dt[, sum(keep)]
      prop_legal_rel <- safe_divide(n_legal_rel, fish_dt[, sum(keep)])
      fish_dt[, kept_to_released_flag := keep]
      fish_dt[, release := keep + release]
      fish_dt[, keep := 0L]
    } else {
      realloc_dt <- fish_dt[keep == 1L]
      base_dt    <- fish_dt[keep == 0L]

      if (nrow(realloc_dt) > 0L) {
        realloc_dt[, u := runif(.N)]
        setorder(realloc_dt, u)
        realloc_dt[, idx := seq_len(.N)]

        n_row_realloc <- nrow(realloc_dt)
        n_legal_rel <- round(p_keep_to_rel * n_row_realloc)
        n_legal_rel <- max(0L, min(n_legal_rel, n_row_realloc))
        prop_legal_rel <- safe_divide(n_legal_rel, fish_dt[, sum(keep)])

        realloc_dt[, rel_new  := fifelse(idx <= n_legal_rel, 1L, 0L)]
        realloc_dt[, keep_new := fifelse(rel_new == 0L, 1L, 0L)]
        realloc_dt[, kept_to_released_flag := fifelse(rel_new == 1L, 1L, 0L)]
        realloc_dt[, `:=`(keep = keep_new, release = rel_new)]
        realloc_dt[, c("u", "idx", "keep_new", "rel_new") := NULL]

        fish_dt <- rbindlist(list(realloc_dt, base_dt), use.names = TRUE)
      }
    }
  }

  if (utility_adjust) {
    fish_dt[, keep_util := fifelse(keep == 1L | kept_to_released_flag == 1L, 1L, 0L)]
    fish_dt[, release_util := fifelse(release == 1L & kept_to_released_flag == 0L, 1L, 0L)]
  } else {
    fish_dt[, keep_util := keep]
    fish_dt[, release_util := release]
  }

  fish_dt[, keep_weight_lb := keep * fish_weight_lb]
  fish_dt[, release_weight_lb := release * fish_weight_lb]
  
  
  trip_out_pos <- fish_dt[, .(
    keep_n = sum(keep),
    rel_n = sum(release),
    keep_util_n = sum(keep_util),
    rel_util_n = sum(release_util), 
    keep_weight_lb = sum(keep_weight_lb, na.rm = TRUE),
    release_weight_lb = sum(release_weight_lb, na.rm = TRUE)
  ), by = key_cols]


  
  
  setnames(
    trip_out_pos,
    old = c("keep_n", "rel_n", "keep_util_n", "rel_util_n", "keep_weight_lb", "release_weight_lb"),
    new = c(keep_col, rel_col, keep_util_col, rel_util_col, keep_wt_col, rel_wt_col)
  )

  trip_out <- rbindlist(list(trip_out_pos, trip_out_zero), use.names = TRUE, fill = TRUE)
  setkeyv(trip_out, key_cols)

  list(
    trip = trip_out,
    n_sub_kept = n_sub_kept,
    n_legal_rel = n_legal_rel,
    prop_sub_kept = prop_sub_kept,
    prop_legal_rel = prop_legal_rel
  )
}

# preload catch-at-length once instead of reading inside the loop
l_w_conversion <- data.table::as.data.table(
  readr::read_csv(
    "C:/Users/andrew.carr-harris/Desktop/Git/flukeRDM/Data/L_W_Conversion.csv",
    show_col_types = FALSE
  )
)

  size_lookup_raw <- data.table::as.data.table(
    readr::read_csv(file.path(input_data_cd, "baseline_catch_at_length.csv"),
                    show_col_types = FALSE))
  
  size_lookup_raw <- size_lookup_raw[
    !is.na(fitted_prob),
    .(state, draw, species, fitted_prob, length)
  ]
  
  # Add months so length-weight can vary by month.
  size_lookup_raw <- size_lookup_raw[
    ,
    .(month = 1:12),
    by = .(state, draw, species, fitted_prob, length)
  ]
  
  size_lookup_raw <- merge(
    size_lookup_raw,
    l_w_conversion,
    by = c("state", "species", "month"),
    all.x = TRUE
  )
  
  size_lookup_raw[, fish_weight_lb := data.table::fcase(
    species == "scup",
    exp(ln_a + b * log(length)) * 2.20462262185,
    species %chin% c("sf", "bsb"),
    a * length^b * 2.20462262185,
    default = NA_real_
  )]
  
  data.table::setkey(size_lookup_raw, state, draw, species, month)


# one state-draw-mode run; expects s, i, md in the parent environment
dtrip_state <- as.data.table(
  read_fst(file.path(
    iterative_input_data_cd,
    paste0("archive/directed_trips_calibration/directed_trips_calibration_", s, ".fst")
  ))
)

dtripz <- dtrip_state[
  draw == i & mode == md,
  .(mode, dtrip, date_parsed, bsb_bag, bsb_min, fluke_bag, fluke_min, scup_bag, scup_min)
]

months_md <- unique(as.integer(format(dtripz$date_parsed, "%m")))

if (nrow(dtripz) == 0L || sum(dtripz$dtrip, na.rm = TRUE) == 0) {

  calib_comparison1 <- data.table(
    mode = md,
    species = c("sf", "bsb", "scup"),
    MRIP_keep = NA_real_,
    model_keep = 0,
    diff_keep = NA_real_,
    pct_diff_keep = NA_real_,
    MRIP_rel = NA_real_,
    model_rel = 0,
    diff_rel = NA_real_,
    pct_diff_rel = NA_real_,
    MRIP_catch = NA_real_,
    model_catch = 0,
    diff_catch = NA_real_,
    pct_diff_catch = NA_real_,
    rel_to_keep_new = NA_real_,
    keep_to_rel_new = NA_real_,
    p_rel_to_keep_new = NA_real_,
    p_keep_to_rel_new = NA_real_,
    draw = i,
    state = s
  )

} else {

  catch_draw_dt <- as.data.table(
    read_fst(file.path(
      iterative_input_data_cd,
      paste0("archive/calib_catch_draws/calib_catch_draws_", s, "_", i, ".fst")
    ))
  )

  catch_data <- merge(catch_draw_dt[mode == md], dtripz, by = c("mode", "date_parsed"), all.x = TRUE)

  angler_dems <- unique(catch_data[, .(date_parsed, mode, tripid, total_trips_12, age, cost)])

  drop_cols <- intersect(c("cost", "total_trips_12", "age"), names(catch_data))
  if (length(drop_cols)) catch_data[, (drop_cols) := NULL]

  sf_size_data <- size_lookup_raw[
    state == s & draw == i & species == "sf" & month %in% months_md,
    .(month, fitted_prob, length, fish_weight_lb)
  ]
  
  bsb_size_data <- size_lookup_raw[
    state == s & draw == i & species == "bsb" & month %in% months_md,
    .(month, fitted_prob, length, fish_weight_lb)
  ]
  
  scup_size_data <- size_lookup_raw[
    state == s & draw == i & species == "scup" & month %in% months_md,
    .(month, fitted_prob, length, fish_weight_lb)
  ]

  floor_subl_sf_harv   <- min(dtripz$fluke_min, na.rm = TRUE) - sf_floor_below_min_in * 2.54
  floor_subl_bsb_harv  <- min(dtripz$bsb_min,   na.rm = TRUE) - bsb_floor_below_min_in * 2.54
  floor_subl_scup_harv <- min(dtripz$scup_min,  na.rm = TRUE) - scup_floor_below_min_in * 2.54

  sf_res <- simulate_species_realloc(
    catch_dt = catch_data,
    catch_col = "sf_cat",
    bag_col = "fluke_bag",
    min_col = "fluke_min",
    size_dt = sf_size_data,
    species_prefix = "sf",
    floor_sublegal = floor_subl_sf_harv,
    rel_to_keep = rel_to_keep_sf,
    keep_to_rel = keep_to_rel_sf,
    p_rel_to_keep = p_rel_to_keep_sf,
    p_keep_to_rel = p_keep_to_rel_sf,
    all_keep_to_rel = all_keep_to_rel_sf,
    utility_adjust = TRUE
  )

  bsb_res <- simulate_species_realloc(
    catch_dt = catch_data,
    catch_col = "bsb_cat",
    bag_col = "bsb_bag",
    min_col = "bsb_min",
    size_dt = bsb_size_data,
    species_prefix = "bsb",
    floor_sublegal = floor_subl_bsb_harv,
    rel_to_keep = rel_to_keep_bsb,
    keep_to_rel = keep_to_rel_bsb,
    p_rel_to_keep = p_rel_to_keep_bsb,
    p_keep_to_rel = p_keep_to_rel_bsb,
    all_keep_to_rel = all_keep_to_rel_bsb,
    utility_adjust = TRUE
  )

  scup_res <- simulate_species_realloc(
    catch_dt = catch_data,
    catch_col = "scup_cat",
    bag_col = "scup_bag",
    min_col = "scup_min",
    size_dt = scup_size_data,
    species_prefix = "scup",
    floor_sublegal = floor_subl_scup_harv,
    rel_to_keep = rel_to_keep_scup,
    keep_to_rel = keep_to_rel_scup,
    p_rel_to_keep = p_rel_to_keep_scup,
    p_keep_to_rel = p_keep_to_rel_scup,
    all_keep_to_rel = all_keep_to_rel_scup,
    utility_adjust = FALSE
  )

  n_sub_kept_sf <- sf_res$n_sub_kept
  n_legal_rel_sf <- sf_res$n_legal_rel
  prop_sub_kept_sf <- sf_res$prop_sub_kept
  prop_legal_rel_sf <- sf_res$prop_legal_rel

  n_sub_kept_bsb <- bsb_res$n_sub_kept
  n_legal_rel_bsb <- bsb_res$n_legal_rel
  prop_sub_kept_bsb <- bsb_res$prop_sub_kept
  prop_legal_rel_bsb <- bsb_res$prop_legal_rel

  n_sub_kept_scup <- scup_res$n_sub_kept
  n_legal_rel_scup <- scup_res$n_legal_rel
  prop_sub_kept_scup <- scup_res$prop_sub_kept
  prop_legal_rel_scup <- scup_res$prop_legal_rel

  key_cols <- c("date_parsed", "mode", "tripid", "catch_draw")
  setkeyv(sf_res$trip, key_cols)
  setkeyv(bsb_res$trip, key_cols)
  setkeyv(scup_res$trip, key_cols)

  trip_data <- merge(sf_res$trip, bsb_res$trip, by = key_cols, all = TRUE)
  trip_data <- merge(trip_data, scup_res$trip, by = key_cols, all = TRUE)

  zero_fill_cols <- intersect(
    c("tot_keep_sf_new", "tot_rel_sf_new", "tot_keep_sf_util", "tot_rel_sf_util",
      "tot_keep_bsb_new", "tot_rel_bsb_new", "tot_keep_bsb_util", "tot_rel_bsb_util",
      "tot_keep_scup_new", "tot_rel_scup_new", "tot_keep_scup_util", "tot_rel_scup_util",
      "tot_keep_sf_weight_lb_new", "tot_rel_sf_weight_lb_new",
      "tot_keep_bsb_weight_lb_new", "tot_rel_bsb_weight_lb_new",
      "tot_keep_scup_weight_lb_new", "tot_rel_scup_weight_lb_new"),
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

  parameters <- unique(trip_data[, .(date_parsed, mode, tripid)])
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

  setkey(parameters, date_parsed, mode, tripid)
  setkey(angler_dems, date_parsed, mode, tripid)
  trip_data <- merge(trip_data, parameters, by = c("date_parsed", "mode", "tripid"), all.x = TRUE)
  trip_data <- merge(trip_data, angler_dems, by = c("date_parsed", "mode", "tripid"), all.x = TRUE)

  setorder(trip_data, date_parsed, mode, tripid, catch_draw)

  baseline_outcomes <- copy(trip_data)
  setnames(
    baseline_outcomes,
    old = c("tot_keep_bsb_new", "tot_keep_scup_new", "tot_keep_sf_new",
            "tot_rel_bsb_new", "tot_rel_scup_new", "tot_rel_sf_new",
            "tot_bsb_catch", "tot_scup_catch", "tot_sf_catch",
            "tot_keep_bsb_weight_lb_new", "tot_keep_scup_weight_lb_new", "tot_keep_sf_weight_lb_new",
            "tot_rel_bsb_weight_lb_new", "tot_rel_scup_weight_lb_new", "tot_rel_sf_weight_lb_new"),
    new = c("tot_keep_bsb_base", "tot_keep_scup_base", "tot_keep_sf_base",
            "tot_rel_bsb_base", "tot_rel_scup_base", "tot_rel_sf_base",
            "tot_cat_bsb_base", "tot_cat_scup_base", "tot_cat_sf_base",
            "tot_keep_bsb_weight_lb_base", "tot_keep_scup_weight_lb_base", "tot_keep_sf_weight_lb_base",
            "tot_rel_bsb_weight_lb_base", "tot_rel_scup_weight_lb_base", "tot_rel_sf_weight_lb_base"),
    skip_absent = TRUE
  )

  fst::write_fst(
    baseline_outcomes,
    file.path(iterative_input_data_cd, 
              paste0("archive/base_outcomes/base_outcomes_", s, "_", md, "_", i, ".fst"))
  )

  trip_data[, `:=`(
    vA_trip =
      beta_sqrt_sf_keep * sqrt(tot_keep_sf_util) +
      beta_sqrt_sf_release * sqrt(tot_rel_sf_util) +
      beta_sqrt_bsb_keep * sqrt(tot_keep_bsb_util) +
      beta_sqrt_bsb_release * sqrt(tot_rel_bsb_util) +
      beta_sqrt_sf_bsb_keep * (sqrt(tot_keep_sf_util) * sqrt(tot_keep_bsb_util)) +
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

  keep_vars <- setdiff(names(mean_trip_data), c("date_parsed", "mode", "tripid"))
  mean_trip_data <- mean_trip_data[, lapply(.SD, mean),
                                   by = .(date_parsed, mode, tripid),
                                   .SDcols = keep_vars]

  mean_trip_data[, probA := calc_prob_trip(vA_trip, vA_optout)]
  mean_trip_data[, c("vA_trip", "vA_optout", "catch_draw",
                     "tot_keep_sf_util", "tot_rel_sf_util",
                     "tot_keep_bsb_util", "tot_rel_bsb_util",
                     "tot_keep_scup_util", "tot_rel_scup_util") := NULL]

  wt_cols <- c(
    "tot_keep_sf_new", "tot_rel_sf_new", "tot_sf_catch",
    "tot_keep_bsb_new", "tot_rel_bsb_new", "tot_bsb_catch",
    "tot_keep_scup_new", "tot_rel_scup_new", "tot_scup_catch",
    "tot_keep_sf_weight_lb_new", "tot_rel_sf_weight_lb_new",
    "tot_keep_bsb_weight_lb_new", "tot_rel_bsb_weight_lb_new",
    "tot_keep_scup_weight_lb_new", "tot_rel_scup_weight_lb_new"
  )

  wt_cols <- intersect(wt_cols, names(mean_trip_data))
  
  mean_trip_data[,
    (wt_cols) := lapply(.SD, function(x) x * probA),
    .SDcols = wt_cols]
  
  mean_trip_data <- merge(mean_trip_data, dtripz, by = c("mode", "date_parsed"), all.x = TRUE)
  drop_reg_cols <- intersect(c("bsb_bag", "bsb_min", "fluke_bag", "fluke_min", "scup_bag", "scup_min"), names(mean_trip_data))
  if (length(drop_reg_cols)) mean_trip_data[, (drop_reg_cols) := NULL]

  mean_trip_data[, mean_prob := mean(probA), by = .(mode, date_parsed)]
  mean_trip_data[is.na(mean_prob) | mean_prob == 0, mean_prob := NA_real_]
  mean_trip_data[, sims := fifelse(!is.na(mean_prob), round(dtrip / mean_prob), 0)]
  mean_trip_data[, expand := sims / n_draws]
  mean_trip_data[, n_choice_occasions := 1]

  expand_cols <- intersect(c(wt_cols, "n_choice_occasions", "probA"), names(mean_trip_data))
  
  mean_trip_data[,
    (expand_cols) := lapply(.SD, function(x) x * expand),
    .SDcols = expand_cols]

  for (j in names(mean_trip_data)) setattr(mean_trip_data[[j]], "label", NULL)

  aggregate_trip_data <- mean_trip_data[, lapply(.SD, sum),
                                        by = .(date_parsed, mode),
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

  n_choice_out <- aggregate_trip_data[, .(date_parsed, mode, n_choice_occasions, estimated_trips)]
  fst::write_fst(
    n_choice_out,
    file.path(iterative_input_data_cd, 
              paste0("archive/n_choice_occasion/n_choice_occasions_", s, "_", md, "_", i,".fst"))
  )

  list_names <- c(
    "bsb_catch", "bsb_keep", "bsb_rel",
    "scup_catch", "scup_keep", "scup_rel",
    "sf_catch", "sf_keep", "sf_rel",
    "estimated_trips", "n_choice_occasions",
    "tot_keep_sf_weight_lb_new", "tot_rel_sf_weight_lb_new",
    "tot_keep_bsb_weight_lb_new", "tot_rel_bsb_weight_lb_new",
    "tot_keep_scup_weight_lb_new", "tot_rel_scup_weight_lb_new"
  )
  
  list_names <- intersect(list_names, names(aggregate_trip_data))
  
  summed_results <- aggregate_trip_data[  ,
    lapply(.SD, sum, na.rm = TRUE),
    by = .(mode),
    .SDcols = list_names  ]

  MRIP_comparison_draw <- as.data.table(MRIP_comparison)[
    draw == i & state == s & mode == md,
    .(mode, sf_keep, sf_rel, sf_catch,
      bsb_keep, bsb_rel, bsb_catch,
      scup_keep, scup_rel, scup_catch)
  ]

  calib_comparison1 <- build_compare_table(summed_results, MRIP_comparison_draw, md)
  calib_comparison1[, `:=`(draw = i, state = s)]
}

