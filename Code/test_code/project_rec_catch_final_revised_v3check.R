################################################################################
################################################################################
# Script:       project_rec_catch_final_revised_v3check.R
# Purpose:      Validation counterpart to project_rec_catch_final_revised_v3.R.
#               Runs the same refactored projection but instrumented for
#               checking rather than production - the intent being to confirm
#               that the restructured code reproduces the results of the
#               original Code/sim projection path before the refactor is
#               adopted.
# Inputs:       Same as project_rec_catch_final_revised_v3.R.
# Outputs:      Returned to the caller.
# Dependencies: Packages data.table, fst, readr.
# Pipeline:     Development/QA scratch. Not called by any wrapper.
# Dev paths:    1 hardcoded absolute path to a developer's local machine
#               (C:\), at line 240 (the L_W_Conversion.csv default argument).
#
# Differs from project_rec_catch_final_revised_v3.R in roughly 290 lines, so
# the two are not interchangeable. Which of the pair is authoritative is not
# recorded anywhere in the repo; the "check" suffix and the smaller size
# suggest this one is the narrower validation run, but confirm with the
# authors before relying on either.
################################################################################
################################################################################


# project_rec_catch_final.R
# Efficient projection-year simulation for summer flounder, black sea bass, and scup.
# Designed to mirror calibrate_rec_catch1_final.R while carrying forward calibrated
# reallocation parameters from calibrated_model_stats.csv.
library(data.table)
library(fst)
library(readr)

safe_divide <- function(num, den) {
  ifelse(is.na(den) | den == 0, NA_real_, num / den)
}

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

stable_logsum2 <- function(v1, v0) {
  m <- pmax(v1, v0)
  m + log(exp(v1 - m) + exp(v0 - m))
}
parse_date_any <- function(x) {
  data.table::as.IDate(as.Date(
    x,
    tryFormats = c("%d%b%Y", "%Y-%m-%d", "%m/%d/%Y", "%d/%m/%Y")
  ))
}


read_fst_any <- function(path_candidates) {
  path <- path_candidates[file.exists(path_candidates)][1]
  if (is.na(path)) stop("None of these files exist: ", paste(path_candidates, collapse = "; "))
  as.data.table(fst::read_fst(path))
}

fill_na_numeric <- function(dt, cols = names(dt)) {
  cols <- intersect(cols, names(dt))
  num_cols <- cols[vapply(dt[, ..cols], is.numeric, logical(1))]
  for (cc in num_cols) data.table::set(dt, which(is.na(dt[[cc]])), cc, 0)
  invisible(dt)
}

species_config <- data.table(
  species = c("sf", "bsb", "scup"),
  catch_col = c("sf_cat", "bsb_cat", "scup_cat"),
  bag_col = c("fluke_bag_y2", "bsb_bag_y2", "scup_bag_y2"),
  min_col = c("fluke_min_y2", "bsb_min_y2", "scup_min_y2"),
  keep_col = c("tot_keep_sf_new", "tot_keep_bsb_new", "tot_keep_scup_new"),
  rel_col = c("tot_rel_sf_new", "tot_rel_bsb_new", "tot_rel_scup_new"),
  catch_total_col = c("tot_cat_sf_new", "tot_cat_bsb_new", "tot_cat_scup_new"),
  keep_util_col = c("tot_keep_sf_util", "tot_keep_bsb_util", "tot_keep_scup_util"),
  rel_util_col = c("tot_rel_sf_util", "tot_rel_bsb_util", "tot_rel_scup_util"),
  utility_adjust = c(TRUE, TRUE, FALSE),
  disc_mort = c(0.10, 0.15, 0.15),
  floor_below_min_in = c(3, 3, 3)
)

# One generic simulator replaces the old simulate_mode_sf/bsb/scup functions.
# Returns trip totals and long length counts at the same time.
simulate_species_project <- function(catch_dt,
                                     size_dt,
                                     calib_row,
                                     cfg,
                                     mode_value = NULL) {

  key_cols <- c("date", "mode", "tripid", "catch_draw")
  sp <- cfg$species

  if (!is.null(mode_value)) catch_dt <- catch_dt[mode == mode_value]

  base_keys <- unique(catch_dt[, ..key_cols])
  if (nrow(base_keys) == 0L) {
    return(list(
      trip = data.table(),
      length = data.table()
    ))
  }

  trip_zero <- copy(base_keys)
  trip_zero[, (c(cfg$keep_col, cfg$rel_col, cfg$keep_util_col, cfg$rel_util_col)) := .(0L, 0L, 0L, 0L)]

  pos_dt <- catch_dt[get(cfg$catch_col) > 0,
                     c(key_cols, cfg$catch_col, cfg$bag_col, cfg$min_col),
                     with = FALSE]

  if (nrow(pos_dt) == 0L || nrow(size_dt) == 0L || sum(size_dt$fitted_prob, na.rm = TRUE) <= 0) {
    setkeyv(trip_zero, key_cols)
    return(list(trip = trip_zero, length = data.table()))
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
    prob = size_dt$fitted_prob
  )]

  fish_dt[, posskeep := fifelse(fitted_length >= min_sz, 1L, 0L)]
  setorder(fish_dt, date, mode, tripid, catch_draw, fishid)
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

  trip_pos <- fish_dt[, .(
    keep_n = sum(keep),
    rel_n = sum(release),
    keep_util_n = sum(keep_util),
    rel_util_n = sum(release_util)
  ), by = key_cols]

  setnames(trip_pos,
           old = c("keep_n", "rel_n", "keep_util_n", "rel_util_n"),
           new = c(cfg$keep_col, cfg$rel_col, cfg$keep_util_col, cfg$rel_util_col))

  trip_out <- rbindlist(list(trip_pos, trip_zero[!trip_pos, on = key_cols]), use.names = TRUE, fill = TRUE)
  fill_na_numeric(trip_out, c(cfg$keep_col, cfg$rel_col, cfg$keep_util_col, cfg$rel_util_col))
  setkeyv(trip_out, key_cols)

  length_out <- fish_dt[, .(
    keep_numbers = sum(keep),
    release_numbers = sum(release)
  ), by = c(key_cols, "fitted_length")]
  length_out[, species := sp]
  setnames(length_out, "fitted_length", "length")

  list(trip = trip_out, length = length_out)
}

read_projection_common_inputs <- function(iterative_input_data_cd,
                                          input_data_cd,
                                          states = NULL,
                                          draws = NULL,
                                          size_lookup_file = "baseline_catch_at_length.csv",
                                          calib_file = file.path(iterative_input_data_cd, "archive/miscellaneous/calibrated_model_stats.fst"),
                                          lw_file = file.path("C:/Users/andrew.carr-harris/Desktop/Git/flukeRDM/Data/L_W_Conversion.csv")) {
  
  size_lookup <- data.table::as.data.table(
    readr::read_csv(file.path(input_data_cd, size_lookup_file), show_col_types = FALSE)
  )
  if (!"mode" %in% names(size_lookup)) size_lookup[, mode := NA_character_]
  size_lookup <- size_lookup[!is.na(fitted_prob), .(state, draw, species, mode, fitted_prob, length)]
  if (!is.null(states)) size_lookup <- size_lookup[state %in% states]
  if (!is.null(draws))  size_lookup <- size_lookup[draw %in% draws]
  data.table::setkey(size_lookup, state, draw, species, mode)

  if (file.exists(calib_file) && grepl("\\.fst$", calib_file, ignore.case = TRUE)) {
    calib <- data.table::as.data.table(fst::read_fst(calib_file))
  } else if (file.exists(calib_file)) {
    calib <- data.table::as.data.table(readr::read_csv(calib_file, show_col_types = FALSE))
  } else {
    calib_csv <- file.path(input_data_cd, "calibrated_model_stats.csv")
    calib <- data.table::as.data.table(readr::read_csv(calib_csv, show_col_types = FALSE))
  }
  if (!"all_keep_to_rel" %in% names(calib)) {
    calib[, all_keep_to_rel := as.integer(p_keep_to_rel >= 1)]
  }
  calib[is.na(all_keep_to_rel), all_keep_to_rel := as.integer(p_keep_to_rel >= 1)]
  calib <- calib[, .(state, mode, draw, species, floor_used_in, keep_to_rel, rel_to_keep,
                     p_rel_to_keep, p_keep_to_rel, all_keep_to_rel)]
  if (!is.null(states)) calib <- calib[state %in% states]
  if (!is.null(draws))  calib <- calib[draw %in% draws]
  data.table::setkey(calib, state, draw, species, mode)

  l_w_conversion <- data.table::as.data.table(readr::read_csv(lw_file, show_col_types = FALSE))
  if (!is.null(states) && "state" %in% names(l_w_conversion)) {
    l_w_conversion <- l_w_conversion[state %in% states]
  }
  data.table::setkeyv(l_w_conversion, intersect(c("state", "species", "month"), names(l_w_conversion)))

  list(size_lookup = size_lookup, calib = calib, l_w_conversion = l_w_conversion)
}


compute_projection <- function(st,
                               dr,
                               iterative_input_data_cd,
                               input_data_cd,
                               output_cd = file.path(iterative_input_data_cd, "archive/projection_outputs"),
                               ndraws = 50L,
                               modes = c("sh", "pr", "fh"),
                               run_tag = "final",
                               write_intermediate = TRUE,
                               common_inputs = NULL,
                               base_outcomes_date_tag = "4_16_26") {

  dir.create(output_cd, recursive = TRUE, showWarnings = FALSE)

  # Directed trips contain status quo regulations and projection-year candidate regulations.
  directed_trips <- as.data.table(
    read_fst(file.path(
      iterative_input_data_cd,
      paste0("archive/directed_trips_calibration/directed_trips_calibration_", st, ".fst")
    )))
  
  directed_trips <- directed_trips[draw == dr]

  # If projection regulation columns are absent, use status quo columns as y2 defaults.
  y2_pairs <- list(
    fluke_bag_y2 = "fluke_bag", fluke_min_y2 = "fluke_min",
    bsb_bag_y2   = "bsb_bag",   bsb_min_y2   = "bsb_min",
    scup_bag_y2  = "scup_bag",  scup_min_y2  = "scup_min"
  )
  for (nm in names(y2_pairs)) {
    if (!nm %in% names(directed_trips) && y2_pairs[[nm]] %in% names(directed_trips)) {
      directed_trips[, (nm) := get(y2_pairs[[nm]])]
    }
  }

  keep_reg_cols <- c("mode", "date", names(y2_pairs), unname(unlist(y2_pairs)))
  directed_trips <- unique(directed_trips[, intersect(keep_reg_cols, names(directed_trips)), with = FALSE])

  catch_data <- as.data.table(
    read_fst(file.path(
      iterative_input_data_cd,
      paste0("archive/proj_catch_draws/proj_catch_draws_", st, "_", dr, ".fst")
    )))
  
  catch_data <- merge(catch_data, directed_trips, by = c("mode", "date"), all.x = TRUE)
  catch_by_mode <- split(catch_data, by = "mode", keep.by = TRUE)
  
  # Shared lookup files can be passed through common_inputs so batch runs do not
  # reread them for every state-draw job. If common_inputs is NULL, read them once here.
  if (is.null(common_inputs)) {
    common_inputs <- read_projection_common_inputs(
      iterative_input_data_cd = iterative_input_data_cd,
      input_data_cd = input_data_cd
    )
  }

  size_lookup <- common_inputs$size_lookup[
    state == st & draw == dr & !is.na(fitted_prob),
    .(state, draw, species, mode, fitted_prob, length)
  ]
  
  calib <- common_inputs$calib[
    state == st & draw == dr,
    .(state, mode, draw, species, floor_used_in, keep_to_rel, rel_to_keep,
      p_rel_to_keep, p_keep_to_rel, all_keep_to_rel)
  ]
  
  l_w_conversion <- common_inputs$l_w_conversion[state == st]

  # Base outcomes and n-choice occasions were written by calibrate_rec_catch1_final.R.
  base_list <- list()
  nchoice_list <- list()
  for (md in modes) {
    base_list[[md]] <- fst::read_fst(
      file.path(iterative_input_data_cd, "archive/base_outcomes", paste0("base_outcomes_", st, "_", md, "_", dr, "_", base_outcomes_date_tag, ".fst"))
    )
    nchoice_list[[md]] <- fst::read_fst(
      file.path(iterative_input_data_cd, "archive/n_choice_occasion", paste0("n_choice_occasions_", st, "_", md, "_", dr, "_", base_outcomes_date_tag, ".fst"))
      )
  }

  base_outcomes <- rbindlist(base_list, fill = TRUE, use.names = TRUE)
  n_choice_occasions <- rbindlist(nchoice_list, fill = TRUE, use.names = TRUE)

  util_cols <- grep("^tot_(keep|rel)_.*_util$", names(base_outcomes), value = TRUE)
  
  data.table::setnames(
    base_outcomes,
    old = util_cols,
    new = paste0(util_cols, "_base")
  )
  
  # Harmonize date field. CANDIDTE TO move outside routine
  for (dt in list(base_outcomes, n_choice_occasions, catch_data)) {
    if (!"date_parsed" %in% names(dt)) dt[, date_parsed := parse_date_any(date)]
  }
  if ("date" %in% names(base_outcomes)) base_outcomes[, date := NULL]
  if ("date" %in% names(n_choice_occasions)) n_choice_occasions[, date := NULL]

  # Simulate projected keep/release and length-at-disposition for all species/modes.
  sim_results <- vector("list", nrow(species_config) * length(modes))
  kk <- 0L
  for (md in modes) {
    for (rr in seq_len(nrow(species_config))) {
      cfg <- species_config[rr]
      cfg[, floor_below_min_in := first_non_na(calib[mode == md & species == cfg$species, floor_used_in], cfg$floor_below_min_in)]
      size_dt <- size_lookup[species == cfg$species & (is.na(mode) | mode == md)]
      if (nrow(size_dt) == 0L) size_dt <- size_lookup[species == cfg$species]
      calib_row <- calib[mode == md & species == cfg$species]
      if (!nrow(calib_row)) {
        calib_row <- data.table(rel_to_keep = 0, keep_to_rel = 0, p_rel_to_keep = 0,
                                p_keep_to_rel = 0, all_keep_to_rel = 0)
      }
      kk <- kk + 1L
      
      catch_md <- catch_by_mode[[md]]
      
      if (is.null(catch_md) || nrow(catch_md) == 0L) next
      
      sim_results[[kk]] <- simulate_species_project(
        catch_md, size_dt, calib_row, cfg, mode_value = NULL
      )    }
  }

  trip_parts <- lapply(sim_results, `[[`, "trip")
  length_data <- rbindlist(lapply(sim_results, `[[`, "length"), fill = TRUE, use.names = TRUE)

  key_cols <- c("date", "mode", "tripid", "catch_draw")
  trip_data <- Reduce(function(x, y) merge(x, y, by = key_cols, all = TRUE), trip_parts)
  fill_na_numeric(trip_data)
  trip_data[, `:=`(
    tot_cat_sf_new = tot_keep_sf_new + tot_rel_sf_new,
    tot_cat_bsb_new = tot_keep_bsb_new + tot_rel_bsb_new,
    tot_cat_scup_new = tot_keep_scup_new + tot_rel_scup_new,
    date_parsed = parse_date_any(date)
  )]
  trip_data[, date := NULL]

  trip_data <- merge(
    trip_data,
    base_outcomes,
    by = c("date_parsed", "mode", "tripid", "catch_draw"),
    all = FALSE
  )

  # Corrected projection utility: compute E[U] across catch draws first, then transform to probabilities/CV.
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

    v_optout = beta_opt_out + beta_opt_out_age * age + beta_opt_out_avidity * total_trips_12
  )]

  mean_drop_cols <- intersect(
    c("beta_opt_out", "beta_opt_out_age", "beta_opt_out_avidity",
      "beta_sqrt_bsb_keep", "beta_sqrt_bsb_release", "beta_sqrt_scup_catch",
      "beta_sqrt_sf_bsb_keep", "beta_sqrt_sf_keep", "beta_sqrt_sf_release",
      "age", "cost", "total_trips_12", "catch_draw"),
    names(trip_data)
  )

  mean_trip_data <- copy(trip_data)
  mean_trip_data[, (mean_drop_cols) := NULL]
  mean_vars <- setdiff(names(mean_trip_data), c("date_parsed", "mode", "tripid"))
  mean_trip_data <- mean_trip_data[, lapply(.SD, mean),
                                   by = .(date_parsed, mode, tripid),
                                   .SDcols = mean_vars]

  mean_trip_data[, `:=`(
    prob0 = calc_prob_trip(v0_trip, v_optout),
    probA = calc_prob_trip(vA_trip, v_optout),
    log_sum_alt = stable_logsum2(vA_trip, v_optout),
    log_sum_base = stable_logsum2(v0_trip, v_optout),
    change_CS = -(1 / beta_cost) * (stable_logsum2(vA_trip, v_optout) - stable_logsum2(v0_trip, v_optout))
  )]

  new_cols <- c("tot_keep_sf_new", "tot_rel_sf_new", "tot_cat_sf_new",
                "tot_keep_bsb_new", "tot_rel_bsb_new", "tot_cat_bsb_new",
                "tot_keep_scup_new", "tot_rel_scup_new", "tot_cat_scup_new")
  base_cols <- c("tot_keep_sf_base", "tot_rel_sf_base", "tot_cat_sf_base",
                 "tot_keep_bsb_base", "tot_rel_bsb_base", "tot_cat_bsb_base",
                 "tot_keep_scup_base", "tot_rel_scup_base", "tot_cat_scup_base")

  mean_trip_data[, (new_cols) := lapply(.SD, function(x) x * probA), .SDcols = new_cols]
  mean_trip_data[, (base_cols) := lapply(.SD, function(x) x * prob0), .SDcols = base_cols]

  n_choice_occasions[, month := as.integer(format(date_parsed, "%m"))]
  mean_trip_data[, month := as.integer(format(date_parsed, "%m"))]
  mean_trip_data <- merge(mean_trip_data, n_choice_occasions,
                          by = c("date_parsed", "mode", "month"), all.x = TRUE)
  if (nrow(calendar_adjustments)) {
    mean_trip_data <- merge(mean_trip_data, calendar_adjustments, by = c("mode", "month"), all.x = TRUE)
  } else {
    mean_trip_data[, expansion_factor := 1]
  }
  mean_trip_data[is.na(expansion_factor), expansion_factor := 1]
  mean_trip_data[is.na(n_choice_occasions), n_choice_occasions := 0]
  mean_trip_data[, expand := n_choice_occasions * expansion_factor / ndraws]

  expansion_factors <- mean_trip_data[, .(date_parsed, mode, tripid, expand, probA)]

  scale_cols <- c(new_cols, base_cols, "probA", "prob0", "change_CS")
  mean_trip_data[, (scale_cols) := lapply(.SD, function(x) x * expand), .SDcols = scale_cols]
  setnames(mean_trip_data, c("probA", "prob0"), c("n_trips_alt", "n_trips_base"))

  # Length data remain long; this avoids thousands of wide keep_sf_XX/release_sf_XX columns.
  length_data[, date_parsed := parse_date_any(date)]
  length_data[, date := NULL]
  length_mean <- length_data[, .(
    keep_numbers = mean(keep_numbers),
    release_numbers = mean(release_numbers)
  ), by = .(date_parsed, mode, tripid, species, length)]
  length_mean <- merge(length_mean, expansion_factors, by = c("date_parsed", "mode", "tripid"), all.x = TRUE)
  fill_na_numeric(length_mean, c("keep_numbers", "release_numbers", "expand", "probA"))
  length_mean[, `:=`(
    keep_numbers = keep_numbers * probA * expand,
    release_numbers = release_numbers * probA * expand
  )]

  length_mean[, month := as.integer(format(date_parsed, "%m"))]

  # L_W_Conversion.csv stores length-weight parameters by state/species/month,
  # not one row per length. Join the parameters first, then compute weight at
  # each simulated fish length.
  lw_join_cols <- intersect(c("state", "species", "month"), names(l_w_conversion))
  if ("state" %in% names(l_w_conversion) && !"state" %in% names(length_mean)) {
    length_mean[, state := st]
  }
  length_mean <- merge(length_mean, l_w_conversion, by = lw_join_cols, all.x = TRUE)

  length_mean[, weight := data.table::fcase(
    species == "scup" & "ln_a" %in% names(length_mean) & "b" %in% names(length_mean),
      exp(ln_a + b * log(length)),
    species %chin% c("sf", "bsb") & "a" %in% names(length_mean) & "b" %in% names(length_mean),
      a * length^b,
    default = NA_real_
  )]

  # Old projection code converted kg to lb after the length-weight calculation.
  length_mean[, weight := weight * 2.20462262185]
  fill_na_numeric(length_mean, c("weight"))

  length_mean[, `:=`(
    keep_weight = keep_numbers * weight,
    release_weight = release_numbers * weight
  )]
  
  length_mean <- merge(length_mean, species_config[, .(species, disc_mort)], by = "species", all.x = TRUE)
  length_mean[, `:=`(
    discmort_number = release_numbers * disc_mort,
    discmort_weight = release_weight * disc_mort
  )]

  trip_metrics <- c("change_CS", "n_trips_alt", "n_trips_base", new_cols, base_cols)
  aggregate_mode <- mean_trip_data[, lapply(.SD, sum), by = .(mode), .SDcols = trip_metrics]
  aggregate_all <- mean_trip_data[, lapply(.SD, sum), .SDcols = trip_metrics][, mode := "all modes"]
  model_output <- rbindlist(list(aggregate_mode, aggregate_all), use.names = TRUE, fill = TRUE)

  model_output_long <- melt(
    model_output,
    id.vars = "mode",
    measure.vars = trip_metrics,
    variable.name = "metric",
    value.name = "value"
  )
  model_output_long[, species := NA_character_]

  length_metrics <- c("keep_numbers", "release_numbers", "keep_weight", "release_weight", "discmort_number", "discmort_weight")
  length_mode <- length_mean[, lapply(.SD, sum), by = .(species, mode), .SDcols = length_metrics]
  length_all <- length_mean[, lapply(.SD, sum), by = .(species), .SDcols = length_metrics][, mode := "all modes"]
  length_output <- rbindlist(list(length_mode, length_all), use.names = TRUE, fill = TRUE)
  length_output_long <- melt(length_output,
                             id.vars = c("species", "mode"),
                             measure.vars = length_metrics,
                             variable.name = "metric",
                             value.name = "value")

  predictions <- rbindlist(list(length_output_long, model_output_long), use.names = TRUE, fill = TRUE)
  predictions[, `:=`(state = st, draw = dr, run_tag = run_tag)]
  setcolorder(predictions, c("state", "draw", "run_tag", "mode", "species", "metric", "value"))

  if (write_intermediate) {
    fst::write_fst(mean_trip_data, 
                   file.path(output_cd, paste0("projected_trip_data_", st, "_", dr, "_", run_tag, ".fst")))
    fst::write_fst(length_mean, 
                   file.path(output_cd, paste0("projected_length_data_", st, "_", dr, "_", run_tag, ".fst")))
  }
  
  fst::write_fst(predictions, 
                 file.path(output_cd, paste0("projected_predictions_", st, "_", dr, "_", run_tag, ".fst")))
  
  # data.table::fwrite(predictions, 
  #                    file.path(output_cd, paste0("projected_predictions_", st, "_", dr, "_", run_tag, ".csv")))

  predictions[]
}
