################################################################################
################################################################################
# ARCHIVED - NOT PRODUCTION CODE. See Code/archive/README.md.
# 
# Script:      predict_rec_catch_functions.R
# Purpose:     Per-species helper functions for the projection main loop.
#
#              READ THIS ONE FIRST IF YOU ARE CHASING THE BROKEN PROJECTION PATH.
#              This is the file that Run_Model.R's per-state scripts, and
#              Code/sim/run_state_model.R, still try to source from Code/sim/ - a
#              path where it does not exist. It was moved here and superseded, but
#              the callers were never updated. See FLAGGED_ISSUES_FLUKE.md.
# Superseded by: Code/sim/predict_rec_catch_final.R
# 
# This file is retained for reference and is not called by any wrapper,
# script or app in this repository. It is NOT maintained: paths, data
# formats and modeling choices in it may be years out of date, and it
# should not be used to understand how the pipeline currently behaves.
# Per the documentation session's scope, archived files received a header
# only - no inline documentation, and no code was changed.
################################################################################
################################################################################


#Functions for the main loop of predict_rec_catch


########## summer flounder ##############
simulate_mode_sf <- function(md, calib_lookup, sf_size_data, catch_data) {

  # Extract calibration parameters
  calib_row <- calib_lookup[mode == md]
  
  rel_to_keep_sf     <- calib_row$rel_to_keep_sf
  keep_to_rel_sf     <- calib_row$keep_to_rel_sf
  p_rel_to_keep_sf   <- calib_row$p_rel_to_keep_sf
  p_keep_to_rel_sf   <- calib_row$p_keep_to_rel_sf
  prop_sublegal_kept_sf <- calib_row$prop_sub_kept_sf
  prop_legal_rel_sf     <- calib_row$prop_legal_rel_sf
  all_keep_to_rel_sf <- as.integer(p_keep_to_rel_sf == 1)
  
  
  #  sublegal_harvest_floor
  #directed_trips_md <- directed_trips[mode == md]
  directed_trips_md <- directed_trips %>% dplyr::filter(mode == md)
  floor_subl_sf_harv <- min(directed_trips_md$fluke_min) - 3 * 2.54
  
  # Filter length data by mode
  #sf_size_data1 <- sf_size_data[mode == md]
  sf_size_data1 <- sf_size_data
  
  # Filter catch data by mode
  catch_data_md <- catch_data[mode == md]
  sf_catch_check_md <- sum(catch_data_md$sf_cat)
  
  if (sf_catch_check_md == 0) {
    
    size_data<-catch_data_md %>% 
      dplyr::mutate(mode=md) %>% 
      dplyr::select("mode","tripid", "catch_draw","date_parsed") %>% 
      dplyr::mutate(keep_sf_1=0, release_sf_1=0)
    
    zero_catch <-data.frame(
      date_parsed = character(0),
      catch_draw = numeric(0),
      tripid = numeric(0),
      mode = character(0) ,
      tot_keep_sf_new = numeric(0),
      tot_rel_sf_new = numeric(0)
    )
    
    return(list(
      trip_data = catch_data_md[, .(date_parsed, catch_draw, tripid, mode,
                                    tot_keep_sf_new = 0L, tot_rel_sf_new = 0L,
                                    domain2 = paste0(date_parsed, "_", mode, "_", catch_draw, "_", tripid))],
      zero_catch = zero_catch, 
      size_data=size_data))
  }
if (sf_catch_check_md != 0) {
    
  # Expand fish by number caught
  sf_catch_data <- catch_data_md[sf_cat > 0]
  sf_catch_data <- sf_catch_data[rep(1:.N, sf_cat)]
  sf_catch_data[, fishid := .I]
  
  # Sample fish lengths
  sf_catch_data[, fitted_length := sample(sf_size_data1$length, .N, 
                                          prob = sf_size_data1$fitted_prob, replace = TRUE)]
  
  # Identify keepable fish
  sf_catch_data[, posskeep := as.integer(fitted_length >= fluke_min)]
  sf_catch_data[, csum_keep := ave(posskeep, tripid, date_parsed, mode, catch_draw, FUN = cumsum)]
  sf_catch_data[, keep_adj := as.integer(posskeep == 1 & csum_keep <= fluke_bag)]
  sf_catch_data[, `:=`(keep = keep_adj, release = 1L - keep_adj)]
  sf_catch_data[, subl_harv_indicator := as.integer(release == 1 & fitted_length >= floor_subl_sf_harv)]
  
  # --- Reallocate rel to keep ---
  # if (rel_to_keep_sf == 1 && sum(sf_catch_data$release) > 0) {
  #   sublegal_keeps <- sf_catch_data[subl_harv_indicator == 1]
  #   base <- sf_catch_data[subl_harv_indicator == 0]
  #   
  #   n_to_keep <- round(p_rel_to_keep_sf * nrow(sublegal_keeps))
  #   sublegal_keeps[, uniform := runif(.N)]
  #   data.table::setorder(sublegal_keeps, uniform)
  #   sublegal_keeps[, fishid2 := .I]
  #   sublegal_keeps[, `:=`(
  #     keep = as.integer(fishid2 <= n_to_keep),
  #     release = as.integer(fishid2 > n_to_keep)
  #   )]
  # 
  #   
  #   # Drop helper columns *only if they exist*
  #   cols_to_drop_sub <- intersect(names(sublegal_keeps), c("uniform", "fishid2", "subl_harv_indicator"))
  #   sublegal_keeps[, (cols_to_drop_sub) := NULL]
  #   
  #   cols_to_drop_base <- intersect(names(base), "subl_harv_indicator")
  #   base[, (cols_to_drop_base) := NULL]
  #   
  #   sf_catch_data <- data.table::rbindlist(list(sublegal_keeps, base), use.names = TRUE, fill = TRUE)
  # }
  
  # --- Reallocate keep to rel ---
  # if (keep_to_rel_sf == 1 && sum(sf_catch_data$keep) > 0) {
  #   if (all_keep_to_rel_sf == 1) {
  #     sf_catch_data[, `:=`(release = keep + release, keep = 0L)]
  #   } else {
  #     kept <- sf_catch_data[keep == 1]
  #     base <- sf_catch_data[keep == 0]
  #     n_to_release <- round(p_keep_to_rel_sf * nrow(kept))
  #     
  #     kept[, uniform := runif(.N)]
  #     data.table::setorder(kept, uniform)
  #     kept[, fishid2 := .I]
  #     kept[, `:=`(
  #       release = as.integer(fishid2 <= n_to_release),
  #       keep = as.integer(fishid2 > n_to_release)
  #     )]
  #     kept[, `:=`(uniform = NULL, fishid2 = NULL)]
  #     
  #     sf_catch_data <- data.table::rbindlist(list(kept, base), use.names = TRUE)
  #   }
  # }
  
  # --- Append length-specific keep/release summary ---
  sf_catch_data <- data.table::as.data.table(sf_catch_data)
  
  new_size_data <- sf_catch_data[, .(
    keep = sum(keep),
    release = sum(release)
  ), by = .(mode, date_parsed, catch_draw, tripid, fitted_length)]
  
  keep_size_data <- new_size_data %>%
    dplyr::select(-release) %>%
    tidyr::pivot_wider(
      names_from = fitted_length,
      names_glue = "keep_sf_{fitted_length}",
      names_sort = TRUE,
      values_from = keep,
      values_fill = 0
    )
  
  release_size_data <- new_size_data %>%
    dplyr::select(-keep) %>%
    tidyr::pivot_wider(
      names_from = fitted_length,
      names_glue = "release_sf_{fitted_length}",
      names_sort = TRUE,
      values_from = release,
      values_fill = 0
    )
  
  keep_release_size_data <- keep_size_data %>%
    dplyr::left_join(release_size_data, by = c("date_parsed", "mode", "tripid", "catch_draw"))
  
  
  # Summarize trip-level data
  trip_summary <- sf_catch_data[, .(tot_keep_sf_new = sum(keep), tot_rel_sf_new = sum(release)), 
                                by = .(date_parsed, catch_draw, tripid, mode)]
  
  # Add zero-catch trips
  zero_catch <- catch_data_md[sf_cat == 0, .(date_parsed, catch_draw, tripid, mode)]
  zero_catch[, `:=`(tot_keep_sf_new = 0L, tot_rel_sf_new = 0L)]
  
  trip_data <- data.table::rbindlist(list(trip_summary, zero_catch))
  trip_data[, domain2 := paste0(date_parsed, "_", mode, "_", catch_draw, "_", tripid)]
  
  output_list<- list(
    trip_data = trip_data,
    zero_catch = zero_catch,
    size_data = keep_release_size_data
  )
  return(output_list)
  
}
}

########## black sea bass ##############
simulate_mode_bsb <- function(md, calib_lookup, bsb_size_data, catch_data) {
  
  # Extract calibration parameters
  calib_row <- calib_lookup[mode == md]
  
  rel_to_keep_bsb     <- calib_row$rel_to_keep_bsb
  keep_to_rel_bsb     <- calib_row$keep_to_rel_bsb
  p_rel_to_keep_bsb   <- calib_row$p_rel_to_keep_bsb
  p_keep_to_rel_bsb   <- calib_row$p_keep_to_rel_bsb
  prop_sublegal_kept_bsb <- calib_row$prop_sub_kept_bsb
  prop_legal_rel_bsb     <- calib_row$prop_legal_rel_bsb
  all_keep_to_rel_bsb <- as.integer(p_keep_to_rel_bsb == 1)
  
  #  sublegal_harvest_floor
  directed_trips_md <- directed_trips %>% dplyr::filter(mode == md)
  floor_subl_bsb_harv <- min(directed_trips_md$bsb_min) - 3 * 2.54
  
  # Filter length data by mode
  bsb_size_data1 <- bsb_size_data

  # Filter catch data by mode
  catch_data_md <- catch_data[mode == md]
  bsb_catch_check_md <- sum(catch_data_md$bsb_cat)
  
  if (bsb_catch_check_md == 0) {
    
    size_data<-catch_data_md %>% 
      dplyr::mutate(mode=md) %>% 
      dplyr::select("mode","tripid", "catch_draw","date_parsed") %>% 
      dplyr::mutate(keep_bsb_1=0, release_bsb_1=0)
    
    zero_catch <-data.frame(
      date_parsed = character(0),
      catch_draw = numeric(0),
      tripid = numeric(0),
      mode = character(0) ,
      tot_keep_bsb_new = numeric(0),
      tot_rel_bsb_new = numeric(0)
    )
    
    return(list(
      trip_data = catch_data_md[, .(date_parsed, catch_draw, tripid, mode,
                                    tot_keep_bsb_new = 0L, tot_rel_bsb_new = 0L,
                                    domain2 = paste0(date_parsed, "_", mode, "_", catch_draw, "_", tripid))],
      zero_catch = zero_catch,
      size_data=size_data
    ))
  }

  if (bsb_catch_check_md != 0) {
    
  # Expand fish by number caught
  bsb_catch_data <- catch_data_md[bsb_cat > 0]
  bsb_catch_data <- bsb_catch_data[rep(1:.N, bsb_cat)]
  bsb_catch_data[, fishid := .I]
  
  # Sample fish lengths
  bsb_catch_data[, fitted_length := sample(bsb_size_data1$length, .N, 
                                          prob = bsb_size_data1$fitted_prob, replace = TRUE)]
  
  # Identify keepable fish
  bsb_catch_data[, posskeep := as.integer(fitted_length >= bsb_min)]
  bsb_catch_data[, csum_keep := ave(posskeep, tripid, date_parsed, mode, catch_draw, FUN = cumsum)]
  bsb_catch_data[, keep_adj := as.integer(posskeep == 1 & csum_keep <= bsb_bag)]
  bsb_catch_data[, `:=`(keep = keep_adj, release = 1L - keep_adj)]
  bsb_catch_data[, subl_harv_indicator := as.integer(release == 1 & fitted_length >= floor_subl_bsb_harv)]
  
  # --- Reallocate rel to keep ---
  # if (rel_to_keep_bsb == 1 && sum(bsb_catch_data$release) > 0) {
  #   sublegal_keeps <- bsb_catch_data[subl_harv_indicator == 1]
  #   base <- bsb_catch_data[subl_harv_indicator == 0]
  #   
  #   n_to_keep <- round(p_rel_to_keep_bsb * nrow(sublegal_keeps))
  #   sublegal_keeps[, uniform := runif(.N)]
  #   data.table::setorder(sublegal_keeps, uniform)
  #   sublegal_keeps[, fishid2 := .I]
  #   sublegal_keeps[, `:=`(
  #     keep = as.integer(fishid2 <= n_to_keep),
  #     release = as.integer(fishid2 > n_to_keep)
  #   )]
  # 
  #   
  #   # Drop helper columns *only if they exist*
  #   cols_to_drop_sub <- intersect(names(sublegal_keeps), c("uniform", "fishid2", "subl_harv_indicator"))
  #   sublegal_keeps[, (cols_to_drop_sub) := NULL]
  #   
  #   cols_to_drop_base <- intersect(names(base), "subl_harv_indicator")
  #   base[, (cols_to_drop_base) := NULL]
  #   
  #   bsb_catch_data <- data.table::rbindlist(list(sublegal_keeps, base), use.names = TRUE, fill = TRUE)
  # }
  # 
  # # --- Reallocate keep to rel ---
  # if (keep_to_rel_bsb == 1 && sum(bsb_catch_data$keep) > 0) {
  #   if (all_keep_to_rel_bsb == 1) {
  #     bsb_catch_data[, `:=`(release = keep + release, keep = 0L)]
  #   } else {
  #     kept <- bsb_catch_data[keep == 1]
  #     base <- bsb_catch_data[keep == 0]
  #     n_to_release <- round(p_keep_to_rel_bsb * nrow(kept))
  #     
  #     kept[, uniform := runif(.N)]
  #     data.table::setorder(kept, uniform)
  #     kept[, fishid2 := .I]
  #     kept[, `:=`(
  #       release = as.integer(fishid2 <= n_to_release),
  #       keep = as.integer(fishid2 > n_to_release)
  #     )]
  #     kept[, `:=`(uniform = NULL, fishid2 = NULL)]
  #     
  #     bsb_catch_data <- data.table::rbindlist(list(kept, base), use.names = TRUE)
  #   }
  # }
  
  # --- Append length-specific keep/release summary ---
  bsb_catch_data <- data.table::as.data.table(bsb_catch_data)
  
  new_size_data <- bsb_catch_data[, .(
    keep = sum(keep),
    release = sum(release)
  ), by = .(mode, date_parsed, catch_draw, tripid, fitted_length)]
  
  keep_size_data <- new_size_data %>%
    dplyr::select(-release) %>%
    tidyr::pivot_wider(
      names_from = fitted_length,
      names_glue = "keep_bsb_{fitted_length}",
      names_sort = TRUE,
      values_from = keep,
      values_fill = 0
    )
  
  release_size_data <- new_size_data %>%
    dplyr::select(-keep) %>%
    tidyr::pivot_wider(
      names_from = fitted_length,
      names_glue = "release_bsb_{fitted_length}",
      names_sort = TRUE,
      values_from = release,
      values_fill = 0
    )
  
  keep_release_size_data <- keep_size_data %>%
    dplyr::left_join(release_size_data, by = c("date_parsed", "mode", "tripid", "catch_draw"))
  
  
  # Summarize trip-level data
  trip_summary <- bsb_catch_data[, .(tot_keep_bsb_new = sum(keep), tot_rel_bsb_new = sum(release)), 
                                by = .(date_parsed, catch_draw, tripid, mode)]
  
  # Add zero-catch trips
  zero_catch <- catch_data_md[bsb_cat == 0, .(date_parsed, catch_draw, tripid, mode)]
  zero_catch[, `:=`(tot_keep_bsb_new = 0L, tot_rel_bsb_new = 0L)]
  
  trip_data <- data.table::rbindlist(list(trip_summary, zero_catch))
  trip_data[, domain2 := paste0(date_parsed, "_", mode, "_", catch_draw, "_", tripid)]
  
  output_list<-list(
    trip_data = trip_data,
    zero_catch = zero_catch,
    size_data = keep_release_size_data
  )
  return(output_list)
}
}

########## scup ##############
simulate_mode_scup <- function(md, calib_lookup, scup_size_data, catch_data) {
  
  # Extract calibration parameters
  calib_row <- calib_lookup[mode == md]
  
  rel_to_keep_scup     <- calib_row$rel_to_keep_scup
  keep_to_rel_scup     <- calib_row$keep_to_rel_scup
  p_rel_to_keep_scup   <- calib_row$p_rel_to_keep_scup
  p_keep_to_rel_scup   <- calib_row$p_keep_to_rel_scup
  prop_sublegal_kept_scup <- calib_row$prop_sub_kept_scup
  prop_legal_rel_scup     <- calib_row$prop_legal_rel_scup
  all_keep_to_rel_scup <- as.integer(p_keep_to_rel_scup == 1)
  
  #  sublegal_harvest_floor
  directed_trips_md <- directed_trips %>% dplyr::filter(mode == md)
  floor_subl_scup_harv <- min(directed_trips_md$scup_min) - 3 * 2.54
  
  # Filter length data by mode
  scup_size_data1 <- scup_size_data
  
  # Filter catch data by mode
  catch_data_md <- catch_data[mode == md]
  scup_catch_check_md <- sum(catch_data_md$scup_cat)

  if (scup_catch_check_md == 0) {
    
    size_data<-catch_data_md %>% 
      dplyr::mutate(mode=md) %>% 
      dplyr::select("mode","tripid", "catch_draw","date_parsed") %>% 
      dplyr::mutate(keep_scup_1=0, release_scup_1=0)
    
    zero_catch <-data.frame(
      date_parsed = character(0),
      catch_draw = numeric(0),
      tripid = numeric(0),
      mode = character(0) ,
      tot_keep_scup_new = numeric(0),
      tot_rel_scup_new = numeric(0)
    )

    return(list(
      trip_data = catch_data_md[, .(date_parsed, catch_draw, tripid, mode,
                                    tot_keep_scup_new = 0L, tot_rel_scup_new = 0L,
                                    domain2 = paste0(date_parsed, "_", mode, "_", catch_draw, "_", tripid))],
      size_data = size_data, 
      zero_catch=zero_catch
      
    ))
  }
  
  if (scup_catch_check_md != 0) {
    
  # Expand fish by number caught
  scup_catch_data <- catch_data_md[scup_cat > 0]
  scup_catch_data <- scup_catch_data[rep(1:.N, scup_cat)]
  scup_catch_data[, fishid := .I]
  
  # Sample fish lengths
  scup_catch_data[, fitted_length := sample(scup_size_data1$length, .N, 
                                           prob = scup_size_data1$fitted_prob, replace = TRUE)]
  
  # Identify keepable fish
  scup_catch_data[, posskeep := as.integer(fitted_length >= scup_min)]
  scup_catch_data[, csum_keep := ave(posskeep, tripid, date_parsed, mode, catch_draw, FUN = cumsum)]
  scup_catch_data[, keep_adj := as.integer(posskeep == 1 & csum_keep <= scup_bag)]
  scup_catch_data[, `:=`(keep = keep_adj, release = 1L - keep_adj)]
  scup_catch_data[, subl_harv_indicator := as.integer(release == 1 & fitted_length >= floor_subl_scup_harv)]
  
  # --- Reallocate rel to keep ---
  # if (rel_to_keep_scup == 1 && sum(scup_catch_data$release) > 0) {
  #   sublegal_keeps <- scup_catch_data[subl_harv_indicator == 1]
  #   base <- scup_catch_data[subl_harv_indicator == 0]
  #   
  #   n_to_keep <- round(p_rel_to_keep_scup * nrow(sublegal_keeps))
  #   sublegal_keeps[, uniform := runif(.N)]
  #   data.table::setorder(sublegal_keeps, uniform)
  #   sublegal_keeps[, fishid2 := .I]
  #   sublegal_keeps[, `:=`(
  #     keep = as.integer(fishid2 <= n_to_keep),
  #     release = as.integer(fishid2 > n_to_keep)
  #   )]
  # 
  #   
  #   # Drop helper columns *only if they exist*
  #   cols_to_drop_sub <- intersect(names(sublegal_keeps), c("uniform", "fishid2", "subl_harv_indicator"))
  #   sublegal_keeps[, (cols_to_drop_sub) := NULL]
  #   
  #   cols_to_drop_base <- intersect(names(base), "subl_harv_indicator")
  #   base[, (cols_to_drop_base) := NULL]
  #   
  #   scup_catch_data <- data.table::rbindlist(list(sublegal_keeps, base), use.names = TRUE, fill = TRUE)
  # }
  # 
  # # --- Reallocate keep to rel ---
  # if (keep_to_rel_scup == 1 && sum(scup_catch_data$keep) > 0) {
  #   if (all_keep_to_rel_scup == 1) {
  #     scup_catch_data[, `:=`(release = keep + release, keep = 0L)]
  #   } else {
  #     kept <- scup_catch_data[keep == 1]
  #     base <- scup_catch_data[keep == 0]
  #     n_to_release <- round(p_keep_to_rel_scup * nrow(kept))
  #     
  #     kept[, uniform := runif(.N)]
  #     data.table::setorder(kept, uniform)
  #     kept[, fishid2 := .I]
  #     kept[, `:=`(
  #       release = as.integer(fishid2 <= n_to_release),
  #       keep = as.integer(fishid2 > n_to_release)
  #     )]
  #     kept[, `:=`(uniform = NULL, fishid2 = NULL)]
  #     
  #     scup_catch_data <- data.table::rbindlist(list(kept, base), use.names = TRUE)
  #   }
  # }
  
  # --- Append length-specific keep/release summary ---
  scup_catch_data <- data.table::as.data.table(scup_catch_data)
  
  new_size_data <- scup_catch_data[, .(
    keep = sum(keep),
    release = sum(release)
  ), by = .(mode, date_parsed, catch_draw, tripid, fitted_length)]
  
  keep_size_data <- new_size_data %>%
    dplyr::select(-release) %>%
    tidyr::pivot_wider(
      names_from = fitted_length,
      names_glue = "keep_scup_{fitted_length}",
      names_sort = TRUE,
      values_from = keep,
      values_fill = 0
    )
  
  release_size_data <- new_size_data %>%
    dplyr::select(-keep) %>%
    tidyr::pivot_wider(
      names_from = fitted_length,
      names_glue = "release_scup_{fitted_length}",
      names_sort = TRUE,
      values_from = release,
      values_fill = 0
    )
  
  keep_release_size_data <- keep_size_data %>%
    dplyr::left_join(release_size_data, by = c("date_parsed", "mode", "tripid", "catch_draw"))
  
  
  # Summarize trip-level data
  trip_summary <- scup_catch_data[, .(tot_keep_scup_new = sum(keep), tot_rel_scup_new = sum(release)), 
                                 by = .(date_parsed, catch_draw, tripid, mode)]
  
  # Add zero-catch trips
  zero_catch <- catch_data_md[scup_cat == 0, .(date_parsed, catch_draw, tripid, mode)]
  zero_catch[, `:=`(tot_keep_scup_new = 0L, tot_rel_scup_new = 0L)]
  
  trip_data <- data.table::rbindlist(list(trip_summary, zero_catch))
  trip_data[, domain2 := paste0(date_parsed, "_", mode, "_", catch_draw, "_", tripid)]
  
  output_list<-list(
    trip_data = trip_data,
    zero_catch = zero_catch,
    size_data = keep_release_size_data
  )
  
  return(output_list)
  }
}
