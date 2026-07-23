################################################################################
################################################################################
# Script:       run_state_model.R
# Purpose:      Defines run_state_model(), a single parameterized replacement
#               for the nine near-identical recDST/model_run_<ST>.R scripts.
#               Same job as those files - build the regulation calendar for
#               one state under a saved scenario, then run the projection
#               across draws in parallel - but with the state as an argument
#               instead of hardcoded, and with the per-state regulation logic
#               factored out into apply_directed_trips_regs().
# Inputs:       regs_<Run_Name>.csv, projected_catch_at_length_new.csv,
#               L_W_Conversion.csv,
#               directed_trips_calibration_new_<ST>.feather,
#               proj_catch_draws_<ST>_<draw>.feather,
#               proj_year_calendar_adjustments_new_<ST>.csv,
#               base_outcomes_new_<ST>_<draw>_<mode>.CSV,
#               n_choice_occasions_new_<ST>_<mode>_<draw>.feather,
#               calibrated_model_stats_new.rds
# Outputs:      output2_<ST>_<Run_Name>_<timestamp>.csv
#               (note the "output2_" prefix, distinct from the "output_"
#               prefix the production model_run_*.R scripts write - so this
#               file's output does not collide with, or get picked up by,
#               anything reading the normal outputs)
# Dependencies: Packages readr, dplyr, tidyr, feather, data.table, lubridate,
#               stringr, furrr, future, here. Requires
#               apply_directed_trips_regs() to be defined - see below.
# Pipeline:     NOT ON ANY ACTIVE CODE PATH. Nothing sources this file and
#               nothing calls run_state_model(). Run_Model.R sources the nine
#               recDST/model_run_<ST>.R scripts instead, each of which defines
#               its own copy of this logic. Read this file as the intended
#               direction of travel, not as what currently runs.
#
# THREE DEFECTS, all left unfixed per this session's scope:
#   1. apply_directed_trips_regs() is called below but never sourced anywhere
#      in the repo. It is defined in Code/sim/apply_directed_trips_regs.R,
#      which no script sources. Calling run_state_model() as committed raises
#      "could not find function".
#   2. and 3. The two source() calls further down name
#      Code/sim/predict_rec_catch_functions.R and Code/sim/predict_rec_catch.R.
#      Neither exists at that path - the first is only in Code/archive/, the
#      second does not exist under that name anywhere (Code/sim/ has
#      predict_rec_catch_final.R). The nine model_run_*.R scripts carry the
#      same two broken calls, so this is a repo-wide stale reference from a
#      rename in Code/sim, not a defect unique to this file.
#
# Also note two settings that look like leftover test configuration rather
# than production values: workers = 3 (the model_run_*.R scripts use 34) and
# a draw range of 1:5 with the 1:100 line commented out directly above it.
################################################################################
################################################################################

##############################
### Rec model run (all states)
##############################

#' @title Run the projection model for one state
#' @description Parameterized replacement for the per-state model_run_<ST>.R
#'   scripts. Materializes the saved regulation scenario as named objects,
#'   loads the state's catch-at-length, length-weight, directed-trips and
#'   calibration inputs, applies the state's regulation rules, then maps the
#'   projection across draws in parallel and writes one CSV.
#' @param state Two-letter state code, e.g. "MA". Selects every per-state
#'   input file and filters the multi-state tables.
#' @param Run_Name Name of the saved regulation scenario; both selects
#'   saved_regs/regs_<Run_Name>.csv and is stamped onto the output as the
#'   model label.
#' @return Invisibly, the elapsed time. The results themselves are written to
#'   output/output2_<state>_<Run_Name>_<timestamp>.csv as a side effect.
#' @examples
#' \dontrun{
#' run_state_model("MA", "SQ")
#' }
run_state_model <- function(state, Run_Name) {
  
  #Run_Name <- args[1]
  
  saved_regs <- read.csv(here::here(paste0("saved_regs/regs_", Run_Name, ".csv")))
  
  for (a in seq_len(nrow(saved_regs))) {
    obj_name  <- saved_regs$input[a]
    obj_value <- saved_regs$value[a]
    # envir = parent.frame() is the important difference from the
    # model_run_*.R scripts, which run at top level and so can assign()
    # straight into the global environment. Here the assignment happens inside
    # a function, so without this the regulation objects would land in the
    # function's own frame and vanish on return - apply_directed_trips_regs()
    # looks them up in the caller.
    assign(obj_name, obj_value, envir = parent.frame())
  }
  
  print(paste0("start model_", state))
  state1 <- state
  predictions_all <- list()
  
  data_path <- here::here("Data/")
  
  # ---- Size data ----
  size_data <- readr::read_csv(
    file.path(here::here("Data"), "projected_catch_at_length_new.csv"),
    show_col_types = FALSE
  ) %>%
    dplyr::filter(state == !!state)
  
  sf_size_data <- size_data %>%
    dplyr::filter(species == "sf") %>%
    dplyr::filter(!is.na(fitted_prob)) %>%
    dplyr::select(state, fitted_prob, length, draw, mode)
  
  bsb_size_data <- size_data %>%
    dplyr::filter(species == "bsb") %>%
    dplyr::filter(!is.na(fitted_prob)) %>%
    dplyr::select(state, fitted_prob, length, draw, mode)
  
  scup_size_data <- size_data %>%
    dplyr::filter(species == "scup") %>%
    dplyr::filter(!is.na(fitted_prob)) %>%
    dplyr::select(state, fitted_prob, length, draw, mode)
  
  # ---- Length-weight conversion ----
  l_w_conversion <- readr::read_csv(
    file.path(data_path, "L_W_Conversion.csv"),
    show_col_types = FALSE
  ) %>%
    dplyr::filter(state == !!state)
  
  # ---- Directed trips: load and apply date adjustment ----
  directed_trips <- feather::read_feather(
    file.path(data_path, paste0("directed_trips_calibration_new_", state, ".feather"))
  ) %>%
    tibble::tibble() %>%
    dplyr::select(
      mode, date, draw,
      bsb_bag, bsb_min, fluke_bag, fluke_min, scup_bag, scup_min,
      bsb_bag_y2, bsb_min_y2, fluke_bag_y2, fluke_min_y2, scup_bag_y2, scup_min_y2
    ) %>%
    dplyr::mutate(
      date_adj = lubridate::dmy(date),
      date_adj = lubridate::yday(date_adj),
      date_adj = dplyr::case_when(date_adj > 60 ~ date_adj - 1, TRUE ~ date_adj)
    )
  
  # ---- State-specific directed trips overwriting ----
  # DEFECT 1 (see header): apply_directed_trips_regs() is never sourced
  # anywhere in the repo. It lives in Code/sim/apply_directed_trips_regs.R.
  # This is where the per-state case_when chains that model_run_*.R inlines
  # were factored out to - the whole point of this refactor.
  directed_trips <- apply_directed_trips_regs(directed_trips, state)
  
  # ---- Parallel predictions ----
  predictions_out10 <- data.frame()
  message("run_state_model.R: starting ", state, " projection for scenario '",
          Run_Name, "'. NOTE: configured for 3 workers and 5 draws - test ",
          "settings, not the 34 workers / 100 draws the model_run_*.R ",
          "scripts use.")
  set.seed(915)
  #future::plan(future::multisession, workers = 34)
  future::plan(future::multisession, workers = 3)
  get_predictions_out <- function(x) {
    
    print(x)
    
    directed_trips2 <- directed_trips %>%
      dplyr::filter(draw == x)
    
    catch_data <- feather::read_feather(
      file.path(data_path, paste0("proj_catch_draws_", state, "_", x, ".feather"))
    ) %>%
      dplyr::left_join(directed_trips2, by = c("mode", "date", "draw"))
    
    print("catch data read in")
    
    calendar_adjustments <- readr::read_csv(
      file.path(here::here(paste0("Data/proj_year_calendar_adjustments_new_", state, ".csv"))),
      show_col_types = FALSE
    ) %>%
      dplyr::filter(draw == x) %>%
      dplyr::select(-dtrip, -dtrip_y2, -state.x, -state.y, -draw)
    
    base_outcomes0      <- list()
    n_choice_occasions0 <- list()
    
    mode_draw <- c("sh", "pr", "fh")
    for (md in mode_draw) {
      
      base_outcomes0[[md]] <- readr::read_csv(
        file.path(here::here(paste0("Data/base_outcomes_new_", state, "_", x, "_", md, ".CSV")))
      ) %>%
        data.table::as.data.table() %>%
        dplyr::mutate(date_parsed = lubridate::dmy(date)) %>%
        dplyr::select(-date)
      
      n_choice_occasions0[[md]] <- feather::read_feather(
        file.path(data_path, paste0("n_choice_occasions_new_", state, "_", md, "_", x, ".feather"))
      ) %>%
        dplyr::mutate(date_parsed = lubridate::dmy(date)) %>%
        dplyr::select(-date)
    }
    
    base_outcomes      <- dplyr::bind_rows(base_outcomes0)
    n_choice_occasions <- dplyr::bind_rows(n_choice_occasions0) %>%
      dplyr::arrange(date_parsed, mode)
    rm(base_outcomes0, n_choice_occasions0)
    
    base_outcomes <- base_outcomes %>%
      dplyr::arrange(date_parsed, mode, tripid, catch_draw)
    
    check_n_choice_occasions <- n_choice_occasions %>%
      dplyr::select(date_parsed, mode) %>%
      dplyr::distinct()
    
    base_outcomes <- base_outcomes %>%
      dplyr::right_join(check_n_choice_occasions, by = c("date_parsed", "mode"))
    
    # ---- Calibration comparison ----
    calib_comparison <- readRDS(file.path(data_path, "calibrated_model_stats_new.rds")) %>%
      dplyr::filter(state == !!state & draw == x) %>%
      dplyr::rename(
        n_legal_rel_bsb      = n_legal_bsb_rel,
        n_legal_rel_scup     = n_legal_scup_rel,
        n_legal_rel_sf       = n_legal_sf_rel,
        n_sub_kept_bsb       = n_sub_bsb_kept,
        n_sub_kept_sf        = n_sub_sf_kept,
        n_sub_kept_scup      = n_sub_scup_kept,
        prop_legal_rel_bsb   = prop_legal_bsb_rel,
        prop_legal_rel_sf    = prop_legal_sf_rel,
        prop_legal_rel_scup  = prop_legal_scup_rel,
        prop_sub_kept_bsb    = prop_sub_bsb_kept,
        prop_sub_kept_sf     = prop_sub_sf_kept,
        prop_sub_kept_scup   = prop_sub_scup_kept,
        convergence_sf       = sf_convergence,
        convergence_bsb      = bsb_convergence,
        convergence_scup     = scup_convergence
      )
    
    species_suffixes     <- c("sf", "bsb", "scup")
    all_vars             <- names(calib_comparison)
    species_specific_vars <- all_vars[
      stringr::str_detect(all_vars, paste0("(_", species_suffixes, ")$", collapse = "|"))
    ]
    id_vars    <- setdiff(all_vars, species_specific_vars)
    base_names <- unique(stringr::str_replace(species_specific_vars, "_(sf|bsb|scup)$", ""))
    
    calib_comparison <- calib_comparison %>%
      dplyr::select(mode, dplyr::all_of(species_specific_vars)) %>%
      tidyr::pivot_longer(
        cols      = dplyr::all_of(species_specific_vars),
        names_to  = c(".value", "species"),
        names_pattern = "(.*)_(sf|bsb|scup)"
      ) %>%
      dplyr::distinct()
    
    # ---- Size data subsets ----
    sf_size_data2   <- sf_size_data   %>% dplyr::filter(draw == x) %>% dplyr::select(-draw)
    bsb_size_data2  <- bsb_size_data  %>% dplyr::filter(draw == x) %>% dplyr::select(-draw)
    scup_size_data2 <- scup_size_data %>% dplyr::filter(draw == x) %>% dplyr::select(-draw)
    
    # ---- Run predict catch ----
    # DEFECTS 2 and 3 (see header): neither file exists at these paths.
    source(here::here("Code/sim/predict_rec_catch_functions.R"))
    source(here::here("Code/sim/predict_rec_catch.R"))
    
    test <- predict_rec_catch(
      st                  = state,
      dr                  = x,
      directed_trips      = directed_trips2,
      catch_data          = catch_data,
      sf_size_data        = sf_size_data2,
      bsb_size_data       = bsb_size_data2,
      scup_size_data      = scup_size_data2,
      l_w_conversion      = l_w_conversion,
      calib_comparison    = calib_comparison,
      n_choice_occasions  = n_choice_occasions,
      calendar_adjustments = calendar_adjustments,
      base_outcomes       = base_outcomes
    )
    
    test <- test %>%
      dplyr::mutate(
        draw  = c(x),
        model = c(Run_Name)
      )
  }
  
  print("out of loop")
  
  start_time <- Sys.time()
  
  predictions_out10 <- furrr::future_map_dfr(
    #1:100,
    1:5,
    ~{
      data.table::setDTthreads(1)
      get_predictions_out(.x)
    },
    .id     = "draw",
    .options = furrr::furrr_options(seed = TRUE)
  )
  
  readr::write_csv(
    predictions_out10,
    file = here::here(paste0(
      "output/output2_", state, "_", Run_Name, "_",
      format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv"
    ))
  )
  
  end_time <- Sys.time()
  print(end_time - start_time)
}

