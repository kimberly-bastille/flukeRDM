################################################################################
################################################################################
# Script:       model_run_MA.R
# Purpose:      Runs the projection model for Massachusetts under a saved
#               regulation scenario. Translates the scenario's season
#               open/close dates, bag limits and size limits into a
#               day x mode regulation calendar, loads the state's projected
#               catch-at-length, catch draws and calibration-year outcomes,
#               and runs 100 draws of the projection in parallel, writing one
#               output CSV.
#
#               THIS FILE IS THE CANONICAL REFERENCE for the nine
#               recDST/model_run_<ST>.R scripts. The other eight are
#               near-identical copies differing only in the state code
#               embedded in filenames and regulation object names (SFma* vs
#               SFnj* and so on) and in how many seasons each state defines.
#               Read this file to understand any of them.
# Inputs:       regs_<Run_Name>.csv, projected_catch_at_length_new.csv,
#               L_W_Conversion.csv,
#               directed_trips_calibration_new_MA.feather,
#               proj_catch_draws_MA_<draw>.feather,
#               proj_year_calendar_adjustments_new_MA.csv,
#               base_outcomes_new_MA_<draw>_<mode>.CSV,
#               n_choice_occasions_new_MA_<mode>_<draw>.feather,
#               calibrated_model_stats_new.rds
# Outputs:      output_MA_<Run_Name>_<timestamp>.csv
# Dependencies: Sourced by Run_Model.R, which must already have defined
#               `args`. Packages readr, dplyr, tidyr, feather, data.table,
#               lubridate, stringr, furrr, future, here.
# Pipeline:     Terminal stage. Reads the outputs of the Stata pre-sim stage
#               and the R calibration stage; its own output is read by app.R.
#
# KNOWN BROKEN - see the source() calls inside get_predictions_out(). Both
# named files are missing from Code/sim/, so this script fails on the first
# draw. Details are commented at that line and in Run_Model.R's header.
#
# Naming convention for the scenario objects: the regulation values arrive as
# rows of regs_<Run_Name>.csv and are assign()ed into the environment by name,
# so identifiers like SFmaFH_seas1_op appear below with no visible definition.
# They decompose as <SPECIES><state><MODE>_<field>:
#   SF / BSB / SCUP   species
#   ma                state
#   FH / PR / SH      for-hire, private, shore
#   seas<n>_op/_cl    season n open / close date
#   <n>_bag / <n>_len season n bag limit / minimum length in INCHES
################################################################################
################################################################################

##############################
#### MA Rec model run  ########
##############################

Run_Name <- args[1]

saved_regs<- read.csv(here::here(paste0("saved_regs/regs_", Run_Name, ".csv")))

################################################################################
################################################################################
# Section A: Materialize the scenario as named objects
################################################################################
################################################################################

# Each row of the scenario CSV becomes a variable named by its `input` column.
# This is why the regulation identifiers used later have no explicit
# assignment anywhere in the file - they are created dynamically here. Two
# consequences: a scenario missing a row silently leaves that object
# undefined until something references it (which is what the exists() test
# further down is guarding against), and this loop re-reads the full CSV even
# though Run_Model.R already filtered it to this state.
for (a in seq_len(nrow(saved_regs))) {
  # Extract name and value
  obj_name <- saved_regs$input[a]
  obj_value <- saved_regs$value[a]

  # Assign to object in the environment
  assign(obj_name, obj_value)
}

start_time<- Sys.time()
# directed_trips<- directed_trips %>%  
print("start model_MA")
state1 = "MA"
predictions_all = list()

data_path <- here::here("Data/")


#### Read in size data ####
size_data <- readr::read_csv(file.path(here::here("Data"), "projected_catch_at_length_new.csv"), show_col_types = FALSE)  %>% 
  dplyr::filter(state == "MA")

sf_size_data <- size_data %>% 
  dplyr::filter(species=="sf") %>% 
  dplyr::filter(!is.na(fitted_prob)) %>% 
  dplyr::select(state, fitted_prob, length, draw, mode)
bsb_size_data <- size_data  %>% 
  dplyr::filter(species=="bsb") %>% 
  dplyr::filter(!is.na(fitted_prob)) %>% 
  dplyr::select(state, fitted_prob, length, draw, mode)
scup_size_data <- size_data %>% 
  dplyr::filter(species=="scup") %>% 
  dplyr::filter(!is.na(fitted_prob)) %>% 
  dplyr::select(state,  fitted_prob, length, draw, mode)


l_w_conversion <- readr::read_csv(file.path(data_path, "L_W_Conversion.csv"), show_col_types = FALSE)  %>% 
  dplyr::filter(state=="MA")

#### directed trips ####
directed_trips<-feather::read_feather(file.path(data_path, paste0("directed_trips_calibration_new_MA.feather"))) %>% 
  tibble::tibble() %>%
  dplyr::select(mode, date, draw, bsb_bag, bsb_min, fluke_bag,fluke_min, scup_bag, scup_min,
                bsb_bag_y2, bsb_min_y2, fluke_bag_y2,fluke_min_y2, scup_bag_y2, scup_min_y2) %>% 
  # Regulations are compared on day-of-year rather than on dates, because the
  # scenario's season boundaries and the calibration calendar can fall in
  # different years. The `> 60 ~ date_adj - 1` step is a leap-year alignment:
  # day 60 is Feb 29 in a leap year, so every later day-of-year is shifted
  # back by one to line up with a non-leap calendar. Without it, seasons in a
  # leap calibration year would be offset by a day against the projection year.
  dplyr::mutate(date_adj = lubridate::dmy(date),
                date_adj = lubridate::yday(date_adj),
                date_adj = dplyr::case_when(date_adj > 60 ~ date_adj -1, TRUE ~ date_adj))  %>%
  # The long case_when chains below build the alternative-scenario regulation
  # calendar (the _y2 columns; _y2 means "year 2", the projection year, as
  # opposed to the calibration-year columns read from the feather file).
  #
  # Three conventions make these readable:
  #   1. They CASCADE. The first case_when in each chain ends `TRUE ~ 0` (bags)
  #      or `TRUE ~ 254` (sizes), establishing a closed-season default; every
  #      subsequent one ends `TRUE ~ <the same column>`, preserving what
  #      earlier lines set. Order therefore matters - a later line can only add
  #      to the calendar, never reopen a day an earlier line closed.
  #   2. `* 2.54` converts the scenario's minimum lengths from inches to
  #      centimetres, the unit the catch-at-length data uses.
  #   3. 254 is the closed-season sentinel: 100 inches x 2.54. It matches the
  #      100-inch default in set_regulations.do and means "no fish is legal".
  dplyr::mutate(
    fluke_bag_y2=dplyr::case_when(mode == "fh" & date_adj >= yday(ymd(SFmaFH_seas1_op)) & date_adj <= yday(ymd(SFmaFH_seas1_cl)) ~ as.numeric(SFmaFH_1_bag), TRUE ~ 0),
    fluke_bag_y2=dplyr::case_when(mode == "fh" & date_adj >= yday(ymd(SFmaFH_seas2_op)) & date_adj <= yday(ymd(SFmaFH_seas2_cl)) ~ as.numeric(SFmaFH_2_bag), TRUE ~ fluke_bag_y2),
    fluke_bag_y2=dplyr::case_when(mode == "pr" & date_adj >= yday(ymd(SFmaPR_seas1_op)) & date_adj <= yday(ymd(SFmaPR_seas1_cl)) ~ as.numeric(SFmaPR_1_bag), TRUE ~ fluke_bag_y2),
    fluke_bag_y2=dplyr::case_when(mode == "pr" & date_adj >= yday(ymd(SFmaPR_seas2_op)) & date_adj <= yday(ymd(SFmaPR_seas2_cl)) ~ as.numeric(SFmaPR_2_bag), TRUE ~ fluke_bag_y2),
    fluke_bag_y2=dplyr::case_when(mode == "sh" & date_adj >= yday(ymd(SFmaSH_seas1_op)) & date_adj <= yday(ymd(SFmaSH_seas1_cl)) ~ as.numeric(SFmaSH_1_bag), TRUE ~ fluke_bag_y2),
    fluke_bag_y2=dplyr::case_when(mode == "sh" & date_adj >= yday(ymd(SFmaSH_seas2_op)) & date_adj <= yday(ymd(SFmaSH_seas2_cl)) ~ as.numeric(SFmaSH_2_bag), TRUE ~ fluke_bag_y2),

    fluke_min_y2=dplyr::case_when(mode == "fh" & date_adj >= yday(ymd(SFmaFH_seas1_op)) & date_adj <= yday(ymd(SFmaFH_seas1_cl)) ~ as.numeric(SFmaFH_1_len) * 2.54, TRUE ~ 254),
    fluke_min_y2=dplyr::case_when(mode == "fh" & date_adj >= yday(ymd(SFmaFH_seas2_op)) & date_adj <= yday(ymd(SFmaFH_seas2_cl)) ~ as.numeric(SFmaFH_2_len) * 2.54, TRUE ~ fluke_min_y2),
    fluke_min_y2=dplyr::case_when(mode == "pr" & date_adj >= yday(ymd(SFmaPR_seas1_op)) & date_adj <= yday(ymd(SFmaPR_seas1_cl)) ~ as.numeric(SFmaPR_1_len) * 2.54, TRUE ~ fluke_min_y2),
    fluke_min_y2=dplyr::case_when(mode == "pr" & date_adj >= yday(ymd(SFmaPR_seas2_op)) & date_adj <= yday(ymd(SFmaPR_seas2_cl)) ~ as.numeric(SFmaPR_2_len) * 2.54, TRUE ~ fluke_min_y2),
    fluke_min_y2=dplyr::case_when(mode == "sh" & date_adj >= yday(ymd(SFmaSH_seas1_op)) & date_adj <= yday(ymd(SFmaSH_seas1_cl)) ~ as.numeric(SFmaSH_1_len) * 2.54, TRUE ~ fluke_min_y2),
    fluke_min_y2=dplyr::case_when(mode == "sh" & date_adj >= yday(ymd(SFmaSH_seas2_op)) & date_adj <= yday(ymd(SFmaSH_seas2_cl)) ~ as.numeric(SFmaSH_2_len) * 2.54, TRUE ~ fluke_min_y2))

    # Black sea bass regulations may be set either statewide or separately by
    # mode, depending on what the scenario author entered in the app. The
    # exists() test distinguishes the two: a statewide entry produces
    # BSBma_seas1_op, while mode-specific entries produce BSBmaFH_seas1_op and
    # siblings. Because the scenario objects are created dynamically in
    # Section A, exists() is the only way to tell which form was supplied.
    if (exists("BSBma_seas1_op")) {
      directed_trips<- directed_trips %>%   dplyr::mutate(
      bsb_bag_y2=dplyr::case_when( date_adj >= yday(ymd(BSBma_seas1_op)) & date_adj <= yday(ymd(BSBma_seas1_cl)) ~ as.numeric(BSBma_1_bag), TRUE ~ 0),
      bsb_min_y2=dplyr::case_when(date_adj >= yday(ymd(BSBma_seas1_op)) & date_adj <= yday(ymd(BSBma_seas1_cl)) ~ as.numeric(BSBma_1_len) * 2.54, TRUE ~ 254))
    }else{
      directed_trips<- directed_trips %>%  dplyr::mutate(
      bsb_bag_y2=dplyr::case_when(mode == "fh" & date_adj >= yday(ymd(BSBmaFH_seas1_op)) & date_adj <= yday(ymd(BSBmaFH_seas1_cl)) ~ as.numeric(BSBmaFH_1_bag), TRUE ~ 0),
      bsb_bag_y2=dplyr::case_when(mode == "pr" & date_adj >= yday(ymd(BSBmaPR_seas1_op)) & date_adj <= yday(ymd(BSBmaPR_seas1_cl)) ~ as.numeric(BSBmaPR_1_bag), TRUE ~ bsb_bag_y2),
      bsb_bag_y2=dplyr::case_when(mode == "sh" & date_adj >= yday(ymd(BSBmaSH_seas1_op)) & date_adj <= yday(ymd(BSBmaSH_seas1_cl)) ~ as.numeric(BSBmaSH_1_bag), TRUE ~ bsb_bag_y2),
      bsb_min_y2=dplyr::case_when(mode == "fh" & date_adj >= yday(ymd(BSBmaFH_seas1_op)) & date_adj <= yday(ymd(BSBmaFH_seas1_cl)) ~ as.numeric(BSBmaFH_1_len) * 2.54, TRUE ~ 254),
      bsb_min_y2=dplyr::case_when(mode == "pr" & date_adj >= yday(ymd(BSBmaPR_seas1_op)) & date_adj <= yday(ymd(BSBmaPR_seas1_cl)) ~ as.numeric(BSBmaPR_1_len) * 2.54, TRUE ~ bsb_min_y2),
      bsb_min_y2=dplyr::case_when(mode == "sh" & date_adj >= yday(ymd(BSBmaSH_seas1_op)) & date_adj <= yday(ymd(BSBmaSH_seas1_cl)) ~ as.numeric(BSBmaSH_1_len) * 2.54, TRUE ~ bsb_min_y2))
    }
      
      
    directed_trips<- directed_trips %>%  dplyr::mutate(
      bsb_bag_y2=dplyr::case_when(mode == "fh" & date_adj >= yday(ymd(BSBmaFH_seas2_op)) & date_adj <= yday(ymd(BSBmaFH_seas2_cl)) ~ as.numeric(BSBmaFH_2_bag), TRUE ~ bsb_bag_y2),
      bsb_bag_y2=dplyr::case_when(mode == "pr" & date_adj >= yday(ymd(BSBmaPR_seas2_op)) & date_adj <= yday(ymd(BSBmaPR_seas2_cl)) ~ as.numeric(BSBmaPR_2_bag), TRUE ~ bsb_bag_y2),
      bsb_bag_y2=dplyr::case_when(mode == "sh" & date_adj >= yday(ymd(BSBmaSH_seas2_op)) & date_adj <= yday(ymd(BSBmaSH_seas2_cl)) ~ as.numeric(BSBmaSH_2_bag), TRUE ~ bsb_bag_y2),
      
      bsb_min_y2=dplyr::case_when(mode == "fh" & date_adj >= yday(ymd(BSBmaFH_seas2_op)) & date_adj <= yday(ymd(BSBmaFH_seas2_cl)) ~ as.numeric(BSBmaFH_2_len) * 2.54, TRUE ~ bsb_min_y2),
      bsb_min_y2=dplyr::case_when(mode == "pr" & date_adj >= yday(ymd(BSBmaPR_seas2_op)) & date_adj <= yday(ymd(BSBmaPR_seas2_cl)) ~ as.numeric(BSBmaPR_2_len) * 2.54, TRUE ~ bsb_min_y2),
      bsb_min_y2=dplyr::case_when(mode == "sh" & date_adj >= yday(ymd(BSBmaSH_seas2_op)) & date_adj <= yday(ymd(BSBmaSH_seas2_cl)) ~ as.numeric(BSBmaSH_2_len) * 2.54, TRUE ~ bsb_min_y2), 

      scup_bag_y2=dplyr::case_when(mode == "fh" & date_adj >= yday(ymd(SCUPmaFH_seas1_op)) & date_adj <= yday(ymd(SCUPmaFH_seas1_cl)) ~ as.numeric(SCUPmaFH_1_bag), TRUE ~ 0),
      scup_bag_y2=dplyr::case_when(mode == "fh" & date_adj >= yday(ymd(SCUPmaFH_seas2_op)) & date_adj <= yday(ymd(SCUPmaFH_seas2_cl)) ~ as.numeric(SCUPmaFH_2_bag), TRUE ~ scup_bag_y2),
      scup_bag_y2=dplyr::case_when(mode == "fh" & date_adj >= yday(ymd(SCUPmaFH_seas3_op)) & date_adj <= yday(ymd(SCUPmaFH_seas3_cl)) ~ as.numeric(SCUPmaFH_3_bag), TRUE ~ scup_bag_y2),
      scup_bag_y2=dplyr::case_when(mode == "pr" & date_adj >= yday(ymd(SCUPmaPR_seas1_op)) & date_adj <= yday(ymd(SCUPmaPR_seas1_cl)) ~ as.numeric(SCUPmaPR_1_bag), TRUE ~ scup_bag_y2),
      scup_bag_y2=dplyr::case_when(mode == "pr" & date_adj >= yday(ymd(SCUPmaPR_seas2_op)) & date_adj <= yday(ymd(SCUPmaPR_seas2_cl)) ~ as.numeric(SCUPmaPR_2_bag), TRUE ~ scup_bag_y2),
      scup_bag_y2=dplyr::case_when(mode == "sh" & date_adj >= yday(ymd(SCUPmaSH_seas1_op)) & date_adj <= yday(ymd(SCUPmaSH_seas1_cl)) ~ as.numeric(SCUPmaSH_1_bag), TRUE ~ scup_bag_y2),
      scup_bag_y2=dplyr::case_when(mode == "sh" & date_adj >= yday(ymd(SCUPmaSH_seas2_op)) & date_adj <= yday(ymd(SCUPmaSH_seas2_cl)) ~ as.numeric(SCUPmaSH_2_bag), TRUE ~ scup_bag_y2),

      scup_min_y2=dplyr::case_when(mode == "fh" & date_adj >= yday(ymd(SCUPmaFH_seas1_op)) & date_adj <= yday(ymd(SCUPmaFH_seas1_cl)) ~ as.numeric(SCUPmaFH_1_len) * 2.54, TRUE ~ 254),
      scup_min_y2=dplyr::case_when(mode == "fh" & date_adj >= yday(ymd(SCUPmaFH_seas2_op)) & date_adj <= yday(ymd(SCUPmaFH_seas2_cl)) ~ as.numeric(SCUPmaFH_2_len) * 2.54, TRUE ~ scup_min_y2),
      scup_min_y2=dplyr::case_when(mode == "fh" & date_adj >= yday(ymd(SCUPmaFH_seas3_op)) & date_adj <= yday(ymd(SCUPmaFH_seas3_cl)) ~ as.numeric(SCUPmaFH_3_len) * 2.54, TRUE ~ scup_min_y2),
      scup_min_y2=dplyr::case_when(mode == "pr" & date_adj >= yday(ymd(SCUPmaPR_seas1_op)) & date_adj <= yday(ymd(SCUPmaPR_seas1_cl)) ~ as.numeric(SCUPmaPR_1_len) * 2.54, TRUE ~ scup_min_y2),
      scup_min_y2=dplyr::case_when(mode == "pr" & date_adj >= yday(ymd(SCUPmaPR_seas2_op)) & date_adj <= yday(ymd(SCUPmaPR_seas2_cl)) ~ as.numeric(SCUPmaPR_2_len) * 2.54, TRUE ~ scup_min_y2),
      scup_min_y2=dplyr::case_when(mode == "sh" & date_adj >= yday(ymd(SCUPmaSH_seas1_op)) & date_adj <= yday(ymd(SCUPmaSH_seas1_cl)) ~ as.numeric(SCUPmaSH_1_len) * 2.54, TRUE ~ scup_min_y2),
      scup_min_y2=dplyr::case_when(mode == "sh" & date_adj >= yday(ymd(SCUPmaSH_seas2_op)) & date_adj <= yday(ymd(SCUPmaSH_seas2_cl)) ~ as.numeric(SCUPmaSH_2_len) * 2.54, TRUE ~ scup_min_y2))


#   readr::write_csv(directed_trips, file = here::here(paste0("output/MA_directed_trips_", Run_Name, ".csv")))

################################################################################
################################################################################
# Section B: Run the projection, one worker per draw
################################################################################
################################################################################

message("model_run_MA.R: starting Massachusetts projection for scenario '", Run_Name, "', 100 draws across 34 parallel workers. Expect a long run.")

    predictions_out10 <- data.frame()
    #future::plan(future::multisession, workers = 36)
# 915 is a fixed seed unrelated to the pipeline's $seed; it makes a given
# scenario reproducible. Note that furrr_options(seed = TRUE) below generates
# per-worker streams from it, which is what keeps parallel draws independent.
set.seed(915)
# 34 workers is hardcoded and sized for the deployment server. On a machine
# with fewer cores this oversubscribes badly; each worker also loads its own
# copy of the state's data, so memory scales with this number.
future::plan(future::multisession, workers = 34)
#' @title Run one projection draw for Massachusetts
#' @description Worker function mapped over draw numbers. For a single draw it
#'   assembles that draw's inputs - the regulation calendar, projected catch
#'   draws, calibration-year trip outcomes, choice occasions, calendar
#'   adjustments and the calibration reallocation proportions - reshapes the
#'   calibration statistics to long form by species, and calls
#'   predict_rec_catch() to produce the projected outcomes.
#' @param x Draw number. Selects the matching per-draw input files and filters
#'   every multi-draw table to that draw.
#' @return A data frame of projected outcomes for this draw, tagged with the
#'   draw number and the scenario name.
get_predictions_out<- function(x){
    #for(x in 1:25){
      
      print(x)
      
      directed_trips2 <- directed_trips %>% 
        dplyr::filter(draw == x) # %>%
      # dplyr::mutate(day = stringr::str_extract(day, "^\\d{2}"), 
      #               period2 = paste0(month24, "-", day, "-", mode))
      
      catch_data <- feather::read_feather(file.path(data_path, paste0("proj_catch_draws_MA", "_", x,".feather"))) %>% 
        dplyr::left_join(directed_trips2, by=c("mode", "date", "draw")) 
      print("catch data read in")
      
      calendar_adjustments <- readr::read_csv(
        file.path(here::here(paste0("Data/proj_year_calendar_adjustments_new_MA.csv"))), show_col_types = FALSE) %>% 
        dplyr::filter(draw == x) %>% 
        dplyr::select(-dtrip, -dtrip_y2, -state.x, -state.y, -draw)
      
      
      base_outcomes0 <- list()
      n_choice_occasions0 <- list()
      
      mode_draw <- c("sh", "pr", "fh")
      for (md in mode_draw) {
        
        # pull trip outcomes from the calibration year
        base_outcomes0[[md]]<-readr::read_csv(file.path(here::here(paste0("Data/base_outcomes_new_MA_", x, "_", md, ".CSV")))) %>% 
          data.table::as.data.table()
        
        base_outcomes0[[md]]<-base_outcomes0[[md]] %>% 
          dplyr::mutate(date_parsed=lubridate::dmy(date)) %>% 
          dplyr::select(-date)
        
        # pull in data on the number of choice occasions per mode-day
        n_choice_occasions0[[md]]<-feather::read_feather(file.path(data_path, paste0("n_choice_occasions_new_MA_", md, "_", x, ".feather")))  
        n_choice_occasions0[[md]]<-n_choice_occasions0[[md]] %>% 
          dplyr::mutate(date_parsed=lubridate::dmy(date)) %>% 
          dplyr::select(-date)
        
      }
      
      base_outcomes <- dplyr::bind_rows(base_outcomes0)
      n_choice_occasions <- dplyr::bind_rows(n_choice_occasions0) %>% 
        dplyr::arrange(date_parsed, mode)
      rm(base_outcomes0, n_choice_occasions0)
      
      base_outcomes<-base_outcomes %>% 
        dplyr::arrange(date_parsed, mode, tripid, catch_draw)
      
      check_n_choice_occasions <- n_choice_occasions %>% 
        dplyr::select(date_parsed, mode) %>%
        dplyr::distinct() 
      
      base_outcomes<-base_outcomes %>% 
        dplyr::right_join(check_n_choice_occasions, by=c("date_parsed", "mode"))
      
      # Pull in calibration comparison information about trip-level harvest/discard re-allocations 
      calib_comparison<-readRDS(file.path(data_path,"calibrated_model_stats_new.rds")) %>%
        dplyr::filter(state=="MA" & draw==x )  
      
      calib_comparison<-calib_comparison %>% 
        dplyr::rename(n_legal_rel_bsb=n_legal_bsb_rel, 
                      n_legal_rel_scup=n_legal_scup_rel, 
                      n_legal_rel_sf=n_legal_sf_rel, 
                      n_sub_kept_bsb=n_sub_bsb_kept,
                      n_sub_kept_sf=n_sub_sf_kept,
                      n_sub_kept_scup=n_sub_scup_kept,
                      prop_legal_rel_bsb=prop_legal_bsb_rel,
                      prop_legal_rel_sf=prop_legal_sf_rel,
                      prop_legal_rel_scup=prop_legal_scup_rel,
                      prop_sub_kept_bsb=prop_sub_bsb_kept,
                      prop_sub_kept_sf=prop_sub_sf_kept,
                      prop_sub_kept_scup=prop_sub_scup_kept,
                      convergence_sf=sf_convergence,
                      convergence_bsb=bsb_convergence,
                      convergence_scup=scup_convergence) 
      
      ##########
      # List of species suffixes
      species_suffixes <- c("sf", "bsb", "scup")
      
      # Get all variable names
      all_vars <- names(calib_comparison)
      
      # Identify columns that are species-specific (contain _sf, _bsb, or _scup)
      species_specific_vars <- all_vars[
        stringr::str_detect(all_vars, paste0("(_", species_suffixes, ")$", collapse = "|"))
      ]
      
      id_vars <- setdiff(all_vars, species_specific_vars)
      
      calib_comparison<-calib_comparison %>% 
        dplyr::select(mode, all_of(species_specific_vars))
      
      # Extract base variable names (without _sf, _bsb, _scup)
      base_names <- unique(stringr::str_replace(species_specific_vars, "_(sf|bsb|scup)$", ""))
      
      # Pivot the data longer on the species-specific columns
      calib_comparison <- calib_comparison %>%
        tidyr::pivot_longer(
          cols = all_of(species_specific_vars),
          names_to = c(".value", "species"),
          names_pattern = "(.*)_(sf|bsb|scup)"
        ) %>% 
        dplyr::distinct()
      
      sf_size_data2 <- sf_size_data %>% 
        dplyr::filter(draw == x) %>%  #Change to X for model for sf and scup
        dplyr::select(-draw)
      
      ### Change when bsb_size is updated
      bsb_size_data2 <- bsb_size_data %>% 
        dplyr::filter(draw == x) %>% 
        dplyr::select(-draw)
      
      scup_size_data2 <- scup_size_data %>% 
        dplyr::filter(draw == x) %>% 
        dplyr::select(-draw)
      
      
      ## Run the predict catch function
      # BROKEN AS COMMITTED - neither file exists at these paths.
      # predict_rec_catch_functions.R exists only in Code/archive/;
      # predict_rec_catch.R does not exist anywhere (Code/sim/ has
      # predict_rec_catch_final.R instead). Every model_run_*.R carries the
      # same two lines, so no state can complete a run. Left unfixed per this
      # session's scope; see Run_Model.R's header.
      source(here::here("Code/sim/predict_rec_catch_functions.R"))
      source(here::here("Code/sim/predict_rec_catch.R"))
      
      test<- predict_rec_catch(st = "MA", dr = x,
                               directed_trips = directed_trips2, 
                               catch_data = catch_data, 
                               sf_size_data = sf_size_data2,
                               bsb_size_data = bsb_size_data2, 
                               scup_size_data = scup_size_data2, 
                               l_w_conversion = l_w_conversion,
                               calib_comparison = calib_comparison, 
                               n_choice_occasions = n_choice_occasions, 
                               calendar_adjustments = calendar_adjustments, 
                               base_outcomes = base_outcomes)
      
      test <- test %>%
        dplyr::mutate(draw = c(x),
                      #model = c("Alt"))
                      model = c(Run_Name))
      # 
      # #regs <- # Input table will be used to fill out regs in DT
      # 
      # predictions_out10<- predictions_out10 %>% rbind(test)
    }
    
    
    print("out of loop")
    
    
    
    # use furrr package to parallelize the get_predictions_out function 100 times
    # This will spit out a dataframe with 100 predictions 
    # setDTthreads(1) inside each worker is essential: without it every
    # data.table operation would itself try to use all cores, and 34 workers
    # each spawning that many threads would thrash the machine.
    # The draw count is hardcoded to 100 and does not read n_simulations.
    predictions_out10<- furrr::future_map_dfr(
      1:100,
      ~{
        data.table::setDTthreads(1)
        get_predictions_out(.x)
      },
      .id = "draw", 
      .options = furrr::furrr_options(seed = TRUE)
    )
    #predictions_out10<- furrr::future_map_dfr(1:25, ~get_predictions_out(.), .id = "draw")
    
    #readr::write_csv(predictions_out10, file = here::here(paste0("output/output_MA_", Run_Name, "_", format(Sys.time(), "%Y%m%d_%H%M%S"),  ".csv")))
    readr::write_csv(predictions_out10, file = here::here(paste0("output/output_MA_", Run_Name, "_", format(Sys.time(), "%Y%m%d_%H%M%S"),  ".csv")))
    
    
    end_time <- Sys.time()
    
    print(end_time - start_time)




