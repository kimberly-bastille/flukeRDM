################################################################################
################################################################################
# Script:       model_run_CT.R
# Purpose:      Runs the projection model for Connecticut under a saved
#               regulation scenario, then writes one output CSV.
#
#               NEAR-DUPLICATE - SEE model_run_MA.R FOR THE FULL EXPLANATION.
#               The nine recDST/model_run_<ST>.R scripts share one structure.
#               model_run_MA.R is documented as the canonical reference and
#               explains the parts that are not self-evident: how the scenario
#               CSV becomes named objects via assign(), the cascading
#               case_when chains that build the regulation calendar, the
#               leap-year day-of-year alignment, the inches-to-centimetres
#               conversion and the 254 closed-season sentinel, the exists()
#               test for statewide vs mode-specific black sea bass rules, and
#               the parallel-draw setup. None of that is repeated here.
#
#               What differs in THIS file: the state code embedded in every
#               filename and regulation object name (ct rather than ma,
#               e.g. SFctFH_seas1_op), and the number of seasons the
#               state defines - 3 summer flounder seasons, 3 black sea bass, 4 scup. It also has two exists() branches.
#               Everything else, including the 100 draws, 34 workers and seed
#               915, is identical to model_run_MA.R.
# Inputs:       regs_<Run_Name>.csv, projected_catch_at_length_new.csv,
#               L_W_Conversion.csv,
#               directed_trips_calibration_new_CT.feather,
#               proj_catch_draws_CT_<draw>.feather,
#               proj_year_calendar_adjustments_new_CT.csv,
#               base_outcomes_new_CT_<draw>_<mode>.CSV,
#               n_choice_occasions_new_CT_<mode>_<draw>.feather,
#               calibrated_model_stats_new.rds
# Outputs:      output_CT_<Run_Name>_<timestamp>.csv
# Dependencies: Sourced by Run_Model.R, which must already have defined
#               `args`. Packages readr, dplyr, tidyr, feather, data.table,
#               lubridate, stringr, furrr, future, here.
# Pipeline:     Terminal stage. Reads the outputs of the Stata pre-sim stage
#               and the R calibration stage; its own output is read by app.R.
#
# KNOWN BROKEN - like every sibling, this script sources
# Code/sim/predict_rec_catch_functions.R and Code/sim/predict_rec_catch.R,
# neither of which exists at those paths, so it fails on the first draw. See
# Run_Model.R's header.
################################################################################
################################################################################

##############################
### CT Rec model run  ########
##############################

# INDICATOR


Run_Name <- args[1]

saved_regs<- read.csv(here::here(paste0("saved_regs/regs_", Run_Name, ".csv")))

for (a in seq_len(nrow(saved_regs))) {
  # Extract name and value
  obj_name <- saved_regs$input[a]
  obj_value <- saved_regs$value[a]
  
  # Assign to object in the environment
  assign(obj_name, obj_value)
}

print("start model_CT")
state1 = "CT"
predictions_all = list()

data_path <- here::here("Data/")

size_data <- readr::read_csv(file.path(here::here("Data"), "projected_catch_at_length_new.csv"), show_col_types = FALSE)  %>% 
  dplyr::filter(state == "CT")

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
  dplyr::filter(state=="CT")

#### directed trips ####
directed_trips<-feather::read_feather(file.path(data_path, paste0("directed_trips_calibration_new_CT.feather"))) %>% 
  tibble::tibble() %>%
  dplyr::select(mode, date, draw, bsb_bag, bsb_min, fluke_bag,fluke_min, scup_bag, scup_min,
                bsb_bag_y2, bsb_min_y2, fluke_bag_y2,fluke_min_y2, scup_bag_y2, scup_min_y2) %>% 
  dplyr::mutate(date_adj = lubridate::dmy(date), 
                date_adj = lubridate::yday(date_adj), 
                date_adj = dplyr::case_when(date_adj > 60 ~ date_adj -1, TRUE ~ date_adj)) 

if (exists("SFct_seas1_op")) {
  directed_trips<- directed_trips %>%
    dplyr::mutate(#Summer Flounder
      fluke_bag_y2=dplyr::case_when(date_adj >= yday(ymd(SFct_seas1_op)) & date_adj <= yday(ymd(SFct_seas1_cl)) ~ as.numeric(SFct_1_bag), TRUE ~ 0), 
      fluke_bag_y2=dplyr::case_when(date_adj >= yday(ymd(SFct_seas2_op)) & date_adj <= yday(ymd(SFct_seas2_cl)) ~ as.numeric(SFct_2_bag), TRUE ~ fluke_bag_y2), 
      fluke_min_y2=dplyr::case_when(date_adj >= yday(ymd(SFct_seas1_op)) & date_adj <= yday(ymd(SFct_seas1_cl)) ~ as.numeric(SFct_1_len) * 2.54, TRUE ~ 254),
      fluke_min_y2=dplyr::case_when(date_adj >= yday(ymd(SFct_seas2_op)) & date_adj <= yday(ymd(SFct_seas2_cl)) ~ as.numeric(SFct_2_len) * 2.54, TRUE ~ fluke_min_y2))
} else {
  directed_trips<- directed_trips %>%
    dplyr::mutate(
      fluke_bag_y2=dplyr::case_when(mode == "fh" & date_adj >= yday(ymd(SFctFH_seas1_op)) & date_adj <= yday(ymd(SFctFH_seas1_cl)) ~ as.numeric(SFctFH_1_bag), TRUE ~ 0), 
      fluke_bag_y2=dplyr::case_when(mode == "pr" & date_adj >= yday(ymd(SFctPR_seas1_op)) & date_adj <= yday(ymd(SFctPR_seas1_cl)) ~ as.numeric(SFctPR_1_bag), TRUE ~ fluke_bag_y2), 
      fluke_bag_y2=dplyr::case_when(mode == "sh" & date_adj >= yday(ymd(SFctSH_seas1_op)) & date_adj <= yday(ymd(SFctSH_seas1_cl)) ~ as.numeric(SFctSH_1_bag), TRUE ~ fluke_bag_y2),
      
      fluke_bag_y2=dplyr::case_when(mode == "fh" & date_adj >= yday(ymd(SFctFH_seas2_op)) & date_adj <= yday(ymd(SFctFH_seas2_cl)) ~ as.numeric(SFctFH_2_bag), TRUE ~ fluke_bag_y2),
      fluke_bag_y2=dplyr::case_when(mode == "pr" & date_adj >= yday(ymd(SFctPR_seas2_op)) & date_adj <= yday(ymd(SFctPR_seas2_cl)) ~ as.numeric(SFctPR_2_bag), TRUE ~ fluke_bag_y2),
      fluke_bag_y2=dplyr::case_when(mode == "sh" & date_adj >= yday(ymd(SFctSH_seas2_op)) & date_adj <= yday(ymd(SFctSH_seas2_cl)) ~ as.numeric(SFctSH_2_bag), TRUE ~ fluke_bag_y2), 
      
      fluke_min_y2=dplyr::case_when(mode == "fh" & date_adj >= yday(ymd(SFctFH_seas1_op)) & date_adj <= yday(ymd(SFctFH_seas1_cl)) ~ as.numeric(SFctFH_1_len) * 2.54, TRUE ~ 254), 
      fluke_min_y2=dplyr::case_when(mode == "pr" & date_adj >= yday(ymd(SFctPR_seas1_op)) & date_adj <= yday(ymd(SFctPR_seas1_cl)) ~ as.numeric(SFctPR_1_len) * 2.54, TRUE ~ fluke_min_y2), 
      fluke_min_y2=dplyr::case_when(mode == "sh" & date_adj >= yday(ymd(SFctSH_seas1_op)) & date_adj <= yday(ymd(SFctSH_seas1_cl)) ~ as.numeric(SFctSH_1_len) * 2.54, TRUE ~ fluke_min_y2),
      
      fluke_min_y2=dplyr::case_when(mode == "fh" & date_adj >= yday(ymd(SFctFH_seas2_op)) & date_adj <= yday(ymd(SFctFH_seas2_cl)) ~ as.numeric(SFctFH_2_len) * 2.54, TRUE ~ fluke_min_y2),
      fluke_min_y2=dplyr::case_when(mode == "pr" & date_adj >= yday(ymd(SFctPR_seas2_op)) & date_adj <= yday(ymd(SFctPR_seas2_cl)) ~ as.numeric(SFctPR_2_len) * 2.54, TRUE ~ fluke_min_y2),
      fluke_min_y2=dplyr::case_when(mode == "sh" & date_adj >= yday(ymd(SFctSH_seas2_op)) & date_adj <= yday(ymd(SFctSH_seas2_cl)) ~ as.numeric(SFctSH_2_len) * 2.54, TRUE ~ fluke_min_y2))
  
}


if (exists("BSBct_seas1_op")) {
  directed_trips<- directed_trips %>%
    dplyr::mutate(# Black Sea Bass Bag Limit
      bsb_bag_y2=dplyr::case_when(date_adj >= yday(ymd(BSBct_seas1_op)) & date_adj <= yday(ymd(BSBct_seas1_cl)) ~ as.numeric(BSBct_1_bag), TRUE ~ 0), 
      bsb_bag_y2=dplyr::case_when(date_adj >= yday(ymd(BSBct_seas2_op)) & date_adj <= yday(ymd(BSBct_seas2_cl)) ~ as.numeric(BSBct_2_bag), TRUE ~ bsb_bag_y2),
      bsb_min_y2=dplyr::case_when(date_adj >= yday(ymd(BSBct_seas1_op)) & date_adj <= yday(ymd(BSBct_seas1_cl)) ~ as.numeric(BSBct_1_len) * 2.54, TRUE ~ 254), 
      bsb_min_y2=dplyr::case_when(date_adj >= yday(ymd(BSBct_seas2_op)) & date_adj <= yday(ymd(BSBct_seas2_cl)) ~ as.numeric(BSBct_2_len) * 2.54, TRUE ~ bsb_min_y2))
} else {
  directed_trips<- directed_trips %>%
    dplyr::mutate(# Black Sea Bass Bag Limit by Mode
      bsb_bag_y2=dplyr::case_when(mode == "fh" & date_adj >= yday(ymd(BSBctFH_seas1_op)) & date_adj <= yday(ymd(BSBctFH_seas1_cl)) ~ as.numeric(BSBctFH_1_bag), TRUE ~ 0), 
      bsb_bag_y2=dplyr::case_when(mode == "pr" & date_adj >= yday(ymd(BSBctPR_seas1_op)) & date_adj <= yday(ymd(BSBctPR_seas1_cl)) ~ as.numeric(BSBctPR_1_bag), TRUE ~ bsb_bag_y2), 
      bsb_bag_y2=dplyr::case_when(mode == "sh" & date_adj >= yday(ymd(BSBctSH_seas1_op)) & date_adj <= yday(ymd(BSBctSH_seas1_cl)) ~ as.numeric(BSBctSH_1_bag), TRUE ~ bsb_bag_y2),
      bsb_bag_y2=dplyr::case_when(mode == "fh" & date_adj >= yday(ymd(BSBctFH_seas2_op)) & date_adj <= yday(ymd(BSBctFH_seas2_cl)) ~ as.numeric(BSBctFH_2_bag), TRUE ~ bsb_bag_y2), 
      bsb_bag_y2=dplyr::case_when(mode == "pr" & date_adj >= yday(ymd(BSBctPR_seas2_op)) & date_adj <= yday(ymd(BSBctPR_seas2_cl)) ~ as.numeric(BSBctPR_2_bag), TRUE ~ bsb_bag_y2), 
      bsb_bag_y2=dplyr::case_when(mode == "sh" & date_adj >= yday(ymd(BSBctSH_seas2_op)) & date_adj <= yday(ymd(BSBctSH_seas2_cl)) ~ as.numeric(BSBctSH_2_bag), TRUE ~ bsb_bag_y2),
      # Black Sea Bass Minimum Length by Mode
      bsb_min_y2=dplyr::case_when(mode == "fh" & date_adj >= yday(ymd(BSBctFH_seas1_op)) & date_adj <= yday(ymd(BSBctFH_seas1_cl)) ~ as.numeric(BSBctFH_1_len) * 2.54, TRUE ~ 254), 
      bsb_min_y2=dplyr::case_when(mode == "pr" & date_adj >= yday(ymd(BSBctPR_seas1_op)) & date_adj <= yday(ymd(BSBctPR_seas1_cl)) ~ as.numeric(BSBctPR_1_len) * 2.54, TRUE ~ bsb_min_y2), 
      bsb_min_y2=dplyr::case_when(mode == "sh" & date_adj >= yday(ymd(BSBctSH_seas1_op)) & date_adj <= yday(ymd(BSBctSH_seas1_cl)) ~ as.numeric(BSBctSH_1_len) * 2.54, TRUE ~ bsb_min_y2),
      bsb_min_y2=dplyr::case_when(mode == "fh" & date_adj >= yday(ymd(BSBctFH_seas2_op)) & date_adj <= yday(ymd(BSBctFH_seas2_cl)) ~ as.numeric(BSBctFH_2_len) * 2.54, TRUE ~ bsb_min_y2), 
      bsb_min_y2=dplyr::case_when(mode == "pr" & date_adj >= yday(ymd(BSBctPR_seas2_op)) & date_adj <= yday(ymd(BSBctPR_seas2_cl)) ~ as.numeric(BSBctPR_2_len) * 2.54, TRUE ~ bsb_min_y2), 
      bsb_min_y2=dplyr::case_when(mode == "sh" & date_adj >= yday(ymd(BSBctSH_seas2_op)) & date_adj <= yday(ymd(BSBctSH_seas2_cl)) ~ as.numeric(BSBctSH_2_len) * 2.54, TRUE ~ bsb_min_y2))
}



directed_trips<- directed_trips %>%  
  dplyr::mutate(
    fluke_bag_y2=dplyr::case_when(mode == "fh" & date_adj >= yday(ymd(SFctFH_seas3_op)) & date_adj <= yday(ymd(SFctFH_seas3_cl)) ~ as.numeric(SFctFH_3_bag), TRUE ~ fluke_bag_y2),
    fluke_bag_y2=dplyr::case_when(mode == "pr" & date_adj >= yday(ymd(SFctPR_seas3_op)) & date_adj <= yday(ymd(SFctPR_seas3_cl)) ~ as.numeric(SFctPR_3_bag), TRUE ~ fluke_bag_y2),
    fluke_bag_y2=dplyr::case_when(mode == "sh" & date_adj >= yday(ymd(SFctSH_seas3_op)) & date_adj <= yday(ymd(SFctSH_seas3_cl)) ~ as.numeric(SFctSH_3_bag), TRUE ~ fluke_bag_y2), 
    
    fluke_min_y2=dplyr::case_when(mode == "fh" & date_adj >= yday(ymd(SFctFH_seas3_op)) & date_adj <= yday(ymd(SFctFH_seas3_cl)) ~ as.numeric(SFctFH_3_len) * 2.54, TRUE ~ fluke_min_y2),
    fluke_min_y2=dplyr::case_when(mode == "pr" & date_adj >= yday(ymd(SFctPR_seas3_op)) & date_adj <= yday(ymd(SFctPR_seas3_cl)) ~ as.numeric(SFctPR_3_len) * 2.54, TRUE ~ fluke_min_y2),
    fluke_min_y2=dplyr::case_when(mode == "sh" & date_adj >= yday(ymd(SFctSH_seas3_op)) & date_adj <= yday(ymd(SFctSH_seas3_cl)) ~ as.numeric(SFctSH_3_len) * 2.54, TRUE ~ fluke_min_y2), 
    
    
    bsb_bag_y2=dplyr::case_when(mode == "fh" & date_adj >= yday(ymd(BSBctFH_seas3_op)) & date_adj <= yday(ymd(BSBctFH_seas3_cl)) ~ as.numeric(BSBctFH_3_bag), TRUE ~ bsb_bag_y2), 
    bsb_bag_y2=dplyr::case_when(mode == "pr" & date_adj >= yday(ymd(BSBctPR_seas3_op)) & date_adj <= yday(ymd(BSBctPR_seas3_cl)) ~ as.numeric(BSBctPR_3_bag), TRUE ~ bsb_bag_y2), 
    bsb_bag_y2=dplyr::case_when(mode == "sh" & date_adj >= yday(ymd(BSBctSH_seas3_op)) & date_adj <= yday(ymd(BSBctSH_seas3_cl)) ~ as.numeric(BSBctSH_3_bag), TRUE ~ bsb_bag_y2), 
    
    bsb_min_y2=dplyr::case_when(mode == "fh" & date_adj >= yday(ymd(BSBctFH_seas3_op)) & date_adj <= yday(ymd(BSBctFH_seas3_cl)) ~ as.numeric(BSBctFH_3_len) * 2.54, TRUE ~ bsb_min_y2), 
    bsb_min_y2=dplyr::case_when(mode == "pr" & date_adj >= yday(ymd(BSBctPR_seas3_op)) & date_adj <= yday(ymd(BSBctPR_seas3_cl)) ~ as.numeric(BSBctPR_3_len) * 2.54, TRUE ~ bsb_min_y2), 
    bsb_min_y2=dplyr::case_when(mode == "sh" & date_adj >= yday(ymd(BSBctSH_seas3_op)) & date_adj <= yday(ymd(BSBctSH_seas3_cl)) ~ as.numeric(BSBctSH_3_len) * 2.54, TRUE ~ bsb_min_y2),
    
    scup_bag_y2=dplyr::case_when(mode == "fh" & date_adj >= yday(ymd(SCUPctFH_seas1_op)) & date_adj <= yday(ymd(SCUPctFH_seas1_cl)) ~ as.numeric(SCUPctFH_1_bag), TRUE ~ 0), 
    scup_min_y2=dplyr::case_when(mode == "fh" & date_adj >= yday(ymd(SCUPctFH_seas1_op)) & date_adj <= yday(ymd(SCUPctFH_seas1_cl)) ~ as.numeric(SCUPctFH_1_len) * 2.54, TRUE ~ 254),
    scup_bag_y2=dplyr::case_when(mode == "fh" & date_adj >= yday(ymd(SCUPctFH_seas2_op)) & date_adj <= yday(ymd(SCUPctFH_seas2_cl)) ~ as.numeric(SCUPctFH_2_bag), TRUE ~ scup_bag_y2),
    scup_min_y2=dplyr::case_when(mode == "fh" & date_adj >= yday(ymd(SCUPctFH_seas2_op)) & date_adj <= yday(ymd(SCUPctFH_seas2_cl)) ~ as.numeric(SCUPctFH_2_len) * 2.54, TRUE ~ scup_min_y2),
    scup_bag_y2=dplyr::case_when(mode == "fh" & date_adj >= yday(ymd(SCUPctFH_seas3_op)) & date_adj <= yday(ymd(SCUPctFH_seas3_cl)) ~ as.numeric(SCUPctFH_3_bag), TRUE ~ scup_bag_y2),
    scup_min_y2=dplyr::case_when(mode == "fh" & date_adj >= yday(ymd(SCUPctFH_seas3_op)) & date_adj <= yday(ymd(SCUPctFH_seas3_cl)) ~ as.numeric(SCUPctFH_3_len) * 2.54, TRUE ~ scup_min_y2),
    scup_bag_y2=dplyr::case_when(mode == "fh" & date_adj >= yday(ymd(SCUPctFH_seas4_op)) & date_adj <= yday(ymd(SCUPctFH_seas4_cl)) ~ as.numeric(SCUPctFH_4_bag), TRUE ~ scup_bag_y2),
    scup_min_y2=dplyr::case_when(mode == "fh" & date_adj >= yday(ymd(SCUPctFH_seas4_op)) & date_adj <= yday(ymd(SCUPctFH_seas4_cl)) ~ as.numeric(SCUPctFH_4_len) * 2.54, TRUE ~ scup_min_y2),
    
    scup_bag_y2=dplyr::case_when(mode == "pr" & date_adj >= yday(ymd(SCUPctPR_seas1_op)) & date_adj <= yday(ymd(SCUPctPR_seas1_cl)) ~ as.numeric(SCUPctPR_1_bag), TRUE ~ scup_bag_y2),
    scup_min_y2=dplyr::case_when(mode == "pr" & date_adj >= yday(ymd(SCUPctPR_seas1_op)) & date_adj <= yday(ymd(SCUPctPR_seas1_cl)) ~ as.numeric(SCUPctPR_1_len) * 2.54, TRUE ~ scup_min_y2),
    scup_bag_y2=dplyr::case_when(mode == "pr" & date_adj >= yday(ymd(SCUPctPR_seas2_op)) & date_adj <= yday(ymd(SCUPctPR_seas2_cl)) ~ as.numeric(SCUPctPR_2_bag), TRUE ~ scup_bag_y2),
    scup_min_y2=dplyr::case_when(mode == "pr" & date_adj >= yday(ymd(SCUPctPR_seas2_op)) & date_adj <= yday(ymd(SCUPctPR_seas2_cl)) ~ as.numeric(SCUPctPR_2_len) * 2.54, TRUE ~ scup_min_y2),
    
    scup_bag_y2=dplyr::case_when(mode == "sh" & date_adj >= yday(ymd(SCUPctSH_seas1_op)) & date_adj <= yday(ymd(SCUPctSH_seas1_cl)) ~ as.numeric(SCUPctSH_1_bag), TRUE ~ scup_bag_y2),
    scup_min_y2=dplyr::case_when(mode == "sh" & date_adj >= yday(ymd(SCUPctSH_seas1_op)) & date_adj <= yday(ymd(SCUPctSH_seas1_cl)) ~ as.numeric(SCUPctSH_1_len) * 2.54, TRUE ~ scup_min_y2),
    scup_bag_y2=dplyr::case_when(mode == "sh" & date_adj >= yday(ymd(SCUPctSH_seas2_op)) & date_adj <= yday(ymd(SCUPctSH_seas2_cl)) ~ as.numeric(SCUPctSH_2_bag), TRUE ~ scup_bag_y2),
    scup_min_y2=dplyr::case_when(mode == "sh" & date_adj >= yday(ymd(SCUPctSH_seas2_op)) & date_adj <= yday(ymd(SCUPctSH_seas2_cl)) ~ as.numeric(SCUPctSH_2_len) * 2.54, TRUE ~ scup_min_y2))


predictions_out10 <- data.frame()
#future::plan(future::multisession, workers = 36)
set.seed(915)
message("model_run_CT.R: starting Connecticut projection for scenario '", Run_Name, "', 100 draws across 34 parallel workers. Expect a long run.")
future::plan(future::multisession, workers = 34)
get_predictions_out<- function(x){
#for(x in 1:25){
  
  print(x)
  
  directed_trips2 <- directed_trips %>% 
    dplyr::filter(draw == x) # %>%
  # dplyr::mutate(day = stringr::str_extract(day, "^\\d{2}"), 
  #               period2 = paste0(month24, "-", day, "-", mode))
  
  catch_data <- feather::read_feather(file.path(data_path, paste0("proj_catch_draws_CT", "_", x,".feather"))) %>% 
    dplyr::left_join(directed_trips2, by=c("mode", "date", "draw")) 
  
  calendar_adjustments <- readr::read_csv(
    file.path(here::here(paste0("Data/proj_year_calendar_adjustments_new_CT.csv"))), show_col_types = FALSE) %>%
    dplyr::filter(draw == x) %>% 
    dplyr::select(-dtrip, -dtrip_y2, -state.x, -state.y, -draw) 
  
  base_outcomes0 <- list()
  n_choice_occasions0 <- list()
  
  mode_draw <- c("sh", "pr", "fh")
  for (md in mode_draw) {
    
    # pull trip outcomes from the calibration year
    base_outcomes0[[md]]<-readr::read_csv(file.path(here::here(paste0("Data/base_outcomes_new_CT_", x, "_", md, ".CSV")))) %>% 
      data.table::as.data.table()
    
    base_outcomes0[[md]]<-base_outcomes0[[md]] %>% 
      dplyr::mutate(date_parsed=lubridate::dmy(date)) %>% 
      dplyr::select(-date)
    
    # pull in data on the number of choice occasions per mode-day
    n_choice_occasions0[[md]]<-feather::read_feather(file.path(data_path, paste0("n_choice_occasions_new_CT_", md, "_", x, ".feather")))  
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
  
  #kim look here
  check_n_choice_occasions <- n_choice_occasions %>% 
    dplyr::select(date_parsed, mode) %>%
    dplyr::distinct() 
  
  base_outcomes<-base_outcomes %>% 
    dplyr::right_join(check_n_choice_occasions, by=c("date_parsed", "mode"))
  
  # Pull in calibration comparison information about trip-level harvest/discard re-allocations 
  calib_comparison<-readRDS(file.path(data_path,"calibrated_model_stats_new.rds")) %>%
    dplyr::filter(state=="CT" & draw==x )  
  
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
  # BROKEN AS COMMITTED - neither sourced file exists at these paths.
  # See model_run_MA.R and Run_Model.R for details.
  source(here::here("Code/sim/predict_rec_catch_functions.R"))
  source(here::here("Code/sim/predict_rec_catch.R"))
  
  test<- predict_rec_catch(st = "CT", dr = x,
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
  
  #regs <- # Input table will be used to fill out regs in DT
  
  #predictions_out10<- predictions_out10 %>% rbind(test) 
}


print("out of loop")

start_time <- Sys.time()


# use furrr package to parallelize the get_predictions_out function 100 times
# This will spit out a dataframe with 100 predictions 
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
readr::write_csv(predictions_out10, file = here::here(paste0("output/output_CT_", Run_Name, "_", format(Sys.time(), "%Y%m%d_%H%M%S"),  ".csv")))

## CHnage 2

end_time <- Sys.time()

print(end_time - start_time)