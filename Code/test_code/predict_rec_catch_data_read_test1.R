################################################################################
################################################################################
# Script:       predict_rec_catch_data_read_test1.R
# Purpose:      Loads projection inputs for one state x draw from FEATHER files. The
#               earliest member of the family and the only one that does not
#               preserve status-quo regulation columns.
# Inputs:       directed_trips_calibration_new_<ST>.feather,
#               proj_catch_draws_<ST>_<draw>.feather,
#               projected_catch_at_length_new.csv
# Outputs:      None. Objects are left in the global environment for the
#               companion predict/functions script to use.
# Dependencies: Objects `st` and `dr` must exist in the calling environment.
#               Packages magrittr, dplyr, feather or fst, readr, data.table.
#
# THE predict_rec_catch_data_read_* FAMILY (5 files). Each loads the inputs
# for one state x draw of the projection, then leaves them in the global
# environment for a companion predict/functions script to consume. They differ
# only in input format and in what they do to the regulations before handing
# them on:
#   test1                - earliest version; feather inputs; no SQ columns kept
#   test2                - same, migrated from feather to fst; keeps the SQ
#                          (status-quo) regulation columns alongside the _y2
#                          alternative columns so the two can be compared
#   test2_min_minus2     - test2's regulation handling on feather inputs, with
#                          every minimum size reduced by 2 inches
#   test2_min_plus2      - same, with every minimum size increased by 2 inches
#   test3_nochange       - same, with regulations left untouched (the control)
# The +/-2 inch pair and the no-change control together form a sensitivity
# test: run all three and the spread shows how responsive the model's welfare
# and harvest estimates are to a small uniform change in minimum size.
#
# Shared conventions across the family:
#   - `st` and `dr` (state, draw) are NOT arguments. They are expected to
#     already exist in the calling environment, set by a driver such as
#     test2_loop.R or test_predict_rec_catch.R. The commented-out loops near
#     the top of each file show the intended driver shape.
#   - ndraws = 50 here means choice occasions simulated per stratum. It is a
#     different quantity from the pipeline's $ndraws / n_simulations, which
#     count simulation draws.
#   - `_y2` columns are the projection-year (alternative) regulations; `_SQ`
#     columns preserve the status-quo values for comparison.
#   - `* 2.54` converts inches to centimetres, the unit of the length data.
#   - Input paths are absolute paths on individual developers' machines, and
#     are not consistent even within the family (note "E:/Lou's projects" vs
#     "E:/Lou_projects"). None of these run as committed without editing.
#
# Pipeline: Development/QA scratch. Not called by any wrapper.
#
# Dev paths (this file): 3 hardcoded absolute paths to a developer's local
#   machine (C:\ or E:\), at lines 55, 56 and 97.
################################################################################
################################################################################

# Data read for non-shiny run of predict_rec_catch.R
## Run this script prior to predict rec catch

#Lou's repos
iterative_input_data_cd="E:/Lou's projects/flukeRDM/flukeRDM_iterative_data"
input_data_cd="C:/Users/andrew.carr-harris/Desktop/MRIP_data_2025"

#check

############# To Run Individual
# Variables to change 
#dr<-1
#st="NJ"
ndraws=50 #number of choice occasions to simulate per strata

library(magrittr)
############# To Run in Loop 

#for (st in c("MA", "RI", "CT", "NY", "NJ", "DE", "MD", "VA", "NC")){
#  for (dr in 1:2){

# import necessary data

# For kim: We need to retain the SQ regulation variables. 
#          As of now, the SQ regulations variables are the ones without subscripts. 
#          I will copy these variables with the subscript _SQ to make this explicit.
#          The alternative regulations that will be adjusted by the users will be 
#          have subscripts _y2 (note this is slightly different from cod and haddock 2024)


directed_trips<-feather::read_feather(file.path(iterative_input_data_cd, paste0("directed_trips_calibration_new_", st, ".feather"))) %>% 
  tibble::tibble() %>%
  dplyr::filter(draw == dr) %>%
  dplyr::select(mode, date, 
                bsb_bag, bsb_min, bsb_bag_y2, bsb_min_y2, 
                fluke_bag, fluke_min, fluke_bag_y2,fluke_min_y2,
                scup_bag, scup_min, scup_bag_y2, scup_min_y2) %>% 
  dplyr::mutate(fluke_min_SQ=fluke_min, fluke_bag_SQ=fluke_bag, 
                bsb_min_SQ=bsb_min, bsb_bag_SQ=bsb_bag, 
                scup_min_SQ=scup_min, scup_bag_SQ=scup_bag)

#catch_data <- feather::read_feather(file.path(iterative_input_data_cd, paste0("proj_catch_draws_",st, "_", dr,".feather"))) %>% 
#  dplyr::left_join(directed_trips, by=c("mode", "date")) 



l_w_conversion <- readr::read_csv(file.path("C:/Users/andrew.carr-harris/Desktop/Git/flukeRDM/Data", "L_W_Conversion.csv"), show_col_types = FALSE)  %>% 
  dplyr::filter(state==st)


# sf_size_data <- read_csv(file.path(iterative_input_data_cd, "projected_catch_at_length_new.csv"), show_col_types = FALSE)  %>% 
#   dplyr::filter(state == st, species=="sf", draw==dr) %>% 
#   dplyr::filter(!is.na(fitted_prob)) %>% 
#   dplyr::select(state, fitted_prob, length, mode)
# 
# bsb_size_data <- read_csv(file.path(iterative_input_data_cd, "projected_catch_at_length_new.csv"), show_col_types = FALSE)  %>% 
#   dplyr::filter(state == st, species=="bsb" , draw==dr) %>% 
#   dplyr::filter(!is.na(fitted_prob)) %>% 
#   dplyr::select(state, fitted_prob, length, mode)
# 
# scup_size_data <- read_csv(file.path(iterative_input_data_cd, "projected_catch_at_length_new.csv"), show_col_types = FALSE)  %>% 
#   dplyr::filter(state == st, species=="scup", draw==dr) %>% 
#   dplyr::filter(!is.na(fitted_prob)) %>% 
#   dplyr::select(state,  fitted_prob, length, mode)

calendar_adjustments <- readr::read_csv(
  file.path(iterative_input_data_cd, paste0("proj_year_calendar_adjustments_new_", st, ".csv")), show_col_types = FALSE) %>%
  dplyr::select(-state.y) %>% 
  dplyr::rename(state=state.x) %>% 
  dplyr::filter(state == st, draw==dr) %>% 
  dplyr::select(-dtrip, -dtrip_y2, -state, -draw)

# base-year trip outcomes
base_outcomes0 <- list()
n_choice_occasions0 <- list()

mode_draw <- c("sh", "pr", "fh")
for (md in mode_draw) {
  
  # pull trip outcomes from the calibration year
  base_outcomes0[[md]]<-feather::read_feather(file.path(iterative_input_data_cd, paste0("base_outcomes_new_", st, "_", md, "_", dr, ".feather"))) %>% 
    data.table::as.data.table()
  
  base_outcomes0[[md]]<-base_outcomes0[[md]] %>% 
    dplyr::select(-domain2) %>% 
    dplyr::mutate(date_parsed=lubridate::dmy(date)) %>% 
    dplyr::select(-date)
  
  # pull in data on the number of choice occasions per mode-day
  n_choice_occasions0[[md]]<-feather::read_feather(file.path(iterative_input_data_cd, paste0("n_choice_occasions_new_", st,"_", md, "_", dr, ".feather")))  
  n_choice_occasions0[[md]]<-n_choice_occasions0[[md]] %>% 
    dplyr::mutate(date_parsed=lubridate::dmy(date)) %>% 
    dplyr::select(-date)
  
}

base_outcomes <- bind_rows(base_outcomes0)
n_choice_occasions <- bind_rows(n_choice_occasions0) %>% 
  dplyr::arrange(date_parsed, mode)
rm(base_outcomes0, n_choice_occasions0)

base_outcomes<-base_outcomes %>% 
  dplyr::arrange(date_parsed, mode, tripid, catch_draw)

catch_data <- base_outcomes %>% 
  dplyr::mutate(date_obj = as.Date(date_parsed, format = "%Y-%m-%d")) %>% 
  dplyr::mutate(date = format(date_obj, format = "%d%b%Y")|> tolower()) %>% 
  dplyr::select(-matches("beta")) %>% 
  dplyr::rename(sf_cat=tot_cat_sf_base, bsb_cat=tot_cat_bsb_base, scup_cat=tot_cat_scup_base) %>% 
  dplyr::select(-cost, -NJ_dummy, -age, -date_parsed, -date_obj,-total_trips_12, -matches("tot_keep"), -matches("tot_rel") ) %>% 
  dplyr::left_join(directed_trips, by=c("mode", "date")) 
  
# Projected catch-at-length *note for Kim that this file now contains distn's by mode
# statez <- c("MA", "RI", "CT", "NY", "NJ", "DE", "MD", "VA", "NC")
# modez <- c("sh", "pr", "fh")
# length_draw_list<-list()
# length_draws_st_list<-list()
# for(st in statez){
#   for(md in modez){
#     
#     good_draws<-read_excel(file.path(iterative_input_data_cd, "calibration_good_draws.xlsx")) %>% 
#       dplyr::filter(state==st & mode==md)
#     
#     length_draw_list[[md]][[st]]<-read_csv(file.path(iterative_input_data_cd, "baseline_catch_at_length.csv"), show_col_types = FALSE) %>% 
#       dplyr::filter(state==st) %>% 
#       dplyr::left_join(good_draws, by=c("state", "draw")) %>% 
#       dplyr::filter(!is.na(draw2)) %>% 
#       dplyr::select(-draw) %>% 
#       dplyr::rename(draw=draw2) %>% 
#       dplyr::mutate(mode=md) 
#   }
#   
# }
# 
# length_draws <- dplyr::bind_rows(purrr::flatten(length_draw_list))
# write_csv(length_draws, file.path(iterative_input_data_cd, paste0("baseline_catch_at_length_new.csv")))


sf_size_data <- read_csv(file.path(iterative_input_data_cd, "baseline_catch_at_length_new.csv"), show_col_types = FALSE)  %>% 
  dplyr::filter(state == st, species=="sf", draw==dr) %>% 
  dplyr::filter(!is.na(fitted_prob)) %>% 
  dplyr::select(state, fitted_prob, length, mode)

bsb_size_data <- read_csv(file.path(iterative_input_data_cd, "baseline_catch_at_length_new.csv"), show_col_types = FALSE)  %>% 
  dplyr::filter(state == st, species=="bsb" , draw==dr) %>% 
  dplyr::filter(!is.na(fitted_prob)) %>% 
  dplyr::select(state, fitted_prob, length, mode)

scup_size_data <- read_csv(file.path(iterative_input_data_cd, "baseline_catch_at_length_new.csv"), show_col_types = FALSE)  %>% 
  dplyr::filter(state == st, species=="scup", draw==dr) %>% 
  dplyr::filter(!is.na(fitted_prob)) %>% 
  dplyr::select(state,  fitted_prob, length, mode)


# Pull in calibration comparison information about trip-level harvest/discard re-allocations 
calib_comparison<-readRDS(file.path(iterative_input_data_cd, "calibrated_model_stats_new.rds")) %>% 
  dplyr::filter(state==st & draw==dr ) 

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
  str_detect(all_vars, paste0("(_", species_suffixes, ")$", collapse = "|"))
]

id_vars <- setdiff(all_vars, species_specific_vars)

calib_comparison<-calib_comparison %>% 
  dplyr::select(mode, all_of(species_specific_vars))

# Extract base variable names (without _sf, _bsb, _scup)
base_names <- unique(str_replace(species_specific_vars, "_(sf|bsb|scup)$", ""))

# Pivot the data longer on the species-specific columns
calib_comparison <- calib_comparison %>%
  pivot_longer(
    cols = all_of(species_specific_vars),
    names_to = c(".value", "species"),
    names_pattern = "(.*)_(sf|bsb|scup)"
  ) %>% 
  dplyr::distinct()


#  }
#}



















