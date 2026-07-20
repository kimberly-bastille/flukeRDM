################################################################################
################################################################################
# Script:       extra_draws_data_compiling.R
# Purpose:      Builds projection input files for the SUPPLEMENTAL draws in
#               RI, MD and VA. Those three states did not reach 100 converged
#               calibration draws on the first pass, so extra draws (101-105)
#               were selected; this script pulls those draws out of the full
#               calibration inputs and renumbers them into the 1-100 keying
#               scheme the projection code expects, writing a parallel set of
#               "_extra" files.
# Inputs:       calibration_good_draws_extras.xlsx,
#               directed_trips_calibration_<ST>.feather,
#               projected catch-at-length and catch draw files for RI, MD, VA
# Outputs:      directed_trips_calibration_new_extra_<ST>.feather and the
#               matching extra-draw files under additional_draws/
# Dependencies: Objects code_cd, input_data_cd and iterative_input_data_cd are
#               set at the top as absolute developer paths. Requires
#               "check calibration convergence.do" to have run first - it is
#               what produces calibration_good_draws_extras.xlsx.
# Pipeline:     Development/QA scratch, between the calibration and projection
#               stages. Not called by any wrapper.
#
# The draw / draw2 renumbering is the reason this script exists. Calibration
# draw numbers are sparse after non-converged draws are discarded, but the
# projection code loops over a dense 1..N range. The good-draws spreadsheet
# carries both the original draw id and its new dense position, and every
# join below is there to swap one for the other.
#
# The two bare `st<-` assignments near the top are leftovers for interactive
# stepping; the loops immediately overwrite them.
#
# Dev paths: 19 hardcoded absolute paths to a developer's local machine
#   (C:\ or E:\), at lines 36, 37, 53, 59, 86, 97, 104, 121, 124, 127, 130,
#   139, 145, 154, 155, 174, 175, 189 and 190.
################################################################################
################################################################################


code_cd=here("Code", "sim")
input_data_cd="C:/Users/andrew.carr-harris/Desktop/MRIP_data_2025"
iterative_input_data_cd="E:/Lou_projects/flukeRDM/flukeRDM_iterative_data"

# Run the stata code "check calibration convergence.do". This will select 100 of 125 draws out of 
# for each state/mode combo. This file creates "calibration_good_draws.xlsx", which contains the 
# original draw number and the "new" draw number (1-100) which facilitates looping/functions
# In each data input file for the projections, we need map draw (original # of draw) to draw2 (new draw scled 1-100) 
st<-"MD"
st<-"VA"

# Directed trips
statez <- c("RI", "MD", "VA")
for(st in statez) {
  
  good_draws<-read_excel(file.path(iterative_input_data_cd, "calibration_good_draws_extras.xlsx")) %>% 
    dplyr::filter(state==st)
  
  directed_trips<-feather::read_feather(paste0("E:/Lou_projects/flukeRDM/flukeRDM_iterative_data/archive/directed_trips_calibration/directed_trips_calibration_", st, ".feather")) %>%  
    dplyr::left_join(good_draws, by=c("state", "mode", "draw")) %>% 
    dplyr::filter(!is.na(draw2)) %>%
    dplyr::select(-draw) %>% 
    dplyr::rename(draw=draw2) 
  
  write_feather(directed_trips, paste0("E:/Lou_projects/flukeRDM/flukeRDM_iterative_data/additional_draws/directed_trips_calibration_new_extra_", st,".feather"))
  
}


# Projected catch-at-length *note for Kim that this file now contains distn's by mode
statez <- c("RI", "MD", "VA")
modez <- c("sh", "pr", "fh")
length_draw_list<-list()
length_draws_st_list<-list()
for(st in statez){
  for(md in modez){
    
    good_draws<-read_excel(file.path(iterative_input_data_cd, "calibration_good_draws_extras.xlsx")) %>% 
      dplyr::filter(state==st & mode==md)
    
    length_draw_list[[md]][[st]]<-read_csv(file.path(iterative_input_data_cd, "projected_catch_at_length.csv"), show_col_types = FALSE) %>% 
      dplyr::filter(state==st) %>% 
      dplyr::left_join(good_draws, by=c("state", "draw")) %>% 
      dplyr::filter(!is.na(draw2)) %>% 
      dplyr::select(-draw) %>% 
      dplyr::rename(draw=draw2) %>% 
      dplyr::mutate(mode=md) 
  }
  
}
length_draws <- dplyr::bind_rows(purrr::flatten(length_draw_list))
write_csv(length_draws, paste0("E:/Lou_projects/flukeRDM/flukeRDM_iterative_data/additional_draws/projected_catch_at_length_new_extra.csv"))


# Calendar year adjustments
statez <- c("RI", "MD", "VA")
for(st in statez) {
  
  good_draws<-read_excel(file.path(iterative_input_data_cd, "calibration_good_draws_extras.xlsx")) %>% 
    dplyr::filter(state==st) %>% 
    dplyr::select(-state)
  
  calendar_adj<- readr::read_csv(paste0("E:/Lou_projects/flukeRDM/flukeRDM_iterative_data/archive/miscellaneous/proj_year_calendar_adjustments/proj_year_calendar_adjustments_", st, ".csv"), show_col_types = FALSE) %>%
    dplyr::filter(state == st) %>% 
    dplyr::left_join(good_draws, by=c("mode", "draw")) %>% 
    dplyr::filter(!is.na(draw2)) %>% 
    dplyr::mutate(draw=draw2)%>% 
    dplyr::select(-draw2) 
  
  write_csv(calendar_adj, paste0("E:/Lou_projects/flukeRDM/flukeRDM_iterative_data/additional_draws/proj_year_calendar_adjustments_new_extra_", st, ".csv"))
  
}

# Baseline year outcomes and number of choice occasions
statez <- c("RI", "MD", "VA")
mode_draw <- c("sh", "pr", "fh")
for(dr in 101:105){
  for (md in mode_draw) {
    for(st in statez) {
      
      good_draws<-read_excel(file.path(iterative_input_data_cd, "calibration_good_draws_extras.xlsx")) %>% 
        dplyr::filter(state==st & mode==md & draw2==dr) 
      
      draw_orig<-mean(good_draws$draw)
      
      # pull trip outcomes from the calibration year
      base_outcomes_in<-feather::read_feather(paste0("E:/Lou_projects/flukeRDM/flukeRDM_iterative_data/archive/base_outcomes/base_outcomes_", st, "_", md, "_", draw_orig, ".feather")) %>% 
        data.table::as.data.table() 
      
      write_feather(base_outcomes_in, paste0("E:/Lou_projects/flukeRDM/flukeRDM_iterative_data/additional_draws/base_outcomes_new_extra_", st, "_", md, "_", dr, ".feather"))
      
      # pull in data on the number of choice occasions per mode-day
      n_choice_occasions_in<-feather::read_feather(paste0("E:/Lou_projects/flukeRDM/flukeRDM_iterative_data/archive/n_choice_occasion/n_choice_occasions_", st, "_", md, "_", draw_orig, ".feather")) %>% 
        data.table::as.data.table() 
      
      write_feather(n_choice_occasions_in, paste0("E:/Lou_projects/flukeRDM/flukeRDM_iterative_data/additional_draws/n_choice_occasions_new_extra_", st, "_", md, "_", dr, ".feather"))
    }
  }
  
}

# Calibration statistics (sublegal harvest/voluntary release information) 
good_draws<-read_excel(file.path(iterative_input_data_cd, "calibration_good_draws_extras.xlsx"))

calib_comparison<-readRDS("E:/Lou_projects/flukeRDM/flukeRDM_iterative_data/archive/miscellaneous/calibrated_model_stats.rds") %>% 
  dplyr::left_join(good_draws, by=c("state", "mode", "draw")) %>% 
  dplyr::filter(!is.na(draw2)) %>%
  dplyr::select(-draw) %>% 
  dplyr::rename(draw=draw2) 

saveRDS(calib_comparison, "E:/Lou_projects/flukeRDM/flukeRDM_iterative_data/additional_draws/calibrated_model_stats_new_extra.rds")


# re-save new directed trips files as excel files to pull into Stata and compute projected catch draws
library(writexl)

statez <- c("RI", "MD", "VA")
for(st in statez) {
  
  directed_trips<-feather::read_feather(paste0("E:/Lou_projects/flukeRDM/flukeRDM_iterative_data/additional_draws/directed_trips_calibration_new_extra_", st,".feather"))
  write_xlsx(directed_trips, paste0("E:/Lou_projects/flukeRDM/flukeRDM_iterative_data/additional_draws/directed_trips_calibration_new_extra_", st, ".xlsx"))
  
}



# Because I computed only 100 draws if projected catch-per-trip, 
# I randomly sample from these draws, to create the extra draws needed. 
# Step 1: randomly select from the original 100 draws
# Step 2: run "catch_per-_trip_projection_part2_extra_draws.do"
# Step 3: Transfer projected catch draw files from .dta to .feather

# Step 1
# randomly selected draws: 27, 84, 6, 59, 42

statez <- c("RI", "MD", "VA")
for(st in statez) {
  k<-101
  for(dr in c(27, 84, 6, 59, 42)){
  proj_catch<-read_excel(paste0("E:/Lou_projects/flukeRDM/flukeRDM_iterative_data/archive/proj_catch_draws_xlsdta/proj_catch_draws_", st,"_", dr, ".xlsx"))  
  write_xlsx(proj_catch, paste0("E:/Lou_projects/flukeRDM/flukeRDM_iterative_data/additional_draws/proj_catch_draws_extra_", st, "_", k, ".xlsx"))
  k<-k+1
  }
}

# Step 2
# run "catch_per-_trip_projection_part2_extra_draws.do"


# Step 3
# Transfer projected catch draw files from .dta to .feather
statez <- c("RI", "MD", "VA")
for(s in statez) {
  for(i in 101:105) {
    catch<-read_dta(paste0("E:/Lou_projects/flukeRDM/flukeRDM_iterative_data/additional_draws/proj_catch_draws_extra_",s, "_", i,".dta"))
    write_feather(catch, paste0("E:/Lou_projects/flukeRDM/flukeRDM_iterative_data/additional_draws/proj_catch_draws_extra_",s, "_", i,".feather"))
  }
}
