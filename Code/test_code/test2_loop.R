
################################################################################
################################################################################
# Script:       test2_loop.R
# Purpose:      Timed driver for the "_test2" branch of the projection code -
#               the variant that switched the input format from feather to
#               fst. Runs MA for 10 draws inside system.time(), then prints
#               mean welfare change, trip counts and kept/caught totals so the
#               numbers can be eyeballed against the pre-switch version. The
#               timing wrapper is the point: this exists to check that the fst
#               switch did not change results while confirming it is faster.
# Inputs:       Whatever predict_rec_catch_data_read_test2.R loads for the
#               given state and draw.
# Outputs:      None - the write_csv call is commented out. Results stay in
#               the object prediction_draws for interactive inspection.
# Dependencies: Absolute source() paths into one developer's Desktop
#               checkout; will not run elsewhere without editing. The loop
#               variables `st` and `dr` are read by the sourced scripts rather
#               than passed as arguments.
# Pipeline:     Development/QA scratch. Not called by any wrapper. Companion
#               to predict_rec_catch_data_read_test2.R and
#               predict_rec_catch_test2.R.
#
# Note: the nine-state loop on the line below the active one is commented out;
# as committed this runs MA only.
################################################################################
################################################################################

options(scipen = 999)

packages <- c("tidyr",  "magrittr", "tidyverse", "reshape2", "splitstackshape","doBy","WriteXLS","Rcpp",
              "ggplot2","dplyr","rlist","fitdistrplus","MASS","psych","rgl","copula","VineCopula","scales",
              "univariateML","logspline","readr","data.table","conflicted", "readxl", "writexl", "fs",
              "purrr", "readr", "here","plyr" , "furrr", "profvis", "future", "magrittr", "feather", "RStata", "haven")

# Install only those not already installed
# installed <- packages %in% rownames(installed.packages())
# if (any(!installed)) {
#   install.packages(packages[!installed])
# }
lapply(packages, library, character.only = TRUE)

library(plyr)
library(dplyr)

conflicts_prefer(here::here)
conflicts_prefer(dplyr::filter)
conflicts_prefer(dplyr::select)
conflicts_prefer(dplyr::mutate)
conflicts_prefer(dplyr::rename)
conflicts_prefer(dplyr::summarize)
conflicts_prefer(dplyr::summarise)
conflicts_prefer(dplyr::count)


#Lou's repos
iterative_input_data_cd="E:/Lou_projects/flukeRDM/flukeRDM_iterative_data"
input_data_cd="C:/Users/andrew.carr-harris/Desktop/MRIP_data_2025"

################################################################################
################################################################################
# Section A: Timed run of the fst-based (_test2) chain
################################################################################
################################################################################

# These two assignments are overwritten by the loops below on the first
# iteration. They exist so the sourced scripts can be run line-by-line
# interactively without setting up the loop first.
st<-"MA"
dr<-1

message("test2_loop.R: running the _test2 (fst) projection chain for MA, 10 draws. Elapsed time is reported at the end.")

system.time({
  
state_list<-list()
#for (st in c("MA", "RI", "CT", "NY", "DE", "MD", "VA", "NC")){
  for (st in c("MA")){
    
  predictions_list<-list()
  for (dr in 1:10){
    
    source("C:/Users/andrew.carr-harris/Desktop/Git/flukeRDM/Code/test_code/predict_rec_catch_data_read_test2.R")
    source("C:/Users/andrew.carr-harris/Desktop/Git/flukeRDM/Code/sim/predict_rec_catch_functions.R")
    source("C:/Users/andrew.carr-harris/Desktop/Git/flukeRDM/Code/test_code/predict_rec_catch_test2.R")
    
    predictions_list[[dr]]<-predictions
    
  }
  prediction_draws <- dplyr::bind_rows(predictions_list)
  
  #write_csv(prediction_draws, file.path(paste0("C:/Users/andrew.carr-harris/Desktop/MRIP_data_2025/rdm testing data/SQ_runs_10_20/SQ_new_", st, ".csv")))
  
}

})
################################################################################
################################################################################
# Section B: Eyeball checks against the pre-fst results
################################################################################
################################################################################

# change_CS is the change in consumer surplus (angler welfare) relative to the
# status-quo regulations - the model's headline economic output. The remaining
# checks cover trip counts and kept/caught totals under baseline vs alternative
# regulations. These are bracket-subset calls on a data.table, not a data.frame.
# Note the tot_keep_bsb_new line is repeated verbatim; tot_keep_scup_* and
# tot_keep_sf_* are not checked here.
mean(prediction_draws[metric=="change_CS" & mode=="all modes"]$value)
mean(prediction_draws[metric=="n_trips_alt" & mode=="all modes"]$value)
mean(prediction_draws[metric=="n_trips_base" & mode=="all modes"]$value)
mean(prediction_draws[metric=="tot_keep_bsb_new" & mode=="all modes"]$value)
mean(prediction_draws[metric=="tot_keep_bsb_base" & mode=="all modes"]$value)
mean(prediction_draws[metric=="tot_keep_bsb_new" & mode=="all modes"]$value)
mean(prediction_draws[metric=="tot_cat_bsb_base" & mode=="all modes"]$value)


sum(prediction_draws[metric=="tot_keep_sf_new" & mode=="all modes"]$value)


