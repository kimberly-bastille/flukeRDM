

################################################################################
################################################################################
# Script:       test_predict_rec_catch.R
# Purpose:      Development harness comparing two versions of the projection
#               read/predict chain. The first loop runs the then-current
#               Code/sim versions over all nine states for 2 draws and checks
#               the result for missing values; the second runs the "_test1"
#               variants over all nine states for 100 draws under unchanged
#               regulations, writes that out, and summarizes the coefficient
#               of variation by state. The point of the second loop is to
#               establish how much run-to-run variation the model produces
#               when nothing about the regulations changes - the noise floor
#               against which a real scenario effect has to be judged.
# Inputs:       Whatever the sourced read scripts load; not parameterized here.
# Outputs:      test_no_change_scenario_MA_NY.csv
# Dependencies: Every source() path below is an absolute path into one
#               developer's Desktop checkout, and two of them
#               (predict_rec_catch_data_read.R, predict_rec_catch.R) name
#               files that no longer exist under Code/sim - they are the
#               pre-rename names, now found only in Code/archive or as
#               *_final.R. This script does not run as committed.
#               The loop variables `st` and `dr` are read by the sourced
#               scripts rather than passed as arguments.
# Pipeline:     Development/QA scratch. Not called by any wrapper.
#
# Note on the output filename: it says MA_NY, but the loops cover all nine
# states. The name is left over from an earlier, narrower version of the test.
################################################################################
################################################################################

#Lou's repos
iterative_input_data_cd="E:/Lou's projects/flukeRDM/flukeRDM_iterative_data"
input_data_cd="C:/Users/andrew.carr-harris/Desktop/MRIP_data_2025"

################################################################################
################################################################################
# Section A: Smoke test of the current chain - 2 draws, check for NAs
################################################################################
################################################################################

message("test_predict_rec_catch.R: starting the 2-draw smoke test over 9 states.")

predictions_list<-list()
k<-1
for (st in c("MA", "RI", "CT", "NY", "NJ", "DE", "MD", "VA", "NC")){
#  for (st in c("MA", "RI")){
    
  for (dr in 1:2){
    k<-k+1
    
    source("C:/Users/andrew.carr-harris/Desktop/Git/flukeRDM/Code/sim/predict_rec_catch_data_read.R")
    source("C:/Users/andrew.carr-harris/Desktop/Git/flukeRDM/Code/sim/predict_rec_catch_functions.R")
    source("C:/Users/andrew.carr-harris/Desktop/Git/flukeRDM/Code/sim/predict_rec_catch.R")
    
    predictions_list[[k]]<-predictions
    
  }
}

# k is incremented before the first assignment, so element [[1]] of the list is
# always NULL; [-1] drops it. The same pattern repeats in the second loop.
predictions_list2<-predictions_list[-1]
prediction_draws <- dplyr::bind_rows(predictions_list2)
prediction_draws_check <- prediction_draws %>% 
  dplyr::filter(is.na(value))



################################################################################
################################################################################
# Section B: No-change run - measure the model's own run-to-run variability
################################################################################
################################################################################

message("test_predict_rec_catch.R: starting the no-change scenario, 9 states x 100 draws. This is the long part of the script and can run for hours.")

## Run the model keeping everything the same
predictions_list2<-list()
k<-1
#for (st in c("MA")){
    for (st in c("MA", "RI", "CT", "NY", "NJ", "DE", "MD", "VA", "NC")){
  
  for (dr in 1:100){
    k<-k+1
    
    source("C:/Users/andrew.carr-harris/Desktop/Git/flukeRDM/Code/sim/predict_rec_catch_data_read_test1.R")
    source("C:/Users/andrew.carr-harris/Desktop/Git/flukeRDM/Code/sim/predict_rec_catch_functions_test1.R")
    source("C:/Users/andrew.carr-harris/Desktop/Git/flukeRDM/Code/sim/predict_rec_catch.R")
    
    predictions_list2[[k]]<-predictions
    
  }
}

predictions_list3<-predictions_list2[-1]   
prediction_draws2 <- dplyr::bind_rows(predictions_list3)
write_csv(prediction_draws2, file.path(iterative_input_data_cd, paste0("test_no_change_scenario_MA_NY.csv")))


output<-prediction_draws2 %>% 
  dplyr::filter(category=="CV" & mode=="all modes") %>% 
  dplyr::group_by(state) %>% 
  dplyr::summarize(mean_cv=mean(value), sd_cv=sd(value))

