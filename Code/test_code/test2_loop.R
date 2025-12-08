
#st<-"NJ"
#dr<-1
#Lou's repos
iterative_input_data_cd="E:/Lou_projects/flukeRDM/flukeRDM_iterative_data"
input_data_cd="C:/Users/andrew.carr-harris/Desktop/MRIP_data_2025"


# Status quo regs
state_list<-list()
#for (st in c("MA", "RI", "CT", "NY", "NJ", "DE", "MD", "VA", "NC")){
  for (st in c("NJ")){
    
  predictions_list<-list()
  for (dr in 1:100){

    source("C:/Users/andrew.carr-harris/Desktop/Git/flukeRDM/Code/test_code/predict_rec_catch_data_read_test2.R")
    source("C:/Users/andrew.carr-harris/Desktop/Git/flukeRDM/Code/sim/predict_rec_catch_functions.R")
    source("C:/Users/andrew.carr-harris/Desktop/Git/flukeRDM/Code/test_code/predict_rec_catch_test2.R")
    
    predictions_list[[dr]]<-predictions

  }
  prediction_draws <- dplyr::bind_rows(predictions_list)

  write_csv(prediction_draws, file.path(paste0("C:/Users/andrew.carr-harris/Desktop/MRIP_data_2025/rdm testing data/SQ_runs_10_20/SQ_new_", st, ".csv")))
  
  }

state_list<-list()
#for (st in c("MA", "RI", "CT", "NY", "NJ", "DE", "MD", "VA", "NC")){
for (st in c("NJ")){
  
  predictions_list<-list()
  for (dr in 65:100){
    
    source("C:/Users/andrew.carr-harris/Desktop/Git/flukeRDM/Code/test_code/predict_rec_catch_data_read_test2.R")
    source("C:/Users/andrew.carr-harris/Desktop/Git/flukeRDM/Code/sim/predict_rec_catch_functions.R")
    source("C:/Users/andrew.carr-harris/Desktop/Git/flukeRDM/Code/test_code/predict_rec_catch_test2.R")
    
    predictions_list[[dr]]<-predictions
    
  }
  prediction_draws <- dplyr::bind_rows(predictions_list)
  
  write_csv(prediction_draws, file.path(paste0("C:/Users/andrew.carr-harris/Desktop/MRIP_data_2025/rdm testing data/SQ_runs_10_20/SQ_new65_", st, ".csv")))
  
}

state_list<-list()
for (st in c("MA", "RI", "CT", "NY", "DE", "MD", "VA", "NC")){
#for (st in c("NJ")){
  
  predictions_list<-list()
  for (dr in 1:100){
    
    source("C:/Users/andrew.carr-harris/Desktop/Git/flukeRDM/Code/test_code/predict_rec_catch_data_read_test2.R")
    source("C:/Users/andrew.carr-harris/Desktop/Git/flukeRDM/Code/sim/predict_rec_catch_functions.R")
    source("C:/Users/andrew.carr-harris/Desktop/Git/flukeRDM/Code/test_code/predict_rec_catch_test2.R")
    
    predictions_list[[dr]]<-predictions
    
  }
  prediction_draws <- dplyr::bind_rows(predictions_list)
  
  write_csv(prediction_draws, file.path(paste0("C:/Users/andrew.carr-harris/Desktop/MRIP_data_2025/rdm testing data/SQ_runs_10_20/SQ_new_", st, ".csv")))
  
}

# NO CHANGE
predictions_list<-list()
  st<-"MA"
  for (dr in 1:2){
    k<-dr
    source("C:/Users/andrew.carr-harris/Desktop/Git/flukeRDM/Code/test_code/predict_rec_catch_data_read_test3_nochange.R")
    source("C:/Users/andrew.carr-harris/Desktop/Git/flukeRDM/Code/sim/predict_rec_catch_functions.R")
    source("C:/Users/andrew.carr-harris/Desktop/Git/flukeRDM/Code/test_code/predict_rec_catch_test2.R")
    
    predictions_list[[k]]<-predictions
    k<-k+1
    
  }
  prediction_draws <- dplyr::bind_rows(predictions_list)
  prediction_draws_check <- prediction_draws %>% 
    dplyr::filter(is.na(value))
  prediction_draws1 <- prediction_draws %>% 
    dplyr::filter(mode=="all modes")
    
  
  
  calib_comparison<-readRDS(file.path(iterative_input_data_cd, "miscellanous/calibrated_model_stats_new.rds")) %>% 
    dplyr::select(mode, state, species, draw, MRIP_catch, MRIP_rel, MRIP_keep, model_catch, model_rel, model_keep) %>% 
    dplyr::filter(draw<=1 & state=="MA") %>% 
    dplyr::group_by(species, draw) %>% 
    dplyr::summarise(MRIP_catch=sum(MRIP_catch), 
                     MRIP_rel=sum(MRIP_rel), 
                     MRIP_keep=sum(MRIP_keep), 
                     model_catch=sum(model_catch), 
                     model_rel=sum(model_rel), 
                     model_keep=sum(model_keep)) %>% 
    dplyr::ungroup()
    %>% 
    dplyr::group_by(species) %>% 
    dplyr::summarise(MRIP_catch=mean(MRIP_catch), 
                     MRIP_rel=mean(MRIP_rel), 
                     MRIP_keep=mean(MRIP_keep), 
                     model_catch=mean(model_catch), 
                     model_rel=mean(model_rel), 
                     model_keep=mean(model_keep)) 


prediction_draws <- dplyr::bind_rows(predictions_list)
prediction_draws_check <- prediction_draws %>% 
  dplyr::filter(is.na(value))

write_csv(prediction_draws, file.path(input_data_cd, "test2output_SQ_new2.csv"))



# Reduce the minimum size by two for all species
predictions_list<-list()
k<-1
for (st in c("MA", "RI", "CT", "NY", "NJ", "DE", "MD", "VA", "NC")){

  for (dr in 1:25){

    source("C:/Users/andrew.carr-harris/Desktop/Git/flukeRDM/Code/test_code/predict_rec_catch_data_read_test2_min_minus2.R")
    source("C:/Users/andrew.carr-harris/Desktop/Git/flukeRDM/Code/sim/predict_rec_catch_functions.R")
    source("C:/Users/andrew.carr-harris/Desktop/Git/flukeRDM/Code/test_code/predict_rec_catch_test2.R")
    
    predictions_list[[k]]<-predictions
    k<-k+1
    
  }
}

prediction_draws <- dplyr::bind_rows(predictions_list)
prediction_draws_check <- prediction_draws %>% 
  dplyr::filter(is.na(value))

write_csv(prediction_draws, file.path(input_data_cd, "test2output_minus2_new2.csv"))



# Increase the minimum size by two for all species
predictions_list<-list()
k<-1
for (st in c("MA", "RI", "CT", "NY", "NJ", "DE", "MD", "VA", "NC")){

  for (dr in 1:25){
    k<-k+1
    
    source("C:/Users/andrew.carr-harris/Desktop/Git/flukeRDM/Code/test_code/predict_rec_catch_data_read_test2_min_plus2.R")
    source("C:/Users/andrew.carr-harris/Desktop/Git/flukeRDM/Code/sim/predict_rec_catch_functions.R")
    source("C:/Users/andrew.carr-harris/Desktop/Git/flukeRDM/Code/test_code/predict_rec_catch_test2.R")
    
    predictions_list[[k]]<-predictions
    k<-k+1
    
  }
}

prediction_draws <- dplyr::bind_rows(predictions_list)
prediction_draws_check <- prediction_draws %>% 
  dplyr::filter(is.na(value))

write_csv(prediction_draws, file.path(input_data_cd, "test2output_plus2_new2.csv"))



