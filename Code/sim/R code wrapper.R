
options(scipen = 999)

packages <- c("tidyr",  "magrittr", "tidyverse", "reshape2", "splitstackshape","doBy","WriteXLS","Rcpp",
                 "ggplot2","dplyr","rlist","fitdistrplus","MASS","psych","rgl","copula","VineCopula","scales",
                 "univariateML","logspline","readr","data.table","conflicted", "readxl", "writexl", "fs",
                 "purrr", "readr", "here","plyr" , "furrr", "profvis", "future", "magrittr", "feather", "RStata", "haven")

# Install only those not already installed
installed <- packages %in% rownames(installed.packages())
if (any(!installed)) {
  install.packages(packages[!installed])
}
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

#There are four folders needed::
#input data - contains all the MRIP, biological data, angler characteristics data, as well as some data generated in the simulation
#code - contains all the model code
#output_data - this folder is empty to begin with. It stores final simulation output
#iterative_data -this folder is empty to begin with. It compiles data generated in the simulation

#Need to ensure that the globals below are set up in both this file and the stata model_wrapper.do file. 


#Set up R globals for input/output data and code scripts
code_cd=here("Code", "sim")
input_data_cd="C:/Users/andrew.carr-harris/Desktop/MRIP_data_2025"
iterative_input_data_cd="E:/Lou_projects/flukeRDM/flukeRDM_iterative_data"

###################################################
###############Pre-sim Stata code##################
###################################################

#Stata code extracts and prepares the data needed for the simulation

#Connect Rstudio to Stata
#options("RStata.StataPath" = "\"C:\\Program Files\\Stata17\\StataMP-64\"")
#options("RStata.StataVersion" = 17)

#Set number of original draws. We create 125 (in case some don't converge in the calibration), but only use 100 for the final run. Choose a lot fewer for test runs
n_simulations<-125

n_draws<-50 #Number of simulated trips per day

#First, open "$code_cd\model wrapper.do" and set globals:
#a) data years for different datasets
#b) number of draws (ndraws), which should be the same as the object n_simulations above
#c) cd's

#Second, open "$code_cd\set regulations.do" and set regulations for the calibration and projection period.

#Third, run the model wrapper code below:
#stata('do "C:/Users/andrew.carr-harris/Desktop/Git/rdmtool/lou_files/cod_haddock/code/model wrapper.do"')

###################################################




###################################################
###############Simulation R code###################
###################################################

#Notes:

#Simulation stratum are the groups in which we allocate and simulate choice occasions.
#For the 2025 SFSBSB RDM, the stratum is the combination of mode (pr/fh/sh) and state 

#Projection results are based on 100 iterations of the model. In each iteration we pull 
#in new distributions of catch-per-trip, directed fishing effort, projected catch-at-length, 
#and angler preferences. I calibrate the model with 125 iterations, some of which are 
#excluded after Step 2. From the pool of remaining iterations, I use the first 100 in the projection.

#Prior to running the model, transfer the catch_draw files from .csv to .feather to reduce computing time
statez <- c("MA", "RI", "CT", "NY", "NJ", "DE", "MD", "VA", "NC")

for(s in statez) {
  
  dtrip0<-read.csv(file.path(iterative_input_data_cd, paste0("directed_trips_calibration_", s,".csv")))
  write_feather(dtrip0, file.path(iterative_input_data_cd, paste0("directed_trips_calibration_", s,".feather")))

  for(i in 1:n_simulations) {
  catch<-read_dta(file.path(input_data_cd, paste0("calib_catch_draws_",s, "_", i,".dta")))
  write_feather(catch, file.path(iterative_input_data_cd, paste0("calib_catch_draws_",s, "_", i,".feather")))
  
  # make fake projection draws
  # write_feather(catch, file.path(iterative_input_data_cd, paste0("projected_catch_draws_",s, "_", i,".feather")))
  
}
}



##################### STEP 1 #####################
# Run the calibration algorithm to determine the difference between model-based harvest and MRIP-based harvest. 
# I do this for each stratum and each stratum's 125 draws of MRIP trips/catch/harvest.
# This code retains for each stratum the percent/absolute difference between model-based harvest and MRIP-based harvest by species. 

MRIP_comparison = read_dta(file.path(iterative_input_data_cd,"simulated_catch_totals.dta")) %>% 
      dplyr::rename(estimated_trips=tot_dtrip_sim, 
              sf_catch=tot_sf_cat_sim, 
              bsb_catch=tot_bsb_cat_sim, 
              scup_catch=tot_scup_cat_sim, 
              sf_keep=tot_sf_keep_sim, 
              bsb_keep=tot_bsb_keep_sim, 
              scup_keep=tot_scup_keep_sim,
              sf_rel=tot_sf_rel_sim, 
              bsb_rel=tot_bsb_rel_sim, 
              scup_rel=tot_scup_rel_sim) 

#Files needed:

#Scripts needed:


source(file.path(code_cd,"calibrate_rec_catch0.R"))

#Output files: 
#calibration_comparison.feather




##################### STEP 2 #####################
#Now, run each stratum's calibration simulation again, but this time allocate discards to harvest, 
#or harvest to discards, until the difference between model-based harvest and MRIP-based harvest 
#is within abs(5%) or <500 fish. 

#If a reallocation of discards to harvest is needed, I reallocate h* percent of all released 
# fish that are between [(min. size - 3 inches), min.size] as harvest.

#If a reallocation of harvest to discards is needed, I reallocate h* percent of all harvested fish as discards

#Note that in some iterations, the difference in harvest between the model and MRIP from Step 1 is too large relative to the number of 
#fish discarded in the model. So even if I allocate all discards as harvest, the percent difference in harvest
#between the model and MRIP will not be within abs(5%) or <500 fish. The code in Step 2 identifies and drops these iterations.

#This script saves calibration output, as well as the proportion of choice occasions in which we reallocate discards as harvest, 
#or vice versa. 

#Files needed:

#Scripts needed:
#calibration_catch_weights.R - can be commented out to save time if calibration catch weight are not needed.

source(file.path(code_cd,"calibration routine.R"))

#Output files: 
#calibration_comparison.rds
#calibration_catch_weights_cm.xlsx
#paste0("pds_new_", i,".rds")), where i is an indicator for a domain-draw combination
#paste0("costs_", i,".rds"))), where i is an indicator for a domain-draw combination


# Run the stata code "check calibration convergence.do". This will select 100 of 125 draws out of 
# for each state/mode combo. This file creates "calibration_good_draws.xlsx", which contains the 
# original draw number and the "new" draw number (1-100) which facilitates looping/functions
# In each data input file for the projections, we need map draw (original # of draw) to draw2 (new draw scled 1-100) 

# Directed trips
statez <- c("MA", "RI", "CT", "NY", "NJ", "DE", "MD", "VA", "NC")
for(st in statez) {

good_draws<-read_excel(file.path(iterative_input_data_cd, "calibration_good_draws.xlsx")) %>% 
  dplyr::filter(state==st)

directed_trips<-feather::read_feather(file.path(iterative_input_data_cd, paste0("directed_trips_calibration_", st, ".feather")))%>%  
  dplyr::left_join(good_draws, by=c("state", "mode", "draw")) %>% 
  dplyr::filter(!is.na(draw2)) %>%
  dplyr::select(-draw) %>% 
  dplyr::rename(draw=draw2) 
  
write_feather(directed_trips, file.path(iterative_input_data_cd, paste0("directed_trips_calibration_new_", st,".feather")))

}


# Projected catch-at-length *note for Kim that this file now contains distn's by mode
statez <- c("MA", "RI", "CT", "NY", "NJ", "DE", "MD", "VA", "NC")
modez <- c("sh", "pr", "fh")
length_draw_list<-list()
length_draws_st_list<-list()
for(st in statez){
    for(md in modez){
      
good_draws<-read_excel(file.path(iterative_input_data_cd, "calibration_good_draws.xlsx")) %>% 
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
write_csv(length_draws, file.path(iterative_input_data_cd, paste0("projected_catch_at_length_new.csv")))

# After testing with projected catch data, I found no projected catch-at-length distribution for NC summer flounder.
# This happened because there was no summer flounder catch in NC in the calibration year. 
# To fix, use length distribution from nearest state, which for NC is VA. 

check_size_data <- read_csv(file.path(iterative_input_data_cd, "projected_catch_at_length_new.csv"), show_col_types = FALSE) %>% 
  dplyr::mutate(domain=paste0(state, "_", species))
unique(check_size_data$domain)
check_size_data<-check_size_data %>% dplyr::select(-domain)
  
# Identify rows to duplicate (e.g., where state=="MD" & species=="sf") 
rows_to_duplicate <- check_size_data %>% dplyr::filter(state=="MD" & species=="sf") 
rows_to_duplicate$state <- "NC"

check_size_data<-bind_rows(check_size_data, rows_to_duplicate)
check_size_data<-check_size_data%>% dplyr::mutate(domain=paste0(state, "_", species))
unique(check_size_data$domain)
check_size_data<-check_size_data %>% dplyr::select(-domain)
write_csv(check_size_data, file.path(iterative_input_data_cd, paste0("projected_catch_at_length_new.csv")))




# Calendar year adjustments
statez <- c("MA", "RI", "CT", "NY", "NJ", "DE", "MD", "VA", "NC")
for(st in statez) {
  
  good_draws<-read_excel(file.path(iterative_input_data_cd, "calibration_good_draws.xlsx")) %>% 
    dplyr::filter(state==st)
  
  calendar_adj<- readr::read_csv(file.path(iterative_input_data_cd, paste0("proj_year_calendar_adjustments_", st, ".csv")), show_col_types = FALSE) %>%
  dplyr::filter(state == st) %>% 
  dplyr::left_join(good_draws, by=c("mode", "draw")) %>% 
    dplyr::filter(!is.na(draw2)) %>% 
    dplyr::mutate(draw=draw2)%>% 
    dplyr::select(-draw2) 
  
 write_csv(calendar_adj, file.path(iterative_input_data_cd, paste0("proj_year_calendar_adjustments_new_", st, ".csv")))
  
}

# Baseline year outcomes and number of choice occassions
for(dr in 1:100)
statez <- c("MA", "RI", "CT", "NY", "NJ", "DE", "MD", "VA", "NC")
mode_draw <- c("sh", "pr", "fh")
for(dr in 1:100){
  for (md in mode_draw) {
    for(st in statez) {
    good_draws<-read_excel(file.path(iterative_input_data_cd, "calibration_good_draws.xlsx")) %>% 
      dplyr::filter(state==st & mode==md & draw2==dr) 
    
    draw_orig<-mean(good_draws$draw)

  # pull trip outcomes from the calibration year
  base_outcomes_in<-feather::read_feather(file.path(iterative_input_data_cd, paste0("base_outcomes_", st, "_", md, "_", draw_orig, ".feather"))) %>% 
    data.table::as.data.table() 
  
  write_feather(base_outcomes_in, file.path(iterative_input_data_cd, paste0("base_outcomes_new_", st, "_", md, "_", dr, ".feather")))
  
  # pull in data on the number of choice occasions per mode-day
  n_choice_occasions_in<-feather::read_feather(file.path(iterative_input_data_cd, paste0("n_choice_occasions_", st, "_", md, "_", draw_orig, ".feather"))) %>% 
    data.table::as.data.table() 
  
  write_feather(n_choice_occasions_in, file.path(iterative_input_data_cd, paste0("n_choice_occasions_new_", st, "_", md, "_", dr, ".feather")))
    }
  }
  
}

# Calibration statistics (sublegal harvest/voluntary release information) 
good_draws<-read_excel(file.path(iterative_input_data_cd, "calibration_good_draws.xlsx"))
calib_comparison<-readRDS(file.path(iterative_input_data_cd, "calibrated_model_stats.rds")) %>% 
  dplyr::left_join(good_draws, by=c("state", "mode", "draw")) %>% 
  dplyr::filter(!is.na(draw2)) %>%
  dplyr::select(-draw) %>% 
  dplyr::rename(draw=draw2) 

saveRDS(calib_comparison, file = file.path(iterative_input_data_cd, "calibrated_model_stats_new.rds"))


# re-save new directed trips files as excel files to pull into Stata and compute projected catch draws
library(writexl)

statez <- c("MA", "RI", "CT", "NY", "NJ", "DE", "MD", "VA", "NC")
for(st in statez) {
  
  directed_trips<-feather::read_feather(file.path(iterative_input_data_cd, paste0("directed_trips_calibration_new_", st, ".feather")))
  write_xlsx(directed_trips, file.path(iterative_input_data_cd, paste0("directed_trips_calibration_new_", st, ".xlsx")))
  
}


# Transfer projected catch draw files from .dta to .feather
statez <- c("MA", "RI", "CT", "NY", "NJ", "DE", "MD", "VA", "NC")
for(s in statez) {
  for(i in 1:100) {
    catch<-read_dta(file.path(iterative_input_data_cd, paste0("proj_catch_draws_",s, "_", i,".dta")))
    write_feather(catch, file.path(iterative_input_data_cd, paste0("proj_catch_draws_",s, "_", i,".feather")))

  }
}

##################### STEP 3 #####################
#Run the projection algorithm. This algorithm pulls in population-adjusted catch-at-length distributions and allocates 
#fish discarded as harvest or vice versa in proportion to how they were allocated in the calibration. 

source(file.path(code_cd, "predict_rec_catch_data_read.R"))
source(file.path(code_cd, "predict_rec_catch_data_functions.R"))
source(file.path(code_cd, "predict_rec_catch.R"))

#Output files: 
# predictions.xlsx - output by mode, season, and draw









