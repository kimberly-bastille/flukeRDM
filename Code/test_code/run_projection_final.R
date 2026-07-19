
################################################################################
################################################################################
# Script:       run_projection_final.R
# Purpose:      Driver for the refactored ("revised_v3") projection code in
#               Code/test_code. Reads the inputs that are common to every
#               state and draw ONCE via read_projection_common_inputs(), then
#               runs the projection batch over the requested states and draws.
#               Hoisting the shared reads out of the loop is the whole point
#               of the refactor - the production path in Code/sim re-reads
#               them per iteration. Currently configured for a single
#               state x draw (MA, draw 1) as a timing test.
# Inputs:       Read by read_projection_common_inputs() from input_data_cd
#               and iterative_input_data_cd; not named individually here.
# Outputs:      Written by run_projection_batch_purrr() under the run tag
#               "candidate_reg_set_1"; nothing is written directly here.
# Dependencies: Sources project_rec_catch_final_revised_v3.R and
#               project_rec_catch_batch_helpers_revised.R by absolute path
#               into one developer's Desktop checkout. Packages data.table,
#               fst, purrr, furrr, future, readr, here, plyr, dplyr.
# Pipeline:     Development/QA scratch - the candidate replacement for the
#               Code/sim projection path. Not called by any wrapper.
#
# Note on n_simulations: set to 125 here, matching the 125 draws generated so
# that "check calibration convergence.do" can select 100 that converged. This
# does not match the 10 used in "Code/sim/R code wrapper.R" or the 100/3 that
# Stata's $ndraws takes; the three are not programmatically linked.
################################################################################
################################################################################

options(scipen = 999)

# packages <- c("tidyr",  "magrittr", "tidyverse", "reshape2", "splitstackshape","doBy","WriteXLS","Rcpp",
#               "ggplot2","dplyr","rlist","fitdistrplus","MASS","psych","rgl","copula","VineCopula","scales",
#               "univariateML","logspline","readr","data.table","conflicted", "readxl", "writexl", "fs", "fst",
#               "purrr", "readr", "here","plyr" , "furrr", "profvis", "future", "magrittr", "feather", "RStata", "haven")
# 
# # Install only those not already installed
# installed <- packages %in% rownames(installed.packages())
# if (any(!installed)) {
#   install.packages(packages[!installed])
# }
# lapply(packages, library, character.only = TRUE)

library(data.table)
library(fst)
library(purrr)
library(furrr)
library(future)
library(readr)
library(here)

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
conflicts_prefer(feather::read_feather)
conflicts_prefer(feather::write_feather)

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

#Set number of original draws. We create 125 (in case some don't converge in the calibration), but only use 100 for the final run. Choose a lot fewer for test runs
n_simulations<-125

n_draws<-50 #Number of simulated trips per day

source("C:/Users/andrew.carr-harris/Desktop/Git/flukeRDM/Code/test_code/project_rec_catch_final_revised_v3.R")
source("C:/Users/andrew.carr-harris/Desktop/Git/flukeRDM/Code/test_code/project_rec_catch_batch_helpers_revised.R")

################################################################################
################################################################################
# Section A: Read shared inputs once, then run the projection batch
################################################################################
################################################################################

states <- c("MA")
draws <- 1:1

message("run_projection_final.R: reading common projection inputs, then running the projection batch for ", length(states), " state(s) x ", length(draws), " draw(s). Elapsed time is reported at the end.")

system.time({

common_inputs <- read_projection_common_inputs(
  iterative_input_data_cd = iterative_input_data_cd,
  input_data_cd = input_data_cd,
  states = states,
  draws = draws
)

pred <- run_projection_batch_purrr(
  states = states,
  draws = draws,
  iterative_input_data_cd = iterative_input_data_cd,
  input_data_cd = input_data_cd,
  common_inputs = common_inputs,
  run_tag = "candidate_reg_set_1"
)
})
