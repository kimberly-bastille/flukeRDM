
################################################################################
################################################################################
# Script:       R code wrapper.R
# Purpose:      Orchestrates the R half of the pipeline: the two-pass
#               calibration and then the projection. Between the modeling
#               steps it also performs the format conversions the modeling
#               code assumes have already happened - Stata .dta and .csv
#               inputs are rewritten as .fst, and date strings are parsed to
#               integer dates once here rather than repeatedly inside the
#               simulation loops. Sets every R-level configuration value
#               (paths, iteration counts) at the top.
# Inputs:       directed_trips_calibration_<ST>.csv,
#               calib_catch_draws_<ST>_<i>.dta,
#               proj_catch_draws_<ST>_<i>.dta
# Outputs:      directed_trips_calibration_<ST>.fst,
#               calib_catch_draws_<ST>_<i>.fst,
#               proj_catch_draws_<ST>_<i>.fst
#               (plus everything written by the three sourced scripts:
#               calibration_comparison.fst, calibrated_model_stats.fst,
#               n_choice_occasions_<ST>_<MODE>_<DRAW>.fst,
#               base_outcomes_<ST>_<MODE>_<DRAW>.fst)
# Dependencies: The Stata pre-sim stage must have completed first, since the
#               .dta and .csv files converted below are its output. Sources
#               calibrate_rec_catch0_optimized.R, calibration_routine_final.R
#               (which itself sources calibrate_rec_catch1_final.R) and
#               predict_rec_catch_final.R. Does NOT source
#               Code/helpers/developer_setup.R - paths are set literally
#               below instead.
# Pipeline:     Entry point 2 of 3. NOTHING CALLS THIS SCRIPT. Unlike
#               GroundfishRDM, whose Stata wrapper invokes its R wrapper as a
#               final gated step, flukeRDM's model_wrapper.do never calls this
#               file. The operator must know to run model_wrapper.do first and
#               then this, by hand, in that order. Downstream, Run_Model.R is
#               a third independent entry point.
# Dev paths:    12 hardcoded absolute paths to a developer's local machine
#               (C:\ or E:\), at lines 129-130, 211-212, 215-216, 297, 301,
#               335 and 339; plus 2 more in commented-out lines (150, 170).
#
# Configuration mismatches to be aware of (documented, not changed):
#   - n_simulations is 10 here. The comment above it describes the intended
#     125-draw calibration / 100-draw production design, and Stata's $ndraws
#     is 100 (or 3 when proto=1). None of these are linked programmatically;
#     changing one does not change the others.
#   - n_draws (50) is assigned and never used in this file.
#   - input_data_cd and iterative_input_data_cd are absolute paths on two
#     different developers' machines. Several loops below then ignore
#     iterative_input_data_cd and re-hardcode the same E: path inline, so
#     editing the variable alone is not enough to relocate the data.
################################################################################
################################################################################

options(scipen = 999)

packages <- c("tidyr",  "magrittr", "tidyverse", "reshape2", "splitstackshape","doBy","WriteXLS","Rcpp",
                 "ggplot2","dplyr","rlist","fitdistrplus","MASS","psych","rgl","copula","VineCopula","scales",
                 "univariateML","logspline","readr","data.table","conflicted", "readxl", "writexl", "fs", "fst",
                 "purrr", "readr", "here","plyr" , "furrr", "profvis", "future", "magrittr", "feather", "RStata", "haven")

# Install only those not already installed
installed <- packages %in% rownames(installed.packages())
if (any(!installed)) {
  install.packages(packages[!installed])
}
lapply(packages, library, character.only = TRUE)

library(plyr)
library(dplyr)

# plyr is attached before dplyr deliberately: plyr masks several dplyr verbs
# (summarize, count, mutate, rename), and attaching it last would break the
# dplyr pipelines used throughout the sourced scripts. The conflicts_prefer
# calls below make the resolution explicit rather than relying on attach order.
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


################################################################################
################################################################################
# Section A: Helpers and configuration
################################################################################
################################################################################

#' @title Parse a date column of unknown format
#' @description Converts a character date vector to a data.table IDate,
#'   trying several formats in turn. This exists because the date columns
#'   arriving from the Stata stage are not written consistently: some files
#'   carry Stata's "%d%b%Y" display format (e.g. "15jul2024") and others
#'   carry ISO or US-style dates depending on how they were exported.
#'   Rather than tracking which file uses which, every date is pushed through
#'   this function. IDate (integer date) is used rather than Date because the
#'   simulation loops join and compare dates millions of times, and the
#'   integer representation is substantially faster.
#' @param x A character vector of dates in any one of the four supported
#'   formats: "%d%b%Y", "%Y-%m-%d", "%m/%d/%Y", "%d/%m/%Y". Note the order:
#'   because US-style precedes day-first, an ambiguous value like "03/04/2024"
#'   is read as 4 March, not 3 April, and no warning is issued.
#' @return An IDate vector. Values matching none of the formats become NA
#'   silently - as.Date does not warn - so unparsed dates surface later as
#'   missing rows rather than as an error here.
#' @examples
#' \dontrun{
#' parse_date_any(c("15jul2024", "2024-07-15"))
#' }
# helpers
parse_date_any <- function(x) {
  data.table::as.IDate(as.Date(
    x,
    tryFormats = c("%d%b%Y", "%Y-%m-%d", "%m/%d/%Y", "%d/%m/%Y")
  ))
}

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

################################################################################
################################################################################
# Section A2: The Stata hand-off - INSTRUCTIONS ONLY, nothing runs here
################################################################################
################################################################################

# Everything in this section is commented out. It is the manual checklist an
# operator follows before running this file, and it is also the exact point
# where GroundfishRDM's wrapper chain differs: there, the Stata wrapper calls
# the R wrapper, so the two stages cannot be run out of order. Here the link is
# a comment, so the ordering is enforced only by the operator remembering it.
# The RStata option lines and the stata() call below would automate the
# hand-off if uncommented; note the stata() path still points at the old
# rdmtool/cod_haddock repository, not flukeRDM.

#Stata code extracts and prepares the data needed for the simulation

#Connect Rstudio to Stata
#options("RStata.StataPath" = "\"C:\\Program Files\\Stata17\\StataMP-64\"")
#options("RStata.StataVersion" = 17)

# The comment below describes the intended design (125 calibration draws, 100
# used); the value actually set is 10, i.e. this file is currently configured
# for a test run, not production. Nothing links this to Stata's $ndraws.
#Set number of original draws. We create 125 (in case some don't converge in the calibration), but only use 100 for the final run. Choose a lot fewer for test runs
n_simulations<-10

# n_draws is not referenced anywhere in this file or the scripts it sources.
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

# Notes:

# Simulation strata are the groups of choice occasions sharing common input data.
# For the 2025 SFSBSB RDM, the stratum is the combination of mode (pr/fh/sh) and state 

# Projection results are based on 100+ iterations of the model. In each iteration I pull 
# new distributions of catch-per-trip, directed fishing effort, projected catch-at-length, 
# and angler preferences. I calibrate the model with 125 iterations,

################################################################################
################################################################################
# Section B: Convert calibration inputs from .dta/.csv to .fst
################################################################################
################################################################################

# The conversion is a performance measure, not a format preference. The
# calibration reads these files once per state x draw inside nested loops;
# read_fst is far faster than read_dta or read.csv and supports reading only
# the needed columns. The originals are left in place - only new .fst siblings
# are written.
# statez is redefined identically three times in this file (here and before
# each of the two later conversion loops); the repeats have no effect.

# Prior to running the simulations, save the catch_draw and directed_trip files as .fst
statez <- c("MA", "RI", "CT", "NY", "NJ", "DE", "MD", "VA", "NC")

message("R code wrapper.R: converting calibration inputs to .fst for ", length(statez), " states x ", n_simulations, " draws. This is I/O bound and can take a long time on first run.")

for(s in statez) {
  
  dtrip0<-read.csv(paste0("E:/Lou_projects/flukeRDM/flukeRDM_iterative_data/archive/directed_trips_calibration/directed_trips_calibration_", s,".csv"))
  write_fst(dtrip0, paste0("E:/Lou_projects/flukeRDM/flukeRDM_iterative_data/archive/directed_trips_calibration/directed_trips_calibration_", s,".fst"))

   for(i in 1:n_simulations) {
   catch<-read_dta(paste0("E:/Lou_projects/flukeRDM/flukeRDM_iterative_data/archive/calib_catch_draws/calib_catch_draws_",s, "_", i,".dta"))
   write_fst(catch, paste0("E:/Lou_projects/flukeRDM/flukeRDM_iterative_data/archive/calib_catch_draws/calib_catch_draws_",s, "_", i,".fst"))

}
}



##################### STEP 1 #####################
# Run the calibration0 algorithm to determine the difference between model- and MRIP-based harvest. 
# Repeat across iterations and strata 
# This code retains for each stratum the percent/absolute difference between model-based harvest and MRIP-based harvest by species. 

################################################################################
################################################################################
# Section C: STEP 1 - measure the model-vs-MRIP harvest gap
################################################################################
################################################################################

message("R code wrapper.R: STEP 1 of 3 - running the calibration0 algorithm. This is a long-running simulation step.")

source(file.path(code_cd,"calibrate_rec_catch0_optimized.R"))

message("R code wrapper.R: STEP 1 complete.")

# Output files:
# calibration_comparison.fst


##################### STEP 2 #####################
# Repeat the calibration algorithm but reallocate trip level harvest as discards, or vice versa 
# until the difference between model- and MRIP-based total harvest is within abs(5%) or <500 fish. 
# Model- and MRIP-based discards and total catch should also align.  

# If a reallocation of discards to harvest is needed, I reallocate h* percent of all discarded 
# fish that are between [(min_size - X inches), min_size] as harvest. If there are not enough eligible
# discards, increase X, which starts at 3 and increased to 4 if necessary. 

# If a reallocation of harvest to discards is needed, I reallocate h* percent of all harvested fish as discards.

# Note that in some iterations, the difference in harvest between the model and MRIP from Step 1 may be
# too large relative to the number of fish discarded in the model. The code in Step 2 identifies and drops these iterations.

# This script saves calibration output and reallocation parameters.

################################################################################
################################################################################
# Section D: STEP 2 - reallocate harvest and discards until MRIP is matched
################################################################################
################################################################################

# The date columns are parsed once here and the .fst files are rewritten in
# place with the parsed columns, replacing the character originals. Step 2
# re-reads these files inside its innermost loop, so parsing dates here rather
# than there removes the single largest avoidable cost in the calibration.
# Note the asymmetry with Section B: this loop resolves its paths through
# iterative_input_data_cd, while the catch-draw loop just below re-hardcodes
# the same E: path inline. Both point to the same place today.

message("R code wrapper.R: pre-parsing date columns in the directed-trips files.")

# Pre-compute date variables
for(s in statez) {
  dtrip <- data.table::as.data.table(
    fst::read_fst(file.path(
      iterative_input_data_cd,
      paste0("archive/directed_trips_calibration/directed_trips_calibration_", s, ".fst")))) %>% 
    dplyr::mutate(date_parsed = parse_date_any(date), 
                  date_parsed_y2 = parse_date_any(day_y2),
                  month=data.table::month(date_parsed)) %>% 
    dplyr::select(-date, -day_y2)
  
  write_fst(dtrip, file.path(
    iterative_input_data_cd,
    paste0("archive/directed_trips_calibration/directed_trips_calibration_", s, ".fst")))
}


statez <- c("MA", "RI", "CT", "NY", "NJ", "DE", "MD", "VA", "NC")
for(s in statez) {
  for(i in 1:n_simulations) {

    catch0<-read_fst(paste0("E:/Lou_projects/flukeRDM/flukeRDM_iterative_data/archive/calib_catch_draws/calib_catch_draws_",s, "_", i,".fst")) %>%
      dplyr::mutate(date_parsed = parse_date_any(date),
                    month=data.table::month(date_parsed)) %>%
      dplyr::select(-date_num, -date)
    write_fst(catch0, paste0("E:/Lou_projects/flukeRDM/flukeRDM_iterative_data/archive/calib_catch_draws/calib_catch_draws_",s, "_", i,".fst"))
    
  }
}

message("R code wrapper.R: STEP 2 of 3 - running the calibration reallocation routine. This is the longest step in the pipeline.")

source(file.path(code_cd,"calibration_routine_final.R")) # this script calls "calibrate_rec_catch1_final.R"

message("R code wrapper.R: STEP 2 complete.")

# Output files:
  # file.path(iterative_input_data_cd, paste0("archive/miscellaneous/calibrated_model_stats.fst")))
  # n_choice_occasions_ST_MD_DRAW.fst -  choice occasions to simulate in projection
  # base_outcomes_ST_MD_DRAW-  baseline trip outcomes


################################################################################
################################################################################
# Section E: Convert projection inputs to .fst
################################################################################
################################################################################

# Same conversion as Section B, but for the projection-year catch draws. It
# sits here rather than with the others because it depends on nothing from
# Step 2 and only has to be done before Step 3.

message("R code wrapper.R: converting projection catch draws to .fst.")

# Transfer projected catch draw files from .dta to .fst
statez <- c("MA", "RI", "CT", "NY", "NJ", "DE", "MD", "VA", "NC")

for(s in statez) {
  for(i in 1:n_simulations) {
    catch<-read_dta(paste0("E:/Lou_projects/flukeRDM/flukeRDM_iterative_data/archive/proj_catch_draws/proj_catch_draws_",s, "_", i,".dta")) %>%
      dplyr::mutate(date_parsed = parse_date_any(date),
                    month=data.table::month(date_parsed)) %>%
      dplyr::select(-date_num, -date)
    write_fst(catch, paste0("E:/Lou_projects/flukeRDM/flukeRDM_iterative_data/archive/proj_catch_draws/proj_catch_draws_",s, "_", i,".fst"))
    
  }
}


##################### STEP 3 #####################
# Run the projection algorithm. This algorithm pulls in population-adjusted catch-at-length distributions and allocates 
# fish discarded as harvest or vice versa in proportion to how they were allocated in the calibration. 
################################################################################
################################################################################
# Section F: STEP 3 - project catch under the alternative regulations
################################################################################
################################################################################

message("R code wrapper.R: STEP 3 of 3 - running the projection algorithm.")

source(file.path(code_cd, "predict_rec_catch_final.R"))

message("R code wrapper.R: STEP 3 complete. R pipeline finished.")











