
################################################################################
################################################################################
# Script:       Run_Model.R
# Purpose:      Command-line entry point for a projection run. Takes a run
#               name, reads the saved regulation scenario of that name, and
#               sources the per-state model script for each state that
#               appears in it. States absent from the scenario are skipped,
#               so a run that only changes Massachusetts regulations costs
#               one state's runtime rather than nine.
# Inputs:       regs_<Run_Name>.csv
# Outputs:      None directly. Each sourced per-state script writes its own
#               output CSV.
# Dependencies: Packages magrittr, data.table, lubridate, here, conflicted.
#               Requires the R calibration stage to have completed, since the
#               per-state scripts read its output.
# Pipeline:     Entry point 3 of 3, invoked as
#                   Rscript Run_Model.R <Run_Name>
#               Nothing calls it from inside the repo; app.R triggers it as an
#               external process. Reads only pre-existing files - it does not
#               chain back to model_wrapper.do or "R code wrapper.R".
#
# KNOWN BROKEN - this path does not run as committed. Each per-state script
# sources two files that do not exist at the paths given:
#     source(here::here("Code/sim/predict_rec_catch_functions.R"))
#     source(here::here("Code/sim/predict_rec_catch.R"))
# The first exists only as Code/archive/predict_rec_catch_functions.R; the
# second does not exist anywhere in the repo under that name - the closest
# match is Code/sim/predict_rec_catch_final.R. The source() calls sit inside
# the per-draw worker function, so a run fails on the first draw of the first
# state attempted. This is consistent with a rename in Code/sim that did not
# update the callers. The developers are aware; a fix may exist on an
# unpushed branch.
#
# Note that Code/sim/run_state_model.R carries the SAME two broken source()
# calls plus a third defect of its own, but it is not on this code path - the
# model_run_*.R scripts define their own worker rather than calling
# run_state_model().
#
# Note on `save_regs`: the per-state filter below assigns save_regs, but each
# per-state script re-reads and re-filters regs_<Run_Name>.csv itself, so the
# object is not actually consumed. Compare compare_savedregs_output.R, which
# additionally assign()s each regulation into the environment before sourcing;
# this file does not.
################################################################################
################################################################################

### Injest run name and run model

# Rscript Run_Model.R Run_Name
start_time <- Sys.time()
library(magrittr)
library(data.table)
library(lubridate)

conflicted::conflicts_prefer(lubridate::yday)
conflicted::conflicts_prefer(lubridate::ymd)


#args = "SQ"

args <- commandArgs(trailingOnly = TRUE)

saved_regs<- read.csv(here::here(paste0("saved_regs/regs_", args[1], ".csv")))

################################################################################
################################################################################
# Section A: Source the per-state model for each state in the scenario
################################################################################
################################################################################

# State selection is a substring test on the `input` column, whose values are
# named like "sfma_bag1", "bsbnj_min2" and so on. grepl("ma", ...) is therefore
# an unanchored match on the two-letter state code embedded in the input name,
# not an exact comparison - worth knowing if input names are ever renamed,
# since a new name that happens to contain another state's code would trigger
# that state's block too.

message("Run_Model.R: starting run '", args[1], "'. Each state present in the scenario is simulated in turn; this can take a long time per state.")

## Massachusetts
if(any(grepl("ma", saved_regs$input))){
  
  save_regs <- saved_regs %>%
    dplyr::filter(grepl("ma", saved_regs$input))
  
  source(here::here("recDST/model_run_MA.R"))
}

## Rhode Island
if(any(grepl("ri", saved_regs$input))){
  
  save_regs <- saved_regs %>%
    dplyr::filter(grepl("ri", saved_regs$input))
  
  source(here::here("recDST/model_run_RI.R"))
}

## Connecticut
if(any(grepl("ct", saved_regs$input))){
  
  save_regs <- saved_regs %>%
    dplyr::filter(grepl("ct", saved_regs$input))
  
  source(here::here("recDST/model_run_CT.R"))
}

## New York
if(any(grepl("ny", saved_regs$input))){
  
  save_regs <- saved_regs %>%
    dplyr::filter(grepl("ny", saved_regs$input))
  
  source(here::here("recDST/model_run_NY.R"))
}

## New Jersey
if(any(grepl("nj", saved_regs$input))){
  
  save_regs <- saved_regs %>%
    dplyr::filter(grepl("nj", saved_regs$input))
  
  source(here::here("recDST/model_run_NJ.R"))
}

## Deleware
if(any(grepl("de", saved_regs$input))){
  
  save_regs <- saved_regs %>%
    dplyr::filter(grepl("de", saved_regs$input))
  
  source(here::here("recDST/model_run_DE.R"))
}

## Maryland
if(any(grepl("md", saved_regs$input))){
  
  save_regs <- saved_regs %>%
    dplyr::filter(grepl("md", saved_regs$input))
  
  source(here::here("recDST/model_run_MD.R"))
}

## Virginia
if(any(grepl("va", saved_regs$input))){
  
  save_regs <- saved_regs %>%
    dplyr::filter(grepl("va", saved_regs$input))
  
  source(here::here("recDST/model_run_VA.R"))
}

# North Carolina
if(any(grepl("nc", saved_regs$input))){

  save_regs <- saved_regs %>%
    dplyr::filter(grepl("nc", saved_regs$input))

  source(here::here("recDST/model_run_NC.R"))
}


end_time <- Sys.time()

message("Run_Model.R: run '", args[1], "' complete. Elapsed time:")
print(end_time - start_time)

