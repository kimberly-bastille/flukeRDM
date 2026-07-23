################################################################################
################################################################################
# Script:       generate_coastwide_data.R
# Purpose:      Assembles a complete coastwide result set from the per-state
#               model output CSVs. Two gaps have to be patched to get there:
#               (1) a regulation scenario may not have been run for every
#               state, so states missing from a scenario inherit that state's
#               status-quo ("SQ") result, and (2) RI and MD are short of the
#               expected 100 draws, so specific existing draws are duplicated
#               under new draw numbers to fill the count. One CSV is written
#               per model/scenario.
# Inputs:       output/*SQ4*.csv - the per-state, per-scenario model output
#               files. Filenames are expected to match the pattern
#               output_<name>_<year>..., since the scenario label is recovered
#               by extracting the text between "output_" and "_202".
# Outputs:      One CSV per model into output_coastwide/ (but see the
#               filename note in Section C).
# Dependencies: Packages dplyr, stringr, readr, purrr, tidyr, here. Requires
#               that the per-state projection runs have already completed and
#               populated output/.
# Pipeline:     Standalone post-processing utility. Not called by any wrapper
#               and not sourced by app.R; run by hand after a projection round.
#
# Behavior notes (documented, not fixed):
#   - The draw-duplication in Section B is hardcoded to specific draw numbers
#     for RI and MD from one particular run. It is not a general gap-filler
#     and will not do the right thing on a different set of output files.
################################################################################
################################################################################

## Coastwide medians
library(dplyr)
library(stringr)
library(readr)
library(purrr)
library(tidyr)

## Find all runs and states that we need to incorporate SQ
### Read in all of output folder

################################################################################
################################################################################
# Section A: Read all output files and fill in states missing from a scenario
################################################################################
################################################################################

message("generate_coastwide_data.R: reading per-state output CSVs from output/ and assembling coastwide files. This may take a few minutes if many scenarios are present.")

# The first assignment is immediately overwritten by the second, so only the
# "SQ4" subset is actually read. The .csv$ line is a leftover from reading the
# whole output folder; it has no effect as written.
files <- list.files(path = here::here("output/"), pattern = "\\.csv$", full.names = TRUE)
files <- list.files(path = here::here("output/"), pattern = "SQ4", full.names = TRUE)
all_data <- files %>%
  set_names(files) %>%  # Optional: keep file names for reference
  purrr::map_dfr(readr::read_csv, .id = "filename") %>% 
  dplyr::mutate(filename = stringr::str_extract(filename, "(?<=output_).+?(?=_202)"))

state_list <- unique(all_data$state)
for (i in unique(all_data$model)){
  df <- all_data %>% dplyr::filter(model == i)
  
  states_present <- unique(df$state)
  
  # Any state absent from this scenario is backfilled with its status-quo row,
  # then relabeled as belonging to this scenario. The modeling assumption is
  # that a state with no scenario-specific run is unaffected by the scenario,
  # so its status-quo outcome stands in for it in the coastwide total.
  ### Compare list
  states_to_get <- setdiff(state_list, states_present)

  missing_states <- all_data %>%
    dplyr::filter(state %in% states_to_get & model == "SQ")
  
  ### Add SQ states to variables
  df <- rbind(df, missing_states) %>% 
    dplyr::mutate(model = i)
  
  
  ##############################################################################
  ##############################################################################
  # Section B: Pad RI and MD up to the expected 100 draws
  ##############################################################################
  ##############################################################################

  # RI finished 98 draws and MD 99; rather than re-running them, existing draws
  # chosen at random (RI 40 and 83, MD 31) are copied and renumbered into the
  # empty slots so every state has the same draw count downstream. This is a
  # patch for one specific run, not a general rule - the draw numbers below are
  # hardcoded and would need revisiting for any other output set.
  # `expected_draws` is assigned but never used.
  ## Fill empty runs
  expected_draws = 1:100
  ### RI
  ri <- df %>% dplyr::filter(state == "RI")
  
  ## Using random number generator 40 and 83 
  ri_dupl <- ri %>% dplyr::filter(draw %in% c(40, 83)) %>% 
    distinct() %>% 
    dplyr::mutate(draw = recode(draw,`40` = 99,  `83` = 100))
  
  ri <- rbind(ri, ri_dupl)
  
  ### MD
  md <- df %>% dplyr::filter(state == "MD")
  length(unique(md$draw))
  
  ## Using random number generator 31
  md_dupl <- md %>% dplyr::filter(draw %in% c(31)) %>% 
    distinct() %>% 
    dplyr::mutate(draw = recode(draw,`31` = 100))
  
  md <- rbind(md, md_dupl)
  
  df<- df %>% dplyr::filter(state != "RI",
                            state != "MD") %>% 
    rbind(ri, md)
  
  ############################################################################
  ############################################################################
  # Section C: Write one coastwide CSV per model
  ############################################################################
  ############################################################################

  # Note: paste() rather than paste0() is used here, so its default sep = " "
  # inserts spaces into the filename - a model named SQ4 produces the file
  # "output_coastwide/ SQ4 .csv", with leading and trailing spaces in the name.
  # The files are written and readable, but the names are not what they appear
  # to be in this line. Flagged, not fixed.
  ## Write new data file out
  write.csv(df, here::here(paste("output_coastwide/", i, ".csv")))
}

message("generate_coastwide_data.R: finished writing coastwide CSVs to output_coastwide/.")




