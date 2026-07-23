################################################################################
################################################################################
# Script:       compare_savedregs_output.R
# Purpose:      Backfill utility. Compares the saved regulation scenarios in
#               saved_regs/ against the result files already in output/,
#               finds the scenarios that were never run, and runs them. The
#               intent is to catch up after runs that failed or were
#               interrupted, without re-running scenarios that already have
#               output.
# Inputs:       saved_regs/regs_<name>.csv (directory listing plus contents),
#               output/ (directory listing only)
# Outputs:      None directly. Each sourced per-state script writes its own
#               output CSV.
# Dependencies: Packages dplyr, magrittr, here. Same runtime prerequisites as
#               Run_Model.R.
# Pipeline:     Standalone operator utility. Not called by any wrapper and not
#               referenced by app.R.
#
# SYNTAX ERROR - THIS FILE DOES NOT PARSE. In the Virginia block below the
# filter reads
#     dplyr::filter(grepl("",va saved_regs$input))
# with the quotes closed early and no comma before saved_regs. It should read
# grepl("va", saved_regs$input). Because R parses a whole file before
# evaluating any of it, this is not a latent bug that only bites Virginia runs
# - source()ing or Rscript-ing this file fails immediately with a parse error
# and nothing in it executes. Flagged, deliberately NOT fixed.
#
# Also inherits the KNOWN BROKEN per-state path: even with the parse error
# corrected, the model_run_*.R scripts it sources fail inside
# Code/sim/run_state_model.R. See that file's header.
#
# Difference from Run_Model.R: this script assign()s each regulation name and
# value into the calling environment before sourcing each per-state script;
# Run_Model.R does not. The per-state scripts re-read the CSV themselves, so
# it is not obvious that either loop's assignments are consumed.
################################################################################
################################################################################

# Both listings are reduced to a bare scenario name so they can be compared:
# "regs_SQ4.csv" and "output_SQ4_2026_04.csv" both become "SQ4". The output
# pattern additionally strips a trailing _<digits>_<digits> date stamp.
saved_regs <- data.frame(run_names = list.files(path = here::here("saved_regs"))) %>%
  dplyr::mutate(run_names = gsub("^regs_|\\.csv$", "", run_names))

output <- data.frame(run_names = list.files(path = here::here("output"))) %>% 
  dplyr::mutate(run_names = gsub("^output_|_[0-9]+_[0-9]+|\\.csv$", "", run_names))

# anti_join keeps scenarios that have no matching output file - i.e. exactly
# the runs still owed. The names are then rebuilt into filenames to read.
compare <- saved_regs %>%
  dplyr::anti_join(output, by = colnames(saved_regs)) %>%
  dplyr::mutate(run_names = paste0("regs_", run_names, ".csv"))

################################################################################
################################################################################
# Section A: Re-run every scenario that has no output
################################################################################
################################################################################

# Note: saved_regs is reassigned here, overwriting the scenario-name table
# built above. That is safe only because `compare$run_names` was already
# materialized before the loop began.

message("compare_savedregs_output.R: found ", nrow(compare), " scenario(s) with no output. Re-running them now; each may take a long time.")

for(i in compare$run_names){
  saved_regs <- read.csv(file.path(here::here(paste0("saved_regs/", i))))
  
  
  
  
  
  
  ## Massachusetts
  if(any(grepl("ma", saved_regs$input))){
  
    save_regs <- saved_regs %>%
      dplyr::filter(grepl("ma", saved_regs$input))
    
    for (a in seq_len(nrow(save_regs))) {
      # Extract name and value
      obj_name <- save_regs$input[a]
      obj_value <- save_regs$value[a]
      
      # Assign to object in the environment
      assign(obj_name, obj_value)
    }
    
    source(here::here("recDST/model_run_MA.R"))
  }



## Rhode Island
if(any(grepl("ri", saved_regs$input))){
  
  save_regs <- saved_regs %>%
    dplyr::filter(grepl("ri", saved_regs$input))
  
  for (a in seq_len(nrow(save_regs))) {
    # Extract name and value
    obj_name <- save_regs$input[a]
    obj_value <- save_regs$value[a]
    
    # Assign to object in the environment
    assign(obj_name, obj_value)
  }
  
  source(here::here("recDST/model_run_RI.R"))
}
  
  ## Connecticut
  if(any(grepl("ct", saved_regs$input))){
    
    save_regs <- saved_regs %>%
      dplyr::filter(grepl("ct", saved_regs$input))
    
    for (a in seq_len(nrow(save_regs))) {
      # Extract name and value
      obj_name <- save_regs$input[a]
      obj_value <- save_regs$value[a]
      
      # Assign to object in the environment
      assign(obj_name, obj_value)
    }
    
    source(here::here("recDST/model_run_CT.R"))
  }
  
  ## New York
  if(any(grepl("ny", saved_regs$input))){
    
    save_regs <- saved_regs %>%
      dplyr::filter(grepl("ny", saved_regs$input))
    
    for (a in seq_len(nrow(save_regs))) {
      # Extract name and value
      obj_name <- save_regs$input[a]
      obj_value <- save_regs$value[a]
      
      # Assign to object in the environment
      assign(obj_name, obj_value)
    }
    
    source(here::here("recDST/model_run_NY.R"))
  }
  
  ## New Jersey
  if(any(grepl("nj", saved_regs$input))){
    
    save_regs <- saved_regs %>%
      dplyr::filter(grepl("nj", saved_regs$input))
    
    for (a in seq_len(nrow(save_regs))) {
      # Extract name and value
      obj_name <- save_regs$input[a]
      obj_value <- save_regs$value[a]
      
      # Assign to object in the environment
      assign(obj_name, obj_value)
    }
    
    source(here::here("recDST/model_run_NJ.R"))
  }
  
  ## Deleware
  if(any(grepl("de", saved_regs$input))){
    
    save_regs <- saved_regs %>%
      dplyr::filter(grepl("de", saved_regs$input))
    
    for (a in seq_len(nrow(save_regs))) {
      # Extract name and value
      obj_name <- save_regs$input[a]
      obj_value <- save_regs$value[a]
      
      # Assign to object in the environment
      assign(obj_name, obj_value)
    }
    
    source(here::here("recDST/model_run_DE.R"))
  }
  
  ## MAryland
  if(any(grepl("md", saved_regs$input))){
    
    save_regs <- saved_regs %>%
      dplyr::filter(grepl("md", saved_regs$input))
    
    for (a in seq_len(nrow(save_regs))) {
      # Extract name and value
      obj_name <- save_regs$input[a]
      obj_value <- save_regs$value[a]
      
      # Assign to object in the environment
      assign(obj_name, obj_value)
    }
    
    source(here::here("recDST/model_run_MD.R"))
  }
  
  ## Virginia
  if(any(grepl("va", saved_regs$input))){

    # SYNTAX ERROR is on the next line - see the file header. Should be
    # grepl("va", saved_regs$input). Left unfixed per this session's scope.
    save_regs <- saved_regs %>%
      dplyr::filter(grepl("",va saved_regs$input))
    
    for (a in seq_len(nrow(save_regs))) {
      # Extract name and value
      obj_name <- save_regs$input[a]
      obj_value <- save_regs$value[a]
      
      # Assign to object in the environment
      assign(obj_name, obj_value)
    }
    
    source(here::here("recDST/model_run_VA.R"))
  }
  
  ## North Carolina
  if(any(grepl("nc", saved_regs$input))){
    
    save_regs <- saved_regs %>%
      dplyr::filter(grepl("nc", saved_regs$input))
    
    for (a in seq_len(nrow(save_regs))) {
      # Extract name and value
      obj_name <- save_regs$input[a]
      obj_value <- save_regs$value[a]
      
      # Assign to object in the environment
      assign(obj_name, obj_value)
    }
    
    source(here::here("recDST/model_run_NC.R"))
  }
}