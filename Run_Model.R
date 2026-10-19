
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
# Dependencies: Requires the R calibration stage to have completed, since the
#               per-state scripts read its output.
# Pipeline:     Entry point 3 of 3, invoked as
#                   Rscript Run_Model.R <Run_Name>
#               Nothing calls it from inside the repo; app.R triggers it as an
#               external process. Reads only pre-existing files - it does not
#               chain back to model_wrapper.do or "R code wrapper.R".
#
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

message("Run_Model.R: starting run '", args[1], "'. Each state present in the scenario is simulated in turn; this can take a long time per state.")

states <- c("ma", "ri", "ct", "ny", "nj", "de", "md", "va", "nc")

for (st in states) {
  if (any(grepl(st, saved_regs$input))) {
    save_regs <- saved_regs %>%
      dplyr::filter(grepl(st, saved_regs$input))
    run_state_model(Run_Name, state = st)
  }
}


end_time <- Sys.time()

message("Run_Model.R: run '", args[1], "' complete. Elapsed time:")
print(end_time - start_time)

