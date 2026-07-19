################################################################################
################################################################################
# Script:       required_packages.R
# Purpose:      One-time, manually-run installation of the R packages the
#               Shiny app and the per-state projection scripts depend on.
#               Note the scope: this list covers app.R, Run_Model.R and the
#               recDST/model_run_*.R family. It does NOT cover the calibration
#               and copula scripts in Code/sim and Code/pre_sim, which need
#               additional packages (copula, VineCopula, fitdistrplus, fst,
#               feather, data.table and others). Installing from this file
#               alone is not sufficient to run the full pipeline.
# Inputs:       None.
# Outputs:      None (installs packages into the user's R library).
# Dependencies: None.
# Pipeline:     Setup stage, outside the pipeline. Not sourced by any wrapper
#               and not called by "R code wrapper.R"; run by hand once when
#               setting up a new machine. Every call is unconditional, so
#               re-running it reinstalls packages that are already present.
################################################################################
################################################################################

## Required Packages
install.packages("shiny")
install.packages("shinyjs")
install.packages("shinyWidgets")
install.packages("magrittr")
install.packages("readr")
install.packages("here")
install.packages("dplyr")
install.packages("tidyr")
install.packages("stringr")
install.packages("lubridate")
install.packages("tibble")
install.packages("data.table")
install.packages("knitr")
install.packages("openxlsx")
install.packages("plyr")

install.packages("markdown")

install.packages("future")
install.packages("furrr")
install.packages("rlist")

