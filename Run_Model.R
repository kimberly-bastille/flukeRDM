
### Injest run name and run model

# Rscript Run_Model.R Run_Name
start_time <- Sys.time()
library(magrittr)
library(data.table)
library(lubridate)

conflicted::conflicts_prefer(lubridate::yday)
conflicted::conflicts_prefer(lubridate::ymd)


args = "SQ"

args <- commandArgs(trailingOnly = TRUE)

saved_regs<- read.csv(here::here(paste0("saved_regs/regs_", args[1], ".csv")))

source(here::here("Code/Sim/apply_directed_trips_regs.R"))
source(here::here("Code/Sim/run_state_model.R"))


# Massachusetts
if(any(grepl("ma", saved_regs$input))){

  save_regs <- saved_regs %>%
    dplyr::filter(grepl("ma", saved_regs$input))

  run_state_model("MA", args[1])
}

## Rhode Island
if(any(grepl("ri", saved_regs$input))){

  save_regs <- saved_regs %>%
    dplyr::filter(grepl("ri", saved_regs$input))

  run_state_model("RI", args[1])
}

## Connecticut
if(any(grepl("ct", saved_regs$input))){

  save_regs <- saved_regs %>%
    dplyr::filter(grepl("ct", saved_regs$input))

  run_state_model("CT", args[1])
}

## New York
if(any(grepl("ny", saved_regs$input))){

  save_regs <- saved_regs %>%
    dplyr::filter(grepl("ny", saved_regs$input))

  run_state_model("NY", args[1])
}

## New Jersey
if(any(grepl("nj", saved_regs$input))){

  save_regs <- saved_regs %>%
    dplyr::filter(grepl("nj", saved_regs$input))

  run_state_model("NJ", args[1])
}

## Deleware
if(any(grepl("de", saved_regs$input))){

  save_regs <- saved_regs %>%
    dplyr::filter(grepl("de", saved_regs$input))

  run_state_model("DE", args[1])
}

## Maryland
if(any(grepl("md", saved_regs$input))){

  save_regs <- saved_regs %>%
    dplyr::filter(grepl("md", saved_regs$input))

  run_state_model("MD", args[1])
}

## Virginia
if(any(grepl("va", saved_regs$input))){

  save_regs <- saved_regs %>%
    dplyr::filter(grepl("va", saved_regs$input))

  run_state_model("VA", args[1])
}

# North Carolina
if(any(grepl("nc", saved_regs$input))){

  save_regs <- saved_regs %>%
    dplyr::filter(grepl("nc", saved_regs$input))

  run_state_model("NC", args[1])
}


end_time <- Sys.time()

print(end_time - start_time)

