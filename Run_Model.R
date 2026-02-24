
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

source(here::here("Code/Sim/apply_directed_trips_regs.R"))
source(here::here("Code/Sim/run_state_model.R"))
## Massachusetts
# if(any(grepl("ma", saved_regs$input))){
#   
#   save_regs <- saved_regs %>%
#     dplyr::filter(grepl("ma", saved_regs$input))
#   
#   source(here::here("recDST/model_run_MA.R"))
# }
# 
# ## Rhode Island
# if(any(grepl("ri", saved_regs$input))){
#   
#   save_regs <- saved_regs %>%
#     dplyr::filter(grepl("ri", saved_regs$input))
#   
#   source(here::here("recDST/model_run_RI.R"))
# }
# 
# ## Connecticut
# if(any(grepl("ct", saved_regs$input))){
#   
#   save_regs <- saved_regs %>%
#     dplyr::filter(grepl("ct", saved_regs$input))
#   
#   source(here::here("recDST/model_run_CT.R"))
# }
# 
# ## New York
# if(any(grepl("ny", saved_regs$input))){
#   
#   save_regs <- saved_regs %>%
#     dplyr::filter(grepl("ny", saved_regs$input))
#   
#   source(here::here("recDST/model_run_NY.R"))
# }
# 
# ## New Jersey
# if(any(grepl("nj", saved_regs$input))){
#   
#   save_regs <- saved_regs %>%
#     dplyr::filter(grepl("nj", saved_regs$input))
#   
#   source(here::here("recDST/model_run_NJ.R"))
# }
# 
# ## Deleware
# if(any(grepl("de", saved_regs$input))){
#   
#   save_regs <- saved_regs %>%
#     dplyr::filter(grepl("de", saved_regs$input))
#   
#   source(here::here("recDST/model_run_DE.R"))
# }
# 
# ## Maryland
# if(any(grepl("md", saved_regs$input))){
#   
#   save_regs <- saved_regs %>%
#     dplyr::filter(grepl("md", saved_regs$input))
#   
#   source(here::here("recDST/model_run_MD.R"))
# }
# 
# ## Virginia
# if(any(grepl("va", saved_regs$input))){
#   
#   save_regs <- saved_regs %>%
#     dplyr::filter(grepl("va", saved_regs$input))
#   
#   source(here::here("recDST/model_run_VA.R"))
# }
# 
# # North Carolina
# if(any(grepl("nc", saved_regs$input))){
# 
#   save_regs <- saved_regs %>%
#     dplyr::filter(grepl("nc", saved_regs$input))
# 
#   source(here::here("recDST/model_run_NC.R"))
# }
# 
# 
# end_time <- Sys.time()
# 
# print(end_time - start_time)

