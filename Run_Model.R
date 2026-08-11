
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


states <- c("ma", "ri", "ct", "ny", "nj", "de", "md", "va", "nc")

for (st in states) {
  if (any(grepl(st, saved_regs$input))) {
    save_regs <- saved_regs %>%
      dplyr::filter(grepl(st, saved_regs$input))
    run_state_model(Run_Name, state = st)
  }
}


end_time <- Sys.time()

print(end_time - start_time)

