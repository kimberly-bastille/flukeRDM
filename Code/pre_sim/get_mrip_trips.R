################################################################################
# Script:       get_mrip_trips.R
# Purpose:      Uses the most recent version of mrip_pullDATE.Rds to construct trips
#               using MRIP tacklebox. Verify that MRIP tacklebox matches known good code
#               before switching to other harder metrics (catch by weight)
# Inputs:       mrip_pull{}.Rds
# Outputs:      groundfish_effort{}.Rds is a list containing estimates at different definitions
#               of effort. For the groundfishRDM, we use the item
#                  groundfish_effort$"PRIM1|PRIM2|A|B1|B2"
#               YEAR, WAVE, MODE, and AREA levels.
# Dependencies: mriptacklebox
#               Sources developer_setup.R (for gf.data.dir).
# Pipeline:     Called by model_wrapper.do
#                 Effort can be sent to Dashboard
# To Do:        PSEs for Effort
################################################################################


# Load libraries
# install the main branch, needs main on/after  7/29/2026
# remotes::install_github("NEFSC/READ-PDB-mriptacklebox")

library("here")
library("mriptacklebox")
library("tidyverse")
library("glue")
library("haven")
library("conflicted")
conflicts_prefer(dplyr::filter)
conflicts_prefer(dplyr::lag)




# standard "here", username setup, and paths
here::i_am("Code/pre_sim/get_mrip_trips.R")
source(here("Code", "helpers", "developer_setup.R"))

output_folder<-file.path(gf.data.dir, "miscellaneous")

vintage_string<-list.files(output_folder, pattern=glob2rx("mrip_pull*Rds"))
vintage_string<-gsub("mrip_pull","",vintage_string)
vintage_string<-gsub(".Rds","",vintage_string)
data_vintage<-max(vintage_string)

# write this to an rds file.
mrip_pull<-read_rds(file=file.path(output_folder, glue("mrip_pull{data_vintage}.Rds")))

# cast to upper case
mrip_pull <- map(mrip_pull, ~rename_with(.x, toupper))

# Compute Effort for different kind of targeting

types<-list(
  c('PRIM1', 'PRIM2'), # Targeting
  'A', # landing and seen
   c('A','B1'), # landed + dead
  'B2', # Releases
  c('PRIM1', 'PRIM2', 'A','B1', 'B2'), # any
  c('A','B1', 'B2') # Any catch
)
list_names <- types %>%
  map_chr(~ paste(.x, collapse = "|"))


# Sample code to get just Cod or Just Haddock
# targets_COD <- types %>%
#   map(~ mrip_effort(
#     dom = c("YEAR", "WAVE"),
#     microdata = mrip_pull,
#     dir_trip = list(
#       comname = c('ATLANTIC COD'),
#       typ = .x
#     )
#   )) %>%
#   set_names(list_names)
#
# targets_HADDOCK <- types %>%
#   map(~ mrip_effort(
#     dom = c("YEAR", "WAVE"),
#     microdata = mrip_pull,
#     dir_trip = list(
#       comname = c('HADDOCK'),
#       typ = .x
#     )
#   )) %>%
#   set_names(list_names)

# Target or caught Either

groundfish_effort <- types %>%
  map(~ mrip_effort(
    dom = c("YEAR", "WAVE","MODE1", "AREA_S"),
    microdata = mrip_pull,
    dir_trip = list(
      comname =  c('ATLANTIC COD', 'HADDOCK'),
      typ = .x
    )
  )) %>%
  set_names(list_names)


write_rds(groundfish_effort,file=file.path(output_folder, glue("groundfish_effort{data_vintage}.Rds")))


# The relationship of some of the entries
# PRIM1 : Cod + haddock=Either by definition
# PRIM2: Same
# Everything else: Either>= Cod, Either>=Haddock, Either<=Cod+Haddock

