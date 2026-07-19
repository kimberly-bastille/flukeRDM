################################################################################
################################################################################
# Script:       rdb_catch_per_trip_to_drive.R
# Purpose:      Converts the simulated catch-per-trip file used by the
#               recreational dashboard from Stata .dta to .Rds, stamps the
#               data_version into the output filename, and uploads the result
#               to the shared NMFS Google Drive so dashboard consumers can
#               pull it without repo access.
# Inputs:       rdb_sim_catch_per_trip.dta
# Outputs:      rdb_catch_per_trip_<data_version>.Rds (written locally and
#               uploaded to Drive under the same name).
# Dependencies: Sources Code/helpers/developer_setup.R, so the object
#               `developer` must be set first. Packages tidyverse, haven,
#               glue, googledrive, here. Requires a cached OAuth token in
#               .secrets/ - run Code/helpers/googledrivesetup.R once first.
# Pipeline:     Terminal step - nothing downstream in this repo reads its
#               output. Intended to be gated by the model_wrapper.do toggle
#               `Rpush_to_gdrive', but NO CALL TO THIS SCRIPT EXISTS in the
#               wrapper; the toggle is defined, defaults OFF, and is labeled
#               "WRITTEN but not tested". Running this file therefore means
#               invoking it by hand.
################################################################################
################################################################################

#This code reads in a catch per trip dta for the rec dashboard and uploads it to Google drive as an Rds


#Load libraries
library(tidyverse)
library(haven)
library(glue)
library(googledrive)
library(here)

here::i_am("Code/pre_sim/rdb_catch_per_trip_to_drive.R")
source(here("Code", "helpers", "developer_setup.R"))

################################################################################
################################################################################
# Section A: Read the .dta and rewrite it as a version-stamped .Rds
################################################################################
################################################################################

output_folder<-file.path(sf.data.dir, "miscellaneous")

input_file <- file.path(sf.data.dir,"miscellaneous","rdb_sim_catch_per_trip.dta")

# Read in my .dta file
rdb_catch_per_trip <- read_dta(input_file)

# data_version is a constant column across the whole file - it records which
# vintage of the input data the simulation was run against. Taking element [1]
# is therefore reading a file-level attribute, not sampling a row.
# Save the data version
file_date <- rdb_catch_per_trip$data_version[1]
SimCPTSaveFile<-glue("rdb_catch_per_trip_{file_date}")

# convert character date to a date variable
rdb_catch_per_trip$data_version<-as.Date(rdb_catch_per_trip$data_version)

# Save dataframe as Rds
write_rds(rdb_catch_per_trip, file=file.path(output_folder,glue("{SimCPTSaveFile}.Rds")))


################################################################################
################################################################################
# Section B: Upload to the shared Google Drive
################################################################################
################################################################################

message("rdb_catch_per_trip_to_drive.R: authenticating to Google Drive and uploading ", SimCPTSaveFile, ".Rds. This may take a few minutes depending on file size and connection.")

# Connect to Google Drive
# NOTE: Relies on cached credentials in .secrets. Will prompt interactive auth if missing or expired.
drive_auth(cache = here(".secrets"), email = TRUE)

# Output folder on google drive
miscellaneous_path <-file.path("socialsci","RecreationalDST","2028_management_cycle_data",
                               "flukeRDM","miscellaneous")

# drive_get() resolves the human-readable folder path to a Drive file ID.
# drive_upload() below needs the ID wrapped in as_id(); passing the path string
# directly would make it create a literal folder of that name instead.
folder_info <- drive_get(
  path = miscellaneous_path,
  shared_drive = "NMFS NEC READ SSB"
)
miscellaneous_path<-folder_info$id


#Put the catch per trip Rds on google drive
drive_upload(
  media = file.path(output_folder,glue("{SimCPTSaveFile}.Rds")),
  path = as_id(miscellaneous_path),
  name = glue("{SimCPTSaveFile}.Rds"),
  overwrite = TRUE
)

message("rdb_catch_per_trip_to_drive.R: finished. Uploaded ", SimCPTSaveFile, ".Rds to the shared Drive.")


