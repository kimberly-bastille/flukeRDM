#This code reads in a catch per trip dta for the rec dashboard and uploads it to Google drive as an Rds


#Load libraries
library(tidyverse)
library(haven)
library(glue)
library(googledrive)
library(here)

here::i_am("Code/pre_sim/rdb_convert_and_push_NAA_to_gdrive.R")
source(here("Code", "helpers", "developer_setup.R"))
source(here("Code","helpers","naa_helpers.R"))

misc_data_dir<-file.path(sf.data.dir, "miscellaneous")

# Find the most recent files. Assume that all 6 age structures are read on the same day.

file_pattern<-"SummerFlounder_projectedNAA_"
data_vintage_string<-list.files(misc_data_dir, pattern=glob2rx(glue("{file_pattern}*.dta")))
data_vintage_string<-gsub(file_pattern,"",data_vintage_string)
data_vintage_string<-gsub(".dta","",data_vintage_string)
data_vintage_string<-max(data_vintage_string)


SFSBSB_filestubs_projected<-c("SummerFlounder_projectedNAA",
                        "Scup_projectedNAA",
                        "BlackSeaBassSouth_projectedNAA",
                        "BlackSeaBassNorth_projectedNAA"
)


SFSBSB_filestubs_historical<-c("SummerFlounder_historicalNAA",
             "Scup_historicalNAA",
             "BlackSeaBassSouth_historicalNAA",
             "BlackSeaBassNorth_historicalNAA"
)

SFSBSB_filestubs<-c("SummerFlounder_historicalNAA",
                        "Scup_historicalNAA",
                        "BlackSeaBassSouth_historicalNAA",
                        "BlackSeaBassNorth_historicalNAA",
                        "SummerFlounder_projectedNAA",
                        "Scup_projectedNAA",
                        "BlackSeaBassSouth_projectedNAA",
                        "BlackSeaBassNorth_projectedNAA"
)
NAA_long_holder<-list()

#I'm writing a loop instead of an lapply. Sorry.
for (file_in in SFSBSB_filestubs){

  input_file_and_path <- file.path(misc_data_dir,glue("{file_in}_{data_vintage_string}.dta"))
  
  output_file<-glue("{file_in}_{data_vintage_string}.Rds")
  output_file_and_path<- file.path(misc_data_dir,output_file)
  
  # Read in my .dta file
  working_NAA <- read_dta(input_file_and_path)
  working_NAA <- working_NAA  %>%
    zap_formats() %>%
    zap_label() %>%
    mutate(data_version=ymd(data_version)) %>%
    relocate(year, fishery, common, species_itis,stock_abbrev, state, wave, metric,units, source, data_version)
  NAA_long<-pivot_naa_long(working_NAA)

  validate_naa_data(NAA_long)
  # Save dataframe as Rds
  write_rds(NAA_long, file=output_file_and_path)
  message(file_in, " saved as .Rds")
  
  NAA_long_holder[[file_in]]<-NAA_long
  }
  
# Ensure we have the right number of files
stopifnot(length(NAA_long_holder)==length(filestubs))  
  
  # Connect to Google Drive
  # NOTE: Relies on cached credentials in .secrets. Will prompt interactive auth if missing or expired.
  drive_auth(cache = here(".secrets"), email = TRUE)
  
  # Output folder on google drive
  miscellaneous_path <-file.path("socialsci","RecreationalDST","2028_management_cycle_data",
                                 "flukeRDM","miscellaneous")
  
  message("Searching for file path")
  # Get the id of that folder.
  folder_info <- drive_get(
    path = miscellaneous_path,
    shared_drive = "NMFS NEC READ SSB"
  )
  miscellaneous_path<-folder_info$id
  
  
  
# push the NAA to google drive  
for (file_in in filestubs){
    
    output_file<-glue("{file_in}_{data_vintage_string}.Rds")
    output_file_and_path<- file.path(misc_data_dir,output_file)
    
  #Put the catch per trip Rds on google drive
  drive_upload(
    media = file.path(output_file_and_path),
    path = as_id(miscellaneous_path),
    name = output_file,
    overwrite = TRUE
  )
  message(output_file, " Written to GoogleDrive")
  
}
