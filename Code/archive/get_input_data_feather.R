################################################################################
################################################################################
# ARCHIVED - NOT PRODUCTION CODE. See Code/archive/README.md.
# 
# Script:      get_input_data_feather.R
# Purpose:     Utility for fetching feather files from a Google Drive URL, covering
#              five groups of input files. Self-described in its own header as
#              utility code that is not used to run the model.
# Superseded by: no direct successor
#              Google Drive access now goes through Code/helpers/googledrivesetup.R and get_assessment_from_gdrive.do
# 
# This file is retained for reference and is not called by any wrapper,
# script or app in this repository. It is NOT maintained: paths, data
# formats and modeling choices in it may be years out of date, and it
# should not be used to understand how the pipeline currently behaves.
# Per the documentation session's scope, archived files received a header
# only - no inline documentation, and no code was changed.
################################################################################
################################################################################

# Code to get feather files from a google drive url.
# This is a piece of utility code, it is not used to run the actual RDM
# 
# There are 5 groups of files
# "base_outcomes_new_STATE_MODE_NN"
# "n_choice_occassion_new_STATE_MODE_NN"
# "proj_catch_draws_new_STATE_NN"
# "directed_trips_calibration_new_STATE"
# "proj_year_calendar_adjustments_new_STATE" (csv)
#  miscellaneous files:
#    calibrated_model_stats_new.rds, projected_catch_at_length_new.csv, 
#    SQ_weight_per_catch.xlsx, L_W_Conversion.csv

library(here)
library(googledrive)
library(purrr)
library(glue)
library(dplyr)
library(conflicted)
conflicts_prefer(dplyr::filter)
here::i_am("Code/pre_sim/get_input_data_feather.R")

#################################################################################
# Declare a preliminary or "new" data
#################################################################################
# If you want to get "preliminary data" or "both preliminary and new data", set new_insert to this:
#new_insert<-"_"

# If you want to ONLY "new", set new_insert to this:
new_insert<-"_new_"
#################################################################################



# # This puts the url for the flukeRDM gooogle drive into the object "flukeRDM_url"
source(here("Code","pre_sim","flukeRDM_url.R"))

flukeRDM<-drive_get(path=flukeRDM_input_data_url)
files_in_folder <- drive_ls(flukeRDM)

# There are alot of files.  This will copy them all to "Data". It WILL overwrite.
# It is not recursive
# Split the files list into  "base_outcomes" and everything else

search_string<-glue("^base_outcomes{new_insert}.*\\.feather$")

base_outcomes_list <- files_in_folder[grepl(search_string, files_in_folder$name, ignore.case = TRUE), ]
files_in_folder2 <- files_in_folder[!grepl(search_string, files_in_folder$name, ignore.case = TRUE), ]



walk2(
  base_outcomes_list$id,
  base_outcomes_list$name,
  ~ drive_download(
    file = .x,
   path = here("Data",.y),
   overwrite = TRUE
  )
)
######################################################
# Get all the "n_choice_occasions" files
######################################################

search_string<-glue("^n_choice_occasions{new_insert}.*\\.feather$")

n_choice_occasions_list <- files_in_folder2[grepl(search_string, files_in_folder2$name, ignore.case = TRUE), ]
files_in_folder3 <- files_in_folder2[!grepl(search_string, files_in_folder2$name, ignore.case = TRUE), ]

walk2(
  n_choice_occasions_list$id,
  n_choice_occasions_list$name,
  ~ drive_download(
    file = .x,
    path = here("Data",.y),
    overwrite = TRUE
  )
)


######################################################
# Get all the "proj_catch_draws_new" files
######################################################

search_string<-glue("^proj_catch_draws{new_insert}.*\\.feather$")


proj_catch_draws_list <- files_in_folder3[grepl(search_string, files_in_folder3$name, ignore.case = TRUE), ]

files_in_folder4 <- files_in_folder3[!grepl(search_string, files_in_folder3$name, ignore.case = TRUE), ]

walk2(
  proj_catch_draws_list$id,
  proj_catch_draws_list$name,
  ~ drive_download(
    file = .x,
    path = here("Data",.y),
    overwrite = TRUE
  )
)
######################################################
# Get all the "directed_trips_calibration" files
######################################################

search_string<-glue("^directed_trips_calibration{new_insert}.*\\.feather$")

directed_trips_calibration_list <- files_in_folder[grepl(search_string, files_in_folder$name, ignore.case = TRUE), ]

files_in_folder5 <- files_in_folder4[!grepl(search_string, files_in_folder4$name, ignore.case = TRUE), ]

walk2(
  directed_trips_calibration_list$id,
  directed_trips_calibration_list$name,
  ~ drive_download(
    file = .x,
    path = here("Data",.y),
    overwrite = TRUE
  )
)


######################################################
# Get all the "proj_year_calendar_adjustments" files
######################################################
search_string<-glue("^proj_year_calendar_adjustments{new_insert}.*\\.csv$")

proj_year_calendar_adjustments_list <- files_in_folder5[grepl(search_string, files_in_folder5$name, ignore.case = TRUE), ]
files_in_folder6 <- files_in_folder5[!grepl(search_string, files_in_folder5$name, ignore.case = TRUE), ]



walk2(
  proj_year_calendar_adjustments_list$id,
  proj_year_calendar_adjustments_list$name,
  ~ drive_download(
    file = .x,
    path = here("Data",.y),
    overwrite = TRUE
  )
)




# # Get all the miscellaneous files
#    calibrated_model_stats_new.rds, projected_catch_at_length_new.csv, 
#    SQ_weight_per_catch.xlsx, L_W_Conversion.csv

m1 <- files_in_folder6[grepl("^calibrated_model_stats_new*.\\.rds$", files_in_folder6$name, ignore.case = TRUE), ]
files_in_folder7 <- files_in_folder6[!grepl("^calibrated_model_stats_new*\\.rds$", files_in_folder6$name, ignore.case = TRUE), ]

m2 <- files_in_folder7[grepl("^projected_catch_at_length.*\\.csv$", files_in_folder7$name, ignore.case = TRUE), ]
files_in_folder7 <- files_in_folder7[!grepl("^projected_catch_at_length.*\\.csv$", files_in_folder7$name, ignore.case = TRUE), ]

m3 <- files_in_folder7[grepl("^SQ_weight_per_catch\\.xlsx$", files_in_folder7$name, ignore.case = TRUE), ]
files_in_folder7 <- files_in_folder7[!grepl("^SQ_weight_per_catch*\\.csv$", files_in_folder7$name, ignore.case = TRUE), ]

m4 <- files_in_folder7[grepl("^L_W_Conversion\\.csv$", files_in_folder7$name, ignore.case = TRUE), ]


#Hopefully files_in_folder7 is empty. It might not be, if there are files left, keep adding them. If there are directories, then continue onwards
files_in_folder7 <- files_in_folder7[!grepl("^L_W_Conversion*\\.csv$", files_in_folder7$name, ignore.case = TRUE), ]

miscellaneous<-rbind(m1,m2,m3,m4)



walk2(
  miscellaneous$id,
  miscellaneous$name,
  ~ drive_download(
    file = .x,
    path = here("Data",.y),
    overwrite = TRUE
  )
)






###############################################################################
###############################################################################
###############################################################################
# Function to recursively get all files and their paths
get_files_recursive <- function(folder_item, current_path = "") {
  # Get contents of current folder
  contents <- drive_ls(folder_item)
  
  all_files <- data.frame()
  
  if (nrow(contents) > 0) {
    for (i in 1:nrow(contents)) {
      item <- contents[i, ]
      item_path <- file.path(current_path, item$name)
      
      if (item$drive_resource[[1]]$mimeType == "application/vnd.google-apps.folder") {
        # It's a folder - recurse into it
        subfolder_files <- get_files_recursive(item, item_path)
        all_files <- rbind(all_files, subfolder_files)
      } else {
        # It's a file - add to list
        file_info <- data.frame(
          id = item$id,
          name = item$name,
          path = current_path,
          full_path = item_path,
          stringsAsFactors = FALSE
        )
        all_files <- rbind(all_files, file_info)
      }
    }
  }
  
  return(all_files)
}



###############################################################################
# Get all subfolders
subfolder_list<-c("a_projected_catch_draws","aa_L_W_conversion","aaa_catch_weights_SQ")

# Code to preserve directories.
# for (k in 1:length(subfolder_list)) {
#   
#   subfolder_name <- subfolder_list[k]
#     
#   subfolder <- files_in_folder %>% 
#     filter(name == subfolder_name)
# 
#   if (nrow(subfolder) > 0) {
#     # Download from this specific subfolder
#     subfolder_files <- get_files_recursive(subfolder[1, ], subfolder_name)
#     
#     # Download preserving structure
#     for (i in 1:nrow(subfolder_files)) {
#       local_path <- here("Data", subfolder_files$path[i])
#       if (!dir.exists(local_path) && local_path != here("Data")) {
#         dir.create(local_path, recursive = TRUE)
#       }
#       
#       local_file_path <- here("Data", subfolder_files$full_path[i])
#       drive_download(
#         file = subfolder_files$id[i],
#         path = local_file_path,
#         overwrite = TRUE
#       )
#       
#       cat("Downloaded:", subfolder_files$full_path[i], "\n")
#     }
#   }
# }


###############################################################################
###############################################################################
###############################################################################


###############################################################################
# No paths,everything into the Data directory
###############################################################################

for (k in 1:length(subfolder_list)) {
  subfolder_name <- subfolder_list[k]
  subfolder <- files_in_folder7 %>% 
     filter(name == subfolder_name)
  
  if (nrow(subfolder) > 0) {
    # Download from this specific subfolder
    subfolder_files <- get_files_recursive(subfolder[1, ], subfolder_name)

    # Download preserving structure
    for (i in 1:nrow(subfolder_files)) {
      local_path <- here("Data", subfolder_files$path[i])
      local_file_path <- here("Data", subfolder_files$full_path[i])
      drive_download(
        file = subfolder_files$id[i],
        path = here("Data",subfolder_files$name[i]),
        overwrite = TRUE
      )

      cat("Downloaded:", subfolder_files$name[i], "\n")
    }
  }
}



