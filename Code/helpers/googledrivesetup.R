################################################################################
################################################################################
# Script:       googledrivesetup.R
# Purpose:      One-time, manually-run instructions for caching a Google Drive
#               OAuth token into the repo's .secrets/ folder, so that scripts
#               which read from or write to the shared NMFS Drive can
#               authenticate without an interactive browser prompt on every
#               run. This file is documentation in executable form: every line
#               is commented out and nothing happens if it is sourced.
# Inputs:       None.
# Outputs:      None directly. Following the steps by hand writes a cached
#               credential file into .secrets/.
# Dependencies: Packages here, googledrive. Must be run interactively in
#               RStudio - the drive_auth() step opens a browser.
# Pipeline:     Setup stage, outside the pipeline. Not called by any wrapper
#               and not sourced by any script. Run once per developer per
#               machine. The scripts that consume the cached token are
#               get_assessment_from_gdrive.do and
#               rdb_catch_per_trip_to_drive.R.
################################################################################
################################################################################

# Run this file once, to set up your token

#########################################################################
####################RUN this once to setup your token####################
#########################################################################
# Open the project in Rstudio
#
#library(here)

#options(gargle_oauth_cache = here(".secrets"))
#
#library(googledrive)
#
# # Put your email in, then run to authorize your token.
#drive_auth(email = "your email here")
#
#
# # Verify tokens were cached
# list.files(here(".secrets"))

#########################################################################
####################END of token setup ##################################
#########################################################################


# In any code that you have that needs access to google drive, run the following:
# library(here)
# library(googledrive)
# drive_auth(cache = here(".secrets"), email = TRUE)






