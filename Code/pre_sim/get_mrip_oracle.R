################################################################################
# Script:       get_mrip_oracle.R
# Purpose:      Pulls MRIP recreational microdata (trip, catch, size, size_b2)
#               from Oracle via the mriptacklebox package for a year range,
#               lower-cases names, stamps a pull date, forces id columns to
#               character, and writes per-element .dta files plus a combined .Rds.
# Inputs:       Command-line args: first_year last_year. Live Oracle connection
#               (mriptacklebox's nefscdb_con).
# Outputs:      <gf.data.dir>/miscellaneous/mrip_{trip,catch,size,size_b2}.dta and
#               mrip_pull<today>.Rds.
# Dependencies: Sources developer_setup.R (for gf.data.dir). Requires Oracle
#               access to MRIP data tables and RECDBS schema
# Pipeline:     Step 2 of model_wrapper.do (gated by pull_MRIP), invoked via
#               `rscript using ... args(first last)`, and followed immediately by
#               tidyup_mrip_data_fromR.do. Also runnable standalone:
#               Rscript get_mrip_oracle.R 2023 2025.
################################################################################


# Define arguments
args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 2) {
  stop("Error: This script requires exactly two arguments.", call. = FALSE)
}

#read in arguments. Ensure they are numeric
first_yr  <- as.numeric(args[1])
last_yr   <-  as.numeric(args[2])
#first_yr<-2023
#last_yr<-2025

# Show them, just in case.
cat("First Year:", first_yr, "\n")
cat("Last Year:", last_yr, "\n")


# Load libraries
# install the main branch
#remotes::install_github("NEFSC/READ-PDB-mriptacklebox")

library("here")
library("mriptacklebox")
library("ROracle")
library("tidyverse")
library("DBI")
library("glue")
library("haven")
library("conflicted")
conflicts_prefer(dplyr::filter)
conflicts_prefer(dplyr::lag)


# standard "here", username setup, and paths
here::i_am("Code/pre_sim/get_mrip_oracle.R")
source(here("Code", "helpers", "developer_setup.R"))
output_folder<-file.path(gf.data.dir, "miscellaneous")

#for help with versioning
todaysdate<-Sys.Date()

# Connect to Oracle
drv<-dbDriver("Oracle")
con_name<-eval(nefscdb_con)


yearlist<-first_yr:last_yr
wavelist<-1:6

# pull data and then disconnect
message("Pulling MRIP microdata from Oracle (this can take a while) ...")
mrip_pull <- mrip_microdata(
  years = yearlist, waves = wavelist,
  typ = c('trip', 'catch', 'size', 'size_b2'),
  format = c('nefsc_db'),
  nefsc_db_con=con_name
)
message("MRIP microdata from Oracle read in...")



message("Pulling Site List from Oracle...")

new_site_list<-glue("select * from RECDBS.MRIP_COD_ALL_SITE_LIST")
site_list<-dbGetQuery(con_name, new_site_list)
dbDisconnect(con_name)

message("Processing MA and ME Sites")
site_list_WGOM_COD<-site_list %>%
  filter(STATE %in% c("MA", "ME")) %>%
  select(c(STATE, INTSITE, NMFS_STOCK_AREA, NMFS_STAT_AREA))  %>%
  mutate(NMFS_STOCK_AREA=case_when(
    NMFS_STAT_AREA %in% c("521", "526", "541", "514", "513", "515") ~ "WGOM",
    TRUE ~ "XX"
    )
  ) %>%
  distinct()


message("Data Munging")

# append a mrip_pull_date column (today's date) to every element, formatted as
# "Month DD, YYYY" (the %B %d, %Y example renders e.g. as "July 16, 2026")
mrip_pull <- map(mrip_pull, ~ mutate(
  .x, MRIP_PULL_DATE =as.character(format(todaysdate,"%B %d, %Y") ) )
)

# Consolidate modes
mrip_pull <- map(mrip_pull, ~ mutate(
  .x, MODE1 =case_when(
    MODE_FX %in% c("1","2","3") ~ "sh",
    MODE_FX %in% c("7") ~ "pr",
    MODE_FX %in% c("4","5") ~ "fh",
    TRUE ~ "oth")
  )
)

#Bring the site list info into trip
#Everything not in WGOM is allocated to XX, but they could be allocated to other stockareas
#If you had the definitions.
mrip_pull$trip <- mrip_pull$trip %>%
  left_join(
  site_list_WGOM_COD, by=join_by(INTSITE==INTSITE, ST_ABB==STATE)
  ) %>%
  mutate(AREA_S=case_when(
    ST =="33" ~ "WGOM",
    ST %in%  c("25","23") ~ NMFS_STOCK_AREA,
    TRUE ~ "XX"
  )) %>%
    select(-c("NMFS_STAT_AREA","NMFS_STOCK_AREA")
)

# append the mrip_pull_date to the mrip_pull list as a tibble

datestamp<-as_tibble(todaysdate)
colnames(datestamp)<-"mrip_pull_date"
mrip_pull$mrip_pull_date<-datestamp

# write this to an rds file.
write_rds(mrip_pull, file=file.path(output_folder, glue("mrip_pull{todaysdate}.Rds")))

message("First Year in Data: ",first_yr)
message("Last Year in Data: ",last_yr)

message("MRIP data successfully pulled on: ", format(todaysdate,"%B %d, %Y") )



# A little data munging
# Downstream stata code needs to be in all caps and we need to ensure the date formats are done properly
# all lower case

mrip_pull$mrip_pull_date<-NULL

mrip_pull <- map(mrip_pull, ~rename_with(.x, tolower)
                 )

#force certain things to character
mrip_pull <- map(mrip_pull, ~ mutate(
  .x, across(c(strat_id, psu_id, id_code,zip), as.character))
  )

# You might need to delete a few columns of of data if the downstream stata code doesn't work.
#MODE1, AREA_S


# write all the elements of x to a dta file
walk2(mrip_pull, names(mrip_pull), ~ write_dta(
  .x,
  path=file.path(output_folder, glue("mrip_{.y}.dta"))
  )
)


