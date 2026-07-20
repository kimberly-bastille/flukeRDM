* stock assessment numbers-at-age data
	* Min-Yang processes the historical numbers-at-age data and makes projections, and stores his output in Google Drive
	* Here I pull that data from Google Drive (using the Desktop app file path) and save it with a generic name in a local folder 

local date: display %td_CCYY_NN_DD date(c(current_date), "DMY")
local today_date_string = subinstr(trim("`date'"), " " , "_", .)
local vintage_string `today_date_string'


local google_folder "D:/Shared drives/NMFS NEC READ SSB/socialsci/RecreationalDST/2028_management_cycle_data/flukeRDM/input_data"

/* will need to adjust this to match actual file names*/



/* input csvs */
local bsb_assessN  "fit_NAA_NORTH_2024.csv"
local bsb_assessS  "fit_NAA_SOUTH_2024.csv"
local bsb_projectN  "fit_proj_NAA_NORTH_2026.csv"
local bsb_projectS "fit_proj_NAA_SOUTH_2026"

local scup_assess  "J1_2024Scup.csv"
local scup_project  "J1_2026Scup.csv"

local sf_assess  "J1_2024Summer_Flounder.csv"
local sf_project  "J1_2026Summer_Flounder.csv"

/*output filenames */
local SF_projected_filename "SummerFlounder_projected_NAA_`vintage_string'.dta"
local SF_historical_filename "SummerFlounder_historical_NAA_`vintage_string'.dta"

local Scup_projected_filename "Scup_projected_NAA_`vintage_string'.dta"
local Scup_historical_filename "Scup_historical_NAA_`vintage_string'.dta"

local BSB_South_projected_filename "BlackSeaBass_South_projected_NAA_`vintage_string'.dta"
local BSB_South_historical_filename "BlackSeaBass_South_historical_NAA_`vintage_string'.dta"

local BSB_North_projected_filename "BlackSeaBass_South_projected_NAA_`vintage_string'.dta"
local BSB_North_historical_filename "BlackSeaBass_South_historical_NAA_`vintage_string'.dta"


set seed 12345

/* Process stock assessment */
/* Summer flounder */
import delimited using "$misc_data_cd/`sf_assess'", clear


foreach var of varlist a0-a7{
	replace `var'=`var'/1000
}

gen fishery= "SFSBSB"
gen common= "SUMMER FLOUNDER"
gen state=""
gen wave=.
gen metric="2024 Numbers at Age"
gen source = "2024 Assessment"
gen stock_abbrev = ""
gen species_itis =172735
gen units = "Thousands"
gen region  = "CST"
gen str data_version= "`vintage_string'"

duplicates drop 
assert year==2024
drop year
sample $ndraws, count

save "$misc_data_cd/`SF_historical_filename'.dta", replace


import delimited using "$misc_data_cd/`sf_project'", clear

foreach var of varlist a0-a7{
	replace `var'=`var'/1000
}

gen fishery= "SFSBSB"
gen common= "SUMMER FLOUNDER"
gen state=""
gen wave=.
gen metric="2026 Projected Numbers At Age"
gen source = "2024 Assessment"
gen stock_abbrev = ""
gen species_itis =172735
gen units = "Thousands"
gen region  = "CST"
gen str data_version= "`vintage_string'"

duplicates drop 
assert year==2026

drop year
sample $ndraws, count

save "$misc_data_cd/`SF_projected_filename'.dta", replace

/*******************************************************/
/* SCUP */
/*******************************************************/
import delimited using "$misc_data_cd/`scup_assess'", clear


foreach var of varlist a0-a7{
	replace `var'=`var'/1000
}

gen fishery= "SFSBSB"
gen common= "SCUP"
gen state=""
gen wave=.
gen metric="2024 Numbers at Age"
gen source = "2024 Assessment"
gen stock_abbrev = ""
gen species_itis =172735
gen units = "Thousands"
gen region  = "CST"
gen str data_version= "`vintage_string'"

duplicates drop 
assert year==2024
drop year
sample $ndraws, count

save "$misc_data_cd/`Scup_historical_filename'.dta", replace


import delimited using "$misc_data_cd/`scup_project'", clear

foreach var of varlist a0-a7{
	replace `var'=`var'/1000
}

gen fishery= "SFSBSB"
gen common= "SCUP"
gen state=""
gen wave=.
gen metric="2026 Projected Numbers At Age"
gen source = "2024 Assessment"
gen stock_abbrev = ""
gen species_itis =169182
gen units = "Thousands"
gen region  = "CST"
gen str data_version= "`vintage_string'"

duplicates drop
assert year==2026 
drop year
sample $ndraws, count

save "$misc_data_cd/`Scup_projected_filename'.dta", replace



/*******************************************************/
/* Black Sea Bass */
/* Assessment */
/*******************************************************/
import delimited using "$misc_data_cd/`bsb_assessS'", clear

forv i = 1/8 {
    rename v`i' a`i'    
}


gen fishery= "SFSBSB"
gen common= "BLACK SEA BASS"
gen state=""
gen wave=.
gen metric="2024 Numbers at Age"
gen source = "2024 Assessment"
gen stock_abbrev = ""
gen species_itis =167687
gen units = "Thousands"
gen region  = "SOUTH"
gen str data_version= "`vintage_string'"

duplicates drop 
capture drop year
sample $ndraws, count

save "$misc_data_cd/`BSB_South_historical_filename'.dta", replace


import delimited using "$misc_data_cd/`bsb_assessN'", clear

forv i = 1/8 {
    rename v`i' a`i'    
}


gen fishery= "SFSBSB"
gen common= "BLACK SEA BASS"
gen state=""
gen wave=.
gen metric="2024 Numbers at Age"
gen source = "2024 Assessment"
gen stock_abbrev = ""
gen species_itis =167687
gen units = "Thousands"
gen region  = "NORTH"
gen str data_version= "`vintage_string'"

duplicates drop 
capture drop year
sample $ndraws, count

save "$misc_data_cd/`BSB_North_historical_filename'.dta", replace



/*******************************************************/
/* Black Sea Bass */
/* Assessment */
/*******************************************************/

import delimited using "$misc_data_cd/`bsb_projectS'", clear

forv i = 1/8 {
    rename v`i' a`i'    
}


gen fishery= "SFSBSB"
gen common= "BLACK SEA BASS"
gen state=""
gen wave=.
gen metric="2024 Numbers at Age"
gen source = "2024 Assessment"
gen stock_abbrev = ""
gen species_itis =167687
gen units = "Thousands"
gen region  = "SOUTH"
gen str data_version= "`vintage_string'"

duplicates drop 
capture drop year
sample $ndraws, count

save "$misc_data_cd/`BSB_South_projected_filename'.dta", replace


import delimited using "$misc_data_cd/`bsb_projectN'", clear

forv i = 1/8 {
    rename v`i' a`i'    
}


gen fishery= "SFSBSB"
gen common= "BLACK SEA BASS"
gen state=""
gen wave=.
gen metric="2026 Numbers at Age"
gen source = "2024 Assessment"
gen stock_abbrev = ""
gen species_itis =167687
gen units = "Thousands"
gen region  = "NORTH"
gen str data_version= "`vintage_string'"

duplicates drop 
capture drop year
sample $ndraws, count

save "$misc_data_cd/`BSB_North_projected_filename'.dta", replace

























