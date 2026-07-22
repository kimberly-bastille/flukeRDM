

/**** SFSBSB RDM code wrapper ****/
/* This uses the user written command here to set directories*/
/* It is not as good as R's version. Before running this code, you must change directories into project directory 
One easy way to do this is to add a line to your profile do that store that directory in the 
global flukeRDMdir "path to this project"
and then cd "$flukeRDMdir" right before running this code

**Data availability**

* We make next year's projections in Oct./November
	// Wave 4 MRIP data available Sepetember 15th

* MRIP effort data: most recent full fishing year

* MRIP length data: we combine MRIP data with VAS discard length data. 
	// sources of VAS data used for discard length: NJ VAS, ALS, CT VAS, RI VAS, MRIP

* MRIP catch data for calibration year: most recent full fishing year
		
* MRIP catch data for projection year: most recent 12 waves of MRIP data
		// For FY 2026 projections: we use MRIP data from 2025 waves 1 to 4,  2024 waves 1 to 6 , 2023 wave 5 and 6
		
* Stock assessment projections data:
		// Jan 1 2024 NAA to compute historical rec. selectivity 
		// Jan 1 2026 NAA to compute projected catch-at-length
		
* NEFSC trawl survey data from 2024, 2023, 2022 years used to create age-length keys

* MRIP data is stored in  
	// "smb://net/mrfss/products/mrip_estim/Public_data_cal2018"
	// Windows, just mount \\net.nefsc.noaa.gov\mrfss to A:\


	
* Dependencies
* ssc install xsvmat 
* ssc install gammafit 
* ssc install grc1leg
* ssc install rscript
	*/
	
	
set varabbrev on


**Adjust globals**

* These need to be changed every year 

* year-waves of MRIP data. 
global yr_wvs 20221 20222 20223 20224 20225 20226 20231 20232 20233 20234 20235 20236  20241 20242 20243 20244 20245  20246 20251 20252 20253 20254 20255 20256
global yearlist 2022 2023 2024 2025
global wavelist 1 2 3 4 5 6

global calibration_year "(year==2024 & inlist(wave, 1, 2, 3, 4, 5, 6))"
global calibration_year_num 2024

global projection_catch_per_trip_years "(year==2025 & inlist(wave, 1, 2, 3, 4)) | (year==2024) | (year==2023) | (year==2022 & inlist(wave, 5, 6))" //ADJUST THIS AFTER MRIP DATA RELEASE

global calibration_start_date td(01jan2024)
global calibration_end_date td(31dec2024)

global projection_date_start td(01jan2026)
global projection_date_end td(31dec2026)


* Federal holidays are are considered "weekend" days by the MRIP 
	// store in global list for estimation of fishing effort at the month and kind-of-day (weekend/weekday) level
/*
Federal Holidays Included:
New Year's Day: January 1 (observed on January 2 if it falls on Sunday).
Martin Luther King Jr. Day: Third Monday in January.
Presidents' Day: Third Monday in February.
Memorial Day: Last Monday in May.
Juneteenth National Independence Day: June 19.
Independence Day: July 4 (observed on July 5 if it falls on Sunday).
Labor Day: First Monday in September.
Columbus Day: Second Monday in October.
Veterans Day: November 11 (observed on November 10 if it falls on Sunday).
Thanksgiving Day: Fourth Thursday in November.
Christmas Day: December 25 (observed on December 26 if it falls on Sunday).
*/


* Federal holidays in the calibration year 
global fed_holidays "inlist(day, td(10nov2023), td(23nov2023), td(25dec2023), td(01jan2024), td(15jan2024), td(19feb2024), td(27may2024), td(19jun2024), td(04jul2024), td(02sep2024), td(14oct2024), td(11nov2024), td(28nov2024), td(25dec2024))" 

* Fed holidays in the projection year 
global fed_holidays_y2 "inlist(day_y2, td(01jan2026), td(19jan2026), td(16feb2026), td(25may2026), td(19jun2026), td(03jul2026), td(07sep2026), td(12oct2026), td(11nov2026), td(26nov2026), td(25dec2026))"

* Put leap-year days here
global leap_yr_days "td(29feb2024)" 

* Number of model iterations
global ndraws 100

* set years of which to pull the NEFSC trawl survey data
global NEFSC_svy_yrs "inlist(year,2024, 2023, 2022)"

* Adjustment to 2022 survey trip costs to account for inflation
	// https://www.bls.gov/data/inflation_calculator.htm, January 2022 - January 202X 
global inflation_expansion=1.31 

/* find the root of the project 
prior to running the wrapper, you must change to $groundfishRDMdir so here picks up the project
*/

here, nogit 
do "${here}/Code/helpers/developer_setup_stata.do"

* Adjust project paths based on developer
global input_code_cd "${here}/Code/pre_sim" 
global misc_data_cd "${sfdatadir}/miscellaneous" 
global calib_catch_data_cd "${sfdatadir}\calib_catch_draws"
global proj_catch_data_cd "${sfdatadir}\proj_catch_draws"
global figure_cd  "${sfdatadir}\figures"

global log_dir "${input_code_cd}/logs" 

/* make directories if necessary */
capture mkdir $misc_data_cd
capture mkdir $calib_catch_draws_cd
capture mkdir $proj_catch_data_cd
capture mkdir $figure_cd
capture mkdir $log_dir

/* start log */
cap log close
log using "${log_dir}\sf_model_wrapper_log_$S_DATE.smcl", replace
														   


global seed 03211990


**********************************************************************
************************ EXECUTION CONTROL ***************************
**********************************************************************

// Control which modules to run (set to 0 to skip)
loc pull_assessment = 1		 		// Pull Assessment data
loc processMRIP = 1		 			// deal with casing MRIP data
loc assemblemriplists = 1		 	// deal with casing MRIP data

loc estimate_dtrips = 1				// Estimate Directed Trips 
loc costs_per_trip = 1			// Create Distributions of costs per trip (run 1x)
loc draw_angler_preferences = 1		// Create draw of angler preference parameters (run 1x)
loc catch_per_trip1 = 1				// Part 1 of catch per trip
loc copula_in_R = 1					// Copula model in R
loc catch_per_trip2 = 1				// Part 2 of catch per trip
loc compare_calibration_MRIP = 1	// compare calibration output to MRIP
loc prep_cpt_for_dashboard= 0		// prep data for dashboard NOT IN WRAPPER. NOT WRITTEN, See Groundfish repo
loc Rpush_to_gdrive =0 				// Push to google drive in R NOT IN WRAPPER. WRITTEN but not tested 
loc angler_demogs	=1				// add additonal angler demographics
loc generate_baseline=1				// Generate baseline-year catch-at-length
loc catch_at_length_project=1		// Generate projection-year catch-at-length
loc catch_per_trip_project=1       // Generate projection-year catch-per trip

loc prep_NAA_for_dashboard = 1		// Pull Assessment data
loc push_NAA_to_gdrive =1 			// Convert Assessment data to Rds, reshape to long, and push to googledrive


// Prototyping
local proto = 0

if `proto' {
	global ndraws 3
}

**************************************************Model calibration ************************************************** 

// 0) Pull Assessment data from google.

/* This code requires you to mount your google drive to D on your computer */
if `pull_assessment' {
	di "Pulling Assessment data from google"
	do "$input_code_cd\get_assessment_from_gdrive.do"
	}

	/* This code requires you to mount your google drive to D on your computer */
if `prep_NAA_for_dashboard' {
	di "Pulling Assessment data from google"
	do "$input_code_cd\rdb_processing_NAA.do"
	}
if `push_NAA_to_gdrive' {
	di "Converting dta to Rds and pushing to GDrive"
	
			rscript using "$input_code_cd\rdb_convert_and_push_NAA_to_gdrive.R"
	di "NAA pushed to GDrive"

	}

	
	

// 1) Pull the MRIP data

if `processMRIP' {
	di "Processing MRIP data"
	do "$input_code_cd\MRIP_column_cases.do"
	di "MRIP data processed"
}

if `assemblemriplists' {
	di "Assembling Lists of MRIP files"
	do "$input_code_cd\MRIP_lists.do"
	di "Lists of MRIP files assembled"
}

	
// 2) Estimate directed trips during calibration period
		// This file calls "set_regulations.do". You must enter the SQ regulations in the calibration and projection year. 
		// THIS NEEDS TO BE ADJUSTED EVERY YEAR. 

if `estimate_dtrips' {

	di "Estimating Directed trips"
    do "$input_code_cd\directed_trips_calibration.do"
	di "Directed trips Estimated"

}

// 3) Create distirbutions of costs per trip across strata

if `costs_per_trip' {
	di "Creating distributions of cost per trip"
	do "$input_code_cd\survey_trip_costs.do"
	di "distributions of cost per trip Done"

}

// 4) Create draw of angler preference parameters - only needs to be run once
if `draw_angler_preferences' {
	di "Creating draws of angler preference parameters"
	do "$input_code_cd\estimate_angler_preferences.do"
	di "Draws of angler preference parameters Done"

}
* 5) Estimate catch-per-trip at the month and mode level
		// a) compute mean catch-per-trip and standard error, imputing standard errors from historcial data when they are missing. 

if `catch_per_trip1' {
	di "Estimate catch-per-trip at the month and mode level"
	do "$input_code_cd\catch_per_trip_calibration_part1.do"
	di "catch-per-trip at the month and mode level Done"

}

		// b) use copula model (in R) to simulate harvest and discards per-trip
if `copula_in_R' {

    	di "Estimating copula in R. This takes a while and will look like it's hung"

		rscript using "$input_code_cd\copula_modeling_calibration.R"
	  	di "Copula in R estimated"
}

		// c) generate estimates of simulated total harvest based on random draws of catch-per-trip and directed trips

if `catch_per_trip2' {
    	di "Generating estimates of simulated total harvest based on random draws"
		do "$input_code_cd\calibration_catch_per_trip_part2.do"
    	di "Estimates of simulated total harvest Done"

}				  

// 6) compare calibration output to MRIP, and retain total simulated harvest and discards to apply to the baseline catch-at-length distribution

if `compare_calibration_MRIP' {
    	di "Comparing calibration output to MRIP"
		do "$input_code_cd\compare_calibration_data_to_MRIP.do" 
      	di "Comparison of calibration output to MRIP done"
}


// 7) Generate baseline-year catch-at-length, using the simulated harvest/discard totals from step 6

if `generate_baseline'{
    	di "Generating baseline catch-at-length" 
		do "$input_code_cd\calibration_catch_at_length.do"
    	di "Baseline catch-at-length generated " 

		}

// 8) Generate projection-year catch-at-length, incorporating the stock assessment data
if `catch_at_length_project'{
		di "Generating projection year catch-at-length" 
		do "$input_code_cd\projected_catch_at_length.do"
		di "Projection year catch-at-length generated " 

}
	
	
if `catch_per_trip_project'{

// 9)  Estimate projected catch-per-trips at the month and mode level
		 *use MRIP catch data from the last THREE full years. 
		 
		//a) compute mean catch-per-trip and standard error, imputing standard errors from historcial data when they are missing. 
		 do "$input_code_cd\catch_per_trip_projection_part1.do"

		//b) use copula model (in R) to simulate harvest and discards per-trip
		rscript using "$input_code_cd\copula_modeling_projection.R"
		
		//c) generate estimates of simulated total harvest based on random draws of catch-per-trip and directed trips
		do "$input_code_cd\catch_per_trip_projection_part2.do"
		
		//d) compare estimates of mean projected catch to MRIP data to ensure consistency and remove extraneous columns from projected catch draw data
		do "$input_code_cd\compare_projection_data_to_MRIP.do"
}

// 10) Run the projection loop in R











