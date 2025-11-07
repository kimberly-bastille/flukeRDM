

/****RDM input code wrapper****/

*************************
****Data availability****
*************************

*We make next year's projections in Oct./November
	*Wave 4 MRIP data available Sepetember 15th

*MRIP effort data: most recent full fishing year

*MRIP length data: we combine MRIP data with VAS discard length data. 
	* sources of VAS data used for discard length: NJ VAS, ALS, CT VAS, RI VAS, MRIP

*MRIP catch data for calibration year:	most recent full fishing year
		
*MRIP catch data for projection year:	use the most recent 12 waves of MRIP data
		*For FY 2026 projections: we use MRIP data from 2025 waves 1 to 4,  2024 waves 1 to 6 , 2023 wave 5 and 6
		
*Stock assessment projections data:
		*Jan 1 2024 NAA to compute historical rec. selectivity 
		*Jan 1 2026 NAA to compute projected catch-at-length
		
*NEFSC trawl survey data from 2024, 2023, 2022 years used to create age-length keys
		*CHECK

*MRIP data is stored in  
	*"smb://net/mrfss/products/mrip_estim/Public_data_cal2018"
	*Windows, just mount \\net.nefsc.noaa.gov\mrfss to A:\

* Dependencies
*ssc install xsvmat 
*ssc install gammafit 



**************************************************ADJUST GLOBALS************************************************** 	
*These need to be changed every year 

*Years/waves of MRIP data. 
global yr_wvs 20221 20222 20223 20224 20225 20226 20231 20232 20233 20234 20235 20236  20241 20242 20243 20244 20245  20246 20251 20252 20253
global yearlist 2022 2023 2024 2025
global wavelist 1 2 3 4 5 6


global calibration_year "(year==2024 & inlist(wave, 1, 2, 3, 4, 5, 6))"
global calibration_year_num 2024

global projection_year "(year==2025 & inlist(wave, 1, 2, 3)) | (year==2024) | (year==2022 & inlist(wave, 4, 5, 6))" //ADJUST THIS AFTER MRIP DATA RELEASE

global calibration_catch_per_trip_years "(year==2024 & inlist(wave, 1, 2, 3, 4, 5)) | (year==2023 & inlist(wave, 6)) | (year==2023 & inlist(wave, 1, 2, 3, 4, 5)) | (year==2022 & inlist(wave, 6))"

global sq_weight_per_fish_years "(year==2024 & inlist(wave, 1, 2, 3, 4, 5, 6))" 

global calibration_start_date td(01jan2024)
global calibration_end_date td(31dec2024)

global projection_date_start td(01jan2026)
global projection_date_end td(31dec2026)


*Add federal holidays, as these are considered "weekend" days by the MRIP and we estimate fishing effort at the month and kind-of-day level
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

/*
global fed_holidays "inlist(day, td(02jan2023), td(16jan2023), td(20feb2023), td(29may2023), td(19jun2023), td(04jul2023), td(04sep2023), td(09oct2023), td(10nov2023), td(23nov2023)," ///
 "td(25dec2023), td(01jan2024), td(15jan2024), td(19feb2024), td(27may2024), td(19jun2024), td(04jul2024), td(02sep2024), td(14oct2024), td(11nov2024), td(28nov2024), td(25dec2024)," ///
" td(01jan2025), td(20jan2025), td(17feb2025), td(26may2025), td(19jun2025), td(04jul2025), td(01sep2025), td(13oct2025), td(11nov2025), td(27nov2025), td(25dec2025)," ///
" td(01jan2026), td(19jan2026), td(16feb2026), td(25may2026))"
*/

*Fed holidays in the calibration year 
global fed_holidays "inlist(day, td(10nov2023), td(23nov2023), td(25dec2023), td(01jan2024), td(15jan2024), td(19feb2024), td(27may2024), td(19jun2024), td(04jul2024), td(02sep2024), td(14oct2024), td(11nov2024), td(28nov2024), td(25dec2024))" 

*Fed holidays in the projection year 
global fed_holidays_y2 "inlist(day_y2, td(01jan2026), td(19jan2026), td(16feb2026), td(25may2026), td(19jun2026), td(03jul2026), td(07sep2026), td(12oct2026), td(11nov2026), td(26nov2026), td(25dec2026))"

*Put leap-year days here
global leap_yr_days "td(29feb2024)" 

*Choose how many draws you want to create. Will create 150 for final version, from which 100 will be selected
global ndraws 125

*Set the global length to pull either ionches or centimeters from MRIP (l_in_bin or l_cm_bin)
*global length_bin l_cm_bin

*set the year to use for historical numbers at age 
*global calibration_year_NAA 2024

*set the year to use for projected numbers at age 
*global projection_year_NAA 2025

*set years of which to pull the NEFSC trawl survey data
global NEFSC_svy_yrs "inlist(year,2024, 2023, 2022)"

*Adjustment to 2017 survey trip costs to account for inflation
*https://www.bls.gov/data/inflation_calculator.htm, January 2017 - January 2025 
global inflation_expansion=1.31 

*Adjust project paths based on user
global project_path "C:\Users\andrew.carr-harris\Desktop\Git\flukeRDM" /* Lou's project path */
*global iterative_data_path "C:\Users\andrew.carr-harris\Desktop\flukeRDM_iterative_data" 
*global project_path "C:\Users\min-yang.lee\Documents\rdmtool\lou_files\cod_haddock"  /* Min-Yang's project path */
global iterative_data_path "E:\Lou_projects\flukeRDM\flukeRDM_iterative_data"/* Lou's path for iterative catch data that is too large to upload to GitHub*/  /* Everything Kim needs to run the model on the app */

global input_data_cd "C:\Users\andrew.carr-harris\Desktop\MRIP_data_2025" /* Lou's local data path */
global input_code_cd "${project_path}\Code\pre_sim"
global iterative_input_data_cd "${iterative_data_path}"
global figure_cd  "${input_data_cd}\figures"



**************************************************Model calibration ************************************************** 
// 1) Pull the MRIP data
do "$input_code_cd\MRIP data wrapper.do"


// 2) Estimate directed trips at the month, mode, kind-of day level
do "$input_code_cd\directed_trips_calibration.do"
		*This file calls "set regulations.do". In it you must enter the SQ regulations in the calibration and projection year. 
		*THIS NEEDS TO BE ADJUSTED EVERY YEAR. 


// 3) Create distirbutions of costs per trip across strata
do "$input_code_cd\survey trip costs.do"


// 4) Estimate catch-per-trip at the month and mode level
		//a) compute mean catch-per-trip and standard error, imputing standard errors from historcial data when they are missing. 
		do "$input_code_cd\catch_per_trip_calibration_part1.do"

		//b) use copula model (in R) to simulate harvest and discards per-trip
		* run copula_model_loop.R
		
		//c) generate estimates of simulated total harvest based on random draws of catch-per-trip and directed trips
		do "$input_code_cd\catch_per_trip_calibration_part2.do"


// 5) compare calibration output to MRIP, and retain total simulated harvest and discards to apply to the baseline catch-at-length distribution
do "$input_code_cd\compare calibration output to MRIP.do" 


// 6) Generate baseline-year catch-at-length, using the simulated harvest/discard totals from step 5
do "$input_code_cd\catch_at_length.do"

// 7) Generate projection-year catch-at-length, incorporating the stock assessment data
		do "$input_code_cd\projected_catch_at_length.do"

		
// 8)  Estimate projected catch-per-trips at the month and mode level
		 *will use MRIP catch data from the last two full years. 
		 *For 2026 mgt. cycle: 2025 waves 1-4, 2024 waves 1-6, 2023 waves 5 & 6	 
		 
		//a) compute mean catch-per-trip and standard error, imputing standard errors from historcial data when they are missing. 
		 do "$input_code_cd\catch_per_trip_projection_part1.do"

		//b) use copula model (in R) to simulate harvest and discards per-trip
		* run copula_model_loop_projection.R
		
		//c) generate estimates of simulated total harvest based on random draws of catch-per-trip and directed trips
		do "$input_code_cd\catch_per_trip_projection_part2.do"
		
		//d) compare estimates of mean projected catch to MRIP data to ensure consistency and remove extraneous columns from projected catch draw data
		do "$input_code_cd\compare projection catch to MRIP.do"
		

// 9) Generate random draws of harvest and discard weight-per-fish to use when projecting SQ scenario 
		do "$input_code_cd\.do"

// Steps 7-10 are not necessary to run. They compare the disaggregated simulated catch and effort data to aggreagte MRIP estimates, and compute catch weight totals in the calibration year 


// 10) Estimate total weight of harvest and release in calibration year, compare to simulation model output
	*a)  compute harvest and discard-at-length from MRIP data (by age-weight equation season: Jan-Jun and July-Dec, and for the whole year)
	
		*do "$input_code_cd\raw_props_at_length_ab1b2_calibration.do"

	*b) compute total harvest and discards from MRIP data (by age-weight equation season: Jan-Jun and July-Dec, and for the whole year)
	
		*do "$input_code_cd\catch_totals_calibration_month1.do"
	
	*c) compute the total mortality weight in the calibration year based on MRIP data and compare the simulation model output. 
		*note that the calibration model must be run in R first, and in calibration_wrapper.R, you must uncomment:
			*1) the line #source(paste0(code_cd, "calibration_catch_weights2.R"))
			*2) the last code chunk in calibration_wrapper.R  

		*datasets needed:
			*Discard mortality rates for each species by month and size-class: $input_data_cd\Discard_Mortality.csv
			*Rates are the same for cod across month and size class, but differ for haddock
			*R code output file: calibration_catch_weights_cm.xlsx
			
		*do "$input_code_cd\compute catch weights calibration.do" - this file requires the 










