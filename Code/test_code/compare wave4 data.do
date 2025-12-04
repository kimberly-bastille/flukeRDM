

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
global yr_wvs 20221 20222 20223 20224 20225 20226 ///
					 20231 20232 20233 20234 20235 20236  ///
					 20241 20242 20243 20244 20245  20246 ///
					 20251 20252 20253 20254
					 
global yearlist 2022 2023 2024 2025
global wavelist 1 2 3 4 5 6


global calibration_year "(year==2024 & inlist(wave, 1, 2, 3, 4, 5, 6))"
global calibration_year_num 2024

global projection_year_old0 "(year==2025 & inlist(wave, 1, 2, 3)) | (year==2024) | (year==2023 & inlist(wave, 4, 5, 6))" //ADJUST THIS AFTER MRIP DATA RELEASE
global projection_year_old "(year==2025 & inlist(wave, 1, 2, 3)) | (year==2024) | (year==2022 & inlist(wave, 4, 5, 6))" //ADJUST THIS AFTER MRIP DATA RELEASE
global projection_year_new "(year==2025 & inlist(wave, 1, 2, 3, 4)) | (year==2024) | (year==2023 & inlist(wave, 5, 6))" //ADJUST THIS AFTER MRIP DATA RELEASE

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



* Pull in MRIP data

cd $input_data_cd

clear
mata: mata clear

tempfile tl1 cl1
dsconcat $triplist

sort year strat_id psu_id id_code
drop if strmatch(id_code, "*xx*")==1
duplicates drop 
save `tl1'
clear

dsconcat $catchlist
sort year strat_id psu_id id_code
replace common=subinstr(lower(common)," ","",.)
save `cl1'

replace var_id=strat_id if strmatch(var_id,"")

use `tl1'
merge 1:m year strat_id psu_id id_code using `cl1', keep(1 3) nogenerate /*Keep all trips including catch==0*/
replace var_id=strat_id if strmatch(var_id,"")


* Format MRIP data for estimation 

* Ensure only relevant states 
keep if inlist(st, 25, 44, 9,  36 , 34, 10, 24, 51, 37)

keep if $projection_year_new

gen st2 = string(st,"%02.0f")

gen state="MA" if st==25
replace state="MD" if st==24
replace state="RI" if st==44
replace state="CT" if st==9
replace state="NY" if st==36
replace state="NJ" if st==34
replace state="DE" if st==10
replace state="VA" if st==51
replace state="NC" if st==37

gen mode1="sh" if inlist(mode_fx, "1", "2", "3")
replace mode1="pr" if inlist(mode_fx, "7")
replace mode1="fh" if inlist(mode_fx, "4", "5")

* classify trips that I care about into the things I care about (caught or targeted sf/bsb) and things I don't care about "ZZ" 
replace prim1_common=subinstr(lower(prim1_common)," ","",.)
replace prim2_common=subinstr(lower(prim1_common)," ","",.)

* We need to retain 1 observation for each strat_id, psu_id, and id_code
/* A.  Trip (Targeted or Caught) (fluke, sea bass, or scup) then it should be marked in the domain "_ATLCO"
   B.  Trip did not (Target or Caught) (fluke, sea bass, or scup) then it is marked in the the domain "ZZZZZ"
*/

gen common_dom="ZZ"
replace common_dom="SF" if inlist(common, "summerflounder") 
replace common_dom="SF" if inlist(common, "blackseabass") 
replace common_dom="SF" if inlist(common, "scup") 

replace common_dom="SF"  if inlist(prim1_common, "summerflounder") 
replace common_dom="SF"  if inlist(prim1_common, "blackseabass") 
replace common_dom="SF"  if inlist(prim1_common, "scup") 

tostring wave, gen(wv2)
tostring year, gen(yr2)

gen my_dom_id_string=wv2+"_"+common_dom
*gen my_dom_id_string=common_dom

gen sf_tot_cat=tot_cat if common=="summerflounder"
egen sum_sf_tot_cat=sum(sf_tot_cat), by(strat_id psu_id id_code)

gen sf_harvest=landing if common=="summerflounder"
egen sum_sf_harvest=sum(sf_harvest), by(strat_id psu_id id_code)
 
gen sf_releases=release if common=="summerflounder"
egen sum_sf_releases=sum(sf_releases), by(strat_id psu_id id_code)
 
gen bsb_tot_cat=tot_cat if common=="blackseabass"
egen sum_bsb_tot_cat=sum(bsb_tot_cat), by(strat_id psu_id id_code)

gen bsb_harvest=landing if common=="blackseabass"
egen sum_bsb_harvest=sum(bsb_harvest), by(strat_id psu_id id_code)

gen bsb_releases=release if common=="blackseabass"
egen sum_bsb_releases=sum(bsb_releases), by(strat_id psu_id id_code)

gen scup_tot_cat=tot_cat if common=="scup"
egen sum_scup_tot_cat=sum(scup_tot_cat), by(strat_id psu_id id_code)

gen scup_harvest=landing if common=="scup"
egen sum_scup_harvest=sum(scup_harvest), by(strat_id psu_id id_code)

gen scup_releases=release if common=="scup"
egen sum_scup_releases=sum(scup_releases), by(strat_id psu_id id_code)

drop sf_tot_cat sf_harvest sf_releases bsb_tot_cat bsb_harvest bsb_releases  scup_tot_cat scup_harvest scup_releases
rename sum_sf_tot_cat sf_cat
rename sum_sf_harvest sf_keep
rename sum_sf_releases sf_rel
rename sum_bsb_tot_cat bsb_cat
rename sum_bsb_harvest bsb_keep
rename sum_bsb_releases bsb_rel
rename sum_scup_tot_cat scup_cat
rename sum_scup_harvest scup_keep
rename sum_scup_releases scup_rel

* Set a variable "no_dup"=0 if the record is "$my_common" catch and no_dup=1 otherwise
  
gen no_dup=0
replace no_dup=1 if  strmatch(common, "summerflounder")==0
replace no_dup=1 if strmatch(common, "blackseabass")==0
replace no_dup=1 if strmatch(common, "scup")==0

/*
We sort on year, strat_id, psu_id, id_code, "no_dup", and "my_dom_id_string". For records with duplicate year, strat_id, psu_id, and id_codes, the first entry will be "my_common catch" if it exists.  These will all be have sp_dom "SF."  If there is no my_common catch, but the trip targeted (fluke, sea bass, or scup) or caught either species, the secondary sorting on "my_dom_id_string" ensures the trip is properly classified.

After sorting, we generate a count variable (count_obs1 from 1....n) and we keep only the "first" observations within each "year, strat_id, psu_id, and id_codes" group.
*/

bysort year strat_id psu_id id_code (my_dom_id_string no_dup): gen count_obs1=_n

keep if count_obs1==1 // This keeps only one record for trips with catch of multiple species. We have already computed catch of the species of interest above and saved these in a trip-row

order strat_id psu_id id_code no_dup my_dom_id_string count_obs1 common

svyset psu_id [pweight= wp_int], strata(strat_id) singleunit(certainty)

/*
local vars sf_catch sf_keep sf_rel bsb_catch bsb_keep bsb_rel  scup_catch scup_keep scup_rel
foreach v of local vars{
	replace `v'=round(`v')
}
*/

keep if common_dom=="SF"
drop if wp_int==0
encode my_dom_id_string, gen(my_dom_id)

preserve
keep my_dom_id my_dom_id_string
duplicates drop 
tempfile domains
save `domains', replace 
restore

tempfile basefile
save `basefile', replace 


* Here I will estimate mean catch/harvest/discards per trip for each strata in order to identify strata with missing SE
* For strata with missing SE's, I'll follow similar approch to MRIP's hot and cold deck imputation for observations with missing lengths and weights

/* From the MRIP data handbook:

"For intercepted angler trips with landings where both length and weight measurements are missing, paired length and weight observations are imputed from complete cases using hot and cold deck imputation. (Complete cases include records with both length and weight data available, as well as records where we were able to compute a missing length or weight using the length-weight modeling described above.) Up to five rounds of imputation are conducted in an attempt to fill in missing values. These rounds begin with imputation cells that correspond to the most detailed MRIP estimation cells, but are aggregated to higher levels in subsequent rounds to bring in more length-weight data. 
	- Round 1: Current year, two-month sampling wave, sub-region, state, mode, area fished, species. 
	- Round 2: Current year, half-year, sub-region, state, mode, species. 
	- Round 3: Current + most recent prior year, two-month sampling wave, sub-region, state, mode, area fished, species. 
	- Round 4: Current + most recent prior year, sub-region, state, mode, species. 
	- Round 5: Current + most recent prior year, sub-region, species."
	

* The calibration estimation strata is: current year + wave + state + mode, for harvest/discards/catch per trip

* For strata with missing, I'll impute a PSE from other strata and apply it to the missing-SE strata. 
	- Round 1: current year + TWO WAVE PERIOD + state + mode
	- Round 2: current year + HALF YEAR PERIOD + state + mode
 */

* Create a postfile to collect results
tempfile results
postfile handle str15 varname str15 domain float mean se ll95 ul95 using `results', replace

* Loop over variables
foreach var in sf_keep sf_rel sf_cat bsb_keep bsb_rel bsb_cat scup_keep scup_rel scup_cat {

    * Run svy mean for the variable by domain
    svy: mean `var', over(my_dom_id)

    * Grab result matrix and domain labels
    matrix M = r(table)
    local colnames : colnames M

    * Loop over columns (domains)
    foreach col of local colnames {
        local m  = M[1, "`col'"]
        local se = M[2, "`col'"]
        local lb = M[5, "`col'"]
        local ub = M[6, "`col'"]

        post handle ("`var'") ("`col'") (`m') (`se') (`lb') (`ub')
    }
}

postclose handle

* Load results back into memory
use `results', clear
/*
split domain, parse("@")
split domain1, parse(.)
split varname, parse("_")
rename varname2 disp
rename varname1 species
keep if disp=="cat"
keep mean se spe disp
order spec disp  mean
*/

split domain, parse("@")

drop domain1
split domain2, parse(.)
split domain21, parse(b)

drop domain2 domain21 domain22 domain212
destring domain211, replace
rename domain211 my_dom_id
merge m:1 my_dom_id using `domains' 
sort varname  my_dom_id
keep varname mean se my_dom_id_string

split varname, parse("_")
rename varname2 disp
rename varname1 species
drop varname
split my, parse(_)
rename my_dom_id_string1 wave
drop my_dom_id_string2
keep if disp=="cat"
drop my
order spec disp wave mean


preserve
u  "C:\Users\andrew.carr-harris\Desktop\MRIP_data_2025\directed_trip_calib_mrip_state_wave_total.dta", clear 
collapse (sum) dtrip, by(wave)
tempfile dtrip 
save `dtrip', replace
restore

merge m:1 wave using `dtrip'
gen tot_catch=dtrip*mean
format tot %12.0gc
order spec disp wave tot_catch

sort spec disp wave 

