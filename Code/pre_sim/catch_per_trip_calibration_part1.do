/*******************************************************************************
 Script:       catch_per_trip_calibration_part1.do
 Purpose:      Estimates mean harvest-, discard- and catch-per-trip with
               standard errors, by state x wave x mode, for the calibration
               year - the inputs the copula step needs in order to simulate
               correlated per-trip catch. Also computes the calibration-year
               MRIP totals the simulation is later compared against.

               The hard part, and most of the length, is the standard errors.
               Some state x wave x mode cells contain only a single MRIP
               primary sampling unit, which yields a mean but no variance. The
               script imputes a standard error for those cells from related
               data - other recent years, or coarser levels of aggregation -
               so that every stratum has a usable mean and SE for the copula
               to draw from. Without this, those strata would drop out of the
               simulation entirely rather than merely being uncertain.
 Inputs:       The MRIP trip and catch files named by $triplist and $catchlist.
 Outputs:      baseline_mrip_catch_processed.xlsx (the per-stratum mean and
               standard-error file consumed by
               copula_modeling_calibration.R), plus the calibration-year MRIP
               totals at three levels of aggregation:
               mrip_catch_calib_state.dta,
               mrip_catch_calib_state_mode.dta,
               mrip_catch_calib_state_mode_wave.dta
 Dependencies: Globals $triplist, $catchlist, $misc_data_cd and
               $calibration_year. Requires MRIP_lists.do to have run. Uses the
               user-written command dsconcat.
 Pipeline:     Step 5a of model_wrapper.do, gated by the toggle catch_per_trip1.
               Feeds the R copula step (5b), whose output
               calibration_catch_per_trip_part2.do then expands into daily
               catch draws. Its projection-year analog is
               catch_per_trip_projection_part1.do, which differs mainly in
               drawing on a rolling 12 waves of MRIP data instead of one year.
*******************************************************************************/

display "catch_per_trip_calibration_part1.do: estimating mean catch-per-trip and standard errors by state x wave x mode, imputing standard errors for single-PSU strata. This may take a while."




/*This code uses the MRIP data to: 
 
	* Part A)  
		1) estimate mean harvest-, discards-, and catch-per-trip and their standard errors at the state, wave, and fishing mode level in the calibration period
		2) For some combinations of state-wave-mode, there is only a single PSU and thus no standard error available for the mean estimates. In these cases, I impute 
			a standard error based on other recent data or difference levels of aggregation. 
		3) Once a mean and standard errors have been estimated for all strata, I save the file and run the "copula_loop.R" in R. This file simulates random 
			draws of harvest and discards-per trip, accounting for possible intra-species correlation in harvest and discards
			
	* Part B)  
		1) Compute catch'harvest totals by state/mode/etc.in the caliabration year to compare with simulated caliabration-year fishery data 
	
*/
		
************** Part A  **************

* Pull in MRIP data


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


keep if $calibration_year
 
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

replace common_dom="ZZ"  if state=="NC" & !inlist(cnty, 15, 29, 41, 53, 55, 139, 143, 177, 187)

tostring wave, gen(wv2)
tostring year, gen(yr2)

gen my_dom_id_string=state+"_"+wv2+"_"+mode1+"_"+common_dom

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

*keep if common_dom=="SF"
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

tempfile base_results
save `base_results', replace

drop if mean==0
gen pse=se/mean
keep if se==.

split my, parse(_)
rename my_dom_id_string1 state
rename my_dom_id_string2 wave
rename my_dom_id_string3 mode
rename my_dom_id_string4 common_dom

gen shoulder_wave="2" if wave=="1"
replace shoulder_wave="1" if wave=="2"
replace shoulder_wave="4" if wave=="3"
replace shoulder_wave="3" if wave=="4"
replace shoulder_wave="6" if wave=="5"
replace shoulder_wave="5" if wave=="6"

gen strata_id=_n
levelsof strata_id, local(stratz)

tempfile missing_se
save `missing_se', replace 

* Round 1
global impute
foreach s of local stratz{
	u `missing_se', clear 
	keep if strata_id==`s'
	
	levelsof state, local(st) clean
	levelsof common_dom, local(common_dom2) clean
	levelsof mode, local(md) clean
	levelsof wave, local(wave1) clean
	levelsof shoulder_wave, local(wave2) clean
	levelsof varname, local(outcome) clean
	levelsof my_dom_id_string, local(my_dom_id_string) clean

	u `basefile', clear 
	keep if state=="`st'" & mode1=="`md'" & inlist(wv2, "`wave1'", "`wave2'") & common_dom=="`common_dom2'"
	drop my_dom_id_string my_dom_id
	gen my_dom_id_string="`my_dom_id_string'"
	encode my_dom_id_string, gen(my_dom_id)

	
	* Create a postfile to collect results
	tempfile results
	postfile handle2 str15 varname str15 domain float mean se ll95 ul95 using `results', replace

    * Run svy mean for the variable by domain
    svy: mean `outcome', over(my_dom_id)

    * Grab result matrix and domain labels
    matrix M = r(table)
    local colnames : colnames M

    * Loop over columns (domains)
    foreach col of local colnames {
        local m  = M[1, "`col'"]
        local se = M[2, "`col'"]
        local lb = M[5, "`col'"]
        local ub = M[6, "`col'"]

        post handle2 ("`outcome'") ("`my_dom_id_string'") (`m') (`se') (`lb') (`ub')
    }


postclose handle2

* Load results back into memory
use `results', clear

gen pse_impute=se/mean
rename domain my_dom_id_string
keep varname pse_impute my_dom_id_string

tempfile impute`s'
save `impute`s'', replace
global impute "$impute "`impute`s''" " 

}	
dsconcat $impute 

merge 1:1  varname my_dom_id_string using `missing_se'

preserve 
keep if pse_impute!=.
tempfile round1
save `round1', replace
restore

drop if pse_impute!=.

drop shoulder_wave*

gen shoulder_wave1="2" if wave=="1"
gen shoulder_wave2="3" if wave=="1"

replace shoulder_wave1="1" if wave=="2"
replace shoulder_wave2="3" if wave=="2"

replace shoulder_wave1="1" if wave=="3"
replace shoulder_wave2="2" if wave=="3"

replace shoulder_wave1="5" if wave=="4"
replace shoulder_wave2="6" if wave=="4"

replace shoulder_wave1="4" if wave=="5"
replace shoulder_wave2="6" if wave=="5"

replace shoulder_wave1="4" if wave=="6"
replace shoulder_wave2="5" if wave=="6"

levelsof strata_id, local(stratz)

drop _merge 

tempfile missing_se
save `missing_se', replace 

* Round 2
global impute2
foreach s of local stratz{
	u `missing_se', clear 
	keep if strata_id==`s'
	
	levelsof state, local(st) clean
	levelsof common_dom, local(common_dom2) clean
	levelsof mode, local(md) clean
	levelsof wave, local(wave1) clean
	levelsof shoulder_wave1, local(wave2) clean
	levelsof shoulder_wave2, local(wave3) clean
	levelsof varname, local(outcome) clean
	levelsof my_dom_id_string, local(my_dom_id_string) clean

	u `basefile', clear 
	keep if state=="`st'" & mode1=="`md'" & inlist(wv2, "`wave1'", "`wave2'", "`wave3'") & common_dom=="`common_dom2'"
	drop my_dom_id_string my_dom_id
	gen my_dom_id_string="`my_dom_id_string'"
	encode my_dom_id_string, gen(my_dom_id)

	
	* Create a postfile to collect results
	tempfile results
	postfile handle2 str15 varname str15 domain float mean se ll95 ul95 using `results', replace

    * Run svy mean for the variable by domain
    svy: mean `outcome', over(my_dom_id)

    * Grab result matrix and domain labels
    matrix M = r(table)
    local colnames : colnames M

    * Loop over columns (domains)
    foreach col of local colnames {
        local m  = M[1, "`col'"]
        local se = M[2, "`col'"]
        local lb = M[5, "`col'"]
        local ub = M[6, "`col'"]

        post handle2 ("`outcome'") ("`my_dom_id_string'") (`m') (`se') (`lb') (`ub')
    }


postclose handle2

* Load results back into memory
use `results', clear

gen pse_impute=se/mean
rename domain my_dom_id_string
keep varname pse_impute my_dom_id_string

tempfile impute2`s'
save `impute2`s'', replace
global impute2 "$impute2 "`impute2`s''" " 

}	
dsconcat $impute2

merge 1:1  varname my_dom_id_string using `missing_se'
append using `round1'
keep varname my_dom_id_string pse_impute 
merge 1:1 varname my_dom_id_string using `base_results'

replace se=mean*pse_impute if se==. & _merge==3

keep if strmatch(my_dom_id, "*SF*")==1


* If after the two rounds there is still a missing standard error for strata with positive mean catch, set se = mean (high ucertainty)
replace se=mean if mean>0 & !missing(mean) & se==.

* Stop code if non-value mean harvest/discards/catch-per trip are missing standard errors
* Check condition across the dataset
summarize if mean != 0 & missing(se)

* If any observations meet the condition, stop
if r(N) > 0 {
    display "Stopping: mean is not zero and se is missing for some observations."
    exit 1
}

gen missing_se=1 if _merge==3
drop _merge
sort my_dom_id_string var
drop pse
reshape wide mean se missing, i(my) j(varname) string

* make indicator variables for whether each domain contains keep, discards, or keep and discards of each species 
mvencode meanbsb_keep meanbsb_rel meanscup_keep meanscup_rel meansf_keep meansf_rel, mv(0) override

gen sf_only_keep=1 if meansf_keep>0 & meansf_rel==0
gen sf_only_rel=1 if meansf_rel>0 & meansf_keep==0
gen sf_keep_and_rel=1 if meansf_rel>0 & meansf_keep>0
gen sf_no_catch=1 if meansf_rel==0 & meansf_keep==0

gen bsb_only_keep=1 if meanbsb_keep>0 & meanbsb_rel==0
gen bsb_only_rel=1 if meanbsb_rel>0 & meanbsb_keep==0
gen bsb_keep_and_rel=1 if meanbsb_rel>0 & meanbsb_keep>0
gen bsb_no_catch=1 if meanbsb_rel==0 & meanbsb_keep==0

gen scup_only_keep=1 if meanscup_keep>0 & meanscup_rel==0
gen scup_only_rel=1 if meanscup_rel>0 & meanscup_keep==0
gen scup_keep_and_rel=1 if meanscup_rel>0 & meanscup_keep>0
gen scup_no_catch=1 if meanscup_rel==0 & meanscup_keep==0


mvencode sf_only_keep sf_only_rel sf_keep_and_rel sf_no_catch bsb_only_keep bsb_only_rel bsb_keep_and_rel bsb_no_catch scup_only_keep scup_only_rel scup_keep_and_rel scup_no_catch, mv(0) override

merge 1:m my_dom_id_string using `basefile'
keep if strmatch(my_dom_id_string, "*SF*")==1

*condition for when keep and release are both positive for a stratum, but they never occur on the same trip
*Will model these distributions as independent
gen tab=1 if sf_keep>0 & sf_keep!=. & sf_rel>0 & sf_rel!=.
egen sumtab=sum(tab), by(my_dom_id_string)
gen sf_keep_and_rel_ind=1 if sf_keep_and_rel==1 & sumtab==0
replace sf_keep_and_rel=0 if sf_keep_and_rel_ind==1
drop tab sumtab

gen tab=1 if bsb_keep>0 & bsb_keep!=. & bsb_rel>0 & bsb_rel!=.
egen sumtab=sum(tab), by(my_dom_id_string)
gen bsb_keep_and_rel_ind=1 if bsb_keep_and_rel==1 & sumtab==0
replace bsb_keep_and_rel=0 if bsb_keep_and_rel_ind==1
drop tab sumtab

gen tab=1 if scup_keep>0 & scup_keep!=. & scup_rel>0 & scup_rel!=.
egen sumtab=sum(tab), by(my_dom_id_string)
gen scup_keep_and_rel_ind=1 if scup_keep_and_rel==1 & sumtab==0
replace scup_keep_and_rel=0 if scup_keep_and_rel_ind==1
drop tab sumtab

*condition for when keep and release are both positive for a stratum, but occured together on only one trip so that the correlation==1.
*Will model these distributions as independent
*sf
gen perfect_corr=.
levelsof my_dom_id_string if sf_keep_and_rel==1, local(doms)
foreach d of local doms{
di "`d'" 
egen rank_keep = rank(sf_keep) if my_dom_id_string=="`d'" 
egen rank_rel  = rank(sf_rel) if my_dom_id_string=="`d'" 
count if  my_dom_id_string=="`d'" 
if `r(N)'>1{
corr rank_keep rank_rel if my_dom_id_string=="`d'"  [aw=wp_int]
if `r(rho)'==1 | `r(rho)'==. {
	replace perfect_corr=1 if my_dom_id_string=="`d'"
}
}
	drop rank*

}

replace sf_keep_and_rel=0 if sf_keep_and_rel==1 & perfect_corr==1
replace sf_keep_and_rel_ind=1 if perfect_corr==1

drop perfect_corr

*bsb
gen perfect_corr=.
levelsof my_dom_id_string if bsb_keep_and_rel==1, local(doms)
foreach d of local doms{
di "`d'" 
egen rank_keep = rank(bsb_keep) if my_dom_id_string=="`d'" 
egen rank_rel  = rank(bsb_rel) if my_dom_id_string=="`d'" 
count if  my_dom_id_string=="`d'" 
if `r(N)'>1{
corr rank_keep rank_rel if my_dom_id_string=="`d'"   [aw=wp_int]
if `r(rho)'==1 | `r(rho)'==. {
	replace perfect_corr=1 if my_dom_id_string=="`d'" 
}
}
	drop rank*

}

replace bsb_keep_and_rel=0 if bsb_keep_and_rel==1 & perfect_corr==1
replace bsb_keep_and_rel_ind=1 if perfect_corr==1
drop perfect_corr

*scup
gen perfect_corr=.
levelsof my_dom_id_string if scup_keep_and_rel==1, local(doms)
foreach d of local doms{
di "`d'" 
egen rank_keep = rank(scup_keep) if my_dom_id_string=="`d'" 
egen rank_rel  = rank(scup_rel) if my_dom_id_string=="`d'" 
count if  my_dom_id_string=="`d'" 
if `r(N)'>1{
corr rank_keep rank_rel if my_dom_id_string=="`d'"  [aw=wp_int]
if `r(rho)'==1 | `r(rho)'==. {
	replace perfect_corr=1 if my_dom_id_string=="`d'"
}
}
	drop rank*

}

replace scup_keep_and_rel=0 if scup_keep_and_rel==1 & perfect_corr==1
replace scup_keep_and_rel_ind=1 if perfect_corr==1
drop perfect_corr

keep wp_int my_dom_id_string meanbsb_cat-id_code year yr2 st  state common_dom sf_cat-scup_rel my_dom_id sf_only_keep sf_only_rel sf_keep_and_rel sf_no_catch bsb_only_keep bsb_only_rel bsb_keep_and_rel bsb_no_catch scup_only_keep scup_only_rel scup_keep_and_rel scup_no_catch bsb_keep_and_rel_ind sf_keep_and_rel_ind scup_keep_and_rel_ind

mvencode se*, mv(0) override
mvencode missing*, mv(0) override

export excel "$misc_data_cd\baseline_mrip_catch_processed.xlsx", firstrow(variables) replace


************** Part B  **************
* Compute MRIP estimates for comparison with simulated estimates 

* Estimates by state and mode
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

* ensure only relevant states/year
keep if inlist(st, 25, 44, 9,  36 , 34, 10, 24, 51, 37)
keep if $calibration_year
 
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

// classify trips that I care about into the things I care about (caught or targeted sf/bsb) and things I don't care about "ZZ" 
replace prim1_common=subinstr(lower(prim1_common)," ","",.)
replace prim2_common=subinstr(lower(prim1_common)," ","",.)

/* we need to retain 1 observation for each strat_id, psu_id, and id_code.  */
/* A.  Trip (Targeted or Caught) (fluke, sea bass, or scup) then it should be marked in the domain "SF"
   B.  Trip did not (Target or Caught) (fluke, sea bass, or scup) then it is marked in the the domain "ZZ"
*/

gen common_dom="ZZ"
replace common_dom="SF" if inlist(common, "summerflounder") 
replace common_dom="SF" if inlist(common, "blackseabass") 
replace common_dom="SF" if inlist(common, "scup") 

replace common_dom="SF"  if inlist(prim1_common, "summerflounder") 
replace common_dom="SF"  if inlist(prim1_common, "blackseabass") 
replace common_dom="SF"  if inlist(prim1_common, "scup") 

* keep only NC north based on county delineation from Tracey 
replace common_dom="ZZ"  if state=="NC" & !inlist(cnty, 15, 29, 41, 53, 55, 139, 143, 177, 187)

tostring wave, gen(wv2)
tostring year, gen(yr2)

gen my_dom_id_string=state+"_"+mode1+"_"+common_dom

* For each trip compute total catch/harvest/discards of the three species
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


* Set a variable "no_dup"=0 if the record is "$my_common" catch and no_dup=1 otherwise.
gen no_dup=0
replace no_dup=1 if  strmatch(common, "summerflounder")==0
replace no_dup=1 if strmatch(common, "blackseabass")==0
replace no_dup=1 if strmatch(common, "scup")==0

/*
We sort on year, strat_id, psu_id, id_code, "no_dup", and "my_dom_id_string". After sorting, we generate a count variable (count_obs1 from 1....n). We keep only the "first" observations within each "year, strat_id, psu_id, and id_codes" group because we have already computed total catch per trip of the species of interest above and saved these in a single trip-row
*/
bysort year strat_id psu_id id_code (my_dom_id_string no_dup): gen count_obs1=_n
keep if count_obs1==1 

order strat_id psu_id id_code no_dup my_dom_id_string count_obs1 common
svyset psu_id [pweight= wp_int], strata(strat_id) singleunit(certainty)

drop if wp_int==0
encode my_dom_id_string, gen(my_dom_id)

preserve
keep my_dom_id my_dom_id_string
duplicates drop 
tempfile domains
save `domains', replace 
restore

gen my_dom_id_string2=state+"_"+mode1+"_"+common_dom
encode my_dom_id_string2, gen(my_dom_id2)

* Create a postfile to collect results
tempfile results1
postfile handle str15 varname str15 domain float total se ll ul using `results1', replace

* Loop over variables
foreach var in sf_keep sf_rel sf_cat bsb_keep bsb_rel bsb_cat scup_keep scup_rel scup_cat {

    * Run svy mean for the variable by domain
    svy: total `var', over(my_dom_id2)

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
use `results1', clear

split domain, parse("@")
drop domain1
split domain2, parse(.)
split domain21, parse(b)
drop domain2 domain21 domain22 domain212
destring domain211, replace
rename domain211 my_dom_id
merge m:1 my_dom_id using `domains' 
sort varname  my_dom_id
keep varname total se ll ul my_dom_id_string

reshape wide total se ll ul, i(my_dom) j(varname) string

ds my_dom_id_string, not
renvarlab `r(varlist)', postfix(_mrip)

split my_dom, parse(_)
rename my_dom_id_string1 state
rename my_dom_id_string2 mode
rename my_dom_id_string3 common_dom
keep if common_dom=="SF"
drop common_dom
order my_dom_id_string state mode

save "$misc_data_cd\mrip_catch_calib_state_mode.dta", replace 



* Estimates by state
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

* ensure only relevant states/years
keep if inlist(st, 25, 44, 9,  36 , 34, 10, 24, 51, 37)
keep if $calibration_year
 
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

// classify trips that I care about into the things I care about (caught or targeted sf/bsb) and things I don't care about "ZZ" 
replace prim1_common=subinstr(lower(prim1_common)," ","",.)
replace prim2_common=subinstr(lower(prim1_common)," ","",.)

/* we need to retain 1 observation for each strat_id, psu_id, and id_code.  */
/* A.  Trip (Targeted or Caught) (fluke, sea bass, or scup) then it should be marked in the domain "SF"
   B.  Trip did not (Target or Caught) (fluke, sea bass, or scup) then it is marked in the the domain "ZZ"
*/

gen common_dom="ZZ"
replace common_dom="SF" if inlist(common, "summerflounder") 
replace common_dom="SF" if inlist(common, "blackseabass") 
replace common_dom="SF" if inlist(common, "scup") 

replace common_dom="SF"  if inlist(prim1_common, "summerflounder") 
replace common_dom="SF"  if inlist(prim1_common, "blackseabass") 
replace common_dom="SF"  if inlist(prim1_common, "scup") 

* keep only NC north based on county delineation from Tracey 
replace common_dom="ZZ"  if state=="NC" & !inlist(cnty, 15, 29, 41, 53, 55, 139, 143, 177, 187)

tostring wave, gen(wv2)
tostring year, gen(yr2)

gen my_dom_id_string=state+"_"+common_dom

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

* Set a variable "no_dup"=0 if the record is "$my_common" catch and no_dup=1 otherwise.
  
gen no_dup=0
replace no_dup=1 if  strmatch(common, "summerflounder")==0
replace no_dup=1 if strmatch(common, "blackseabass")==0
replace no_dup=1 if strmatch(common, "scup")==0

/*
We sort on year, strat_id, psu_id, id_code, "no_dup", and "my_dom_id_string". After sorting, we generate a count variable (count_obs1 from 1....n). We keep only the "first" observations within each "year, strat_id, psu_id, and id_codes" group because we have already computed total catch per trip of the species of interest above and saved these in a single trip-row
*/

bysort year strat_id psu_id id_code (my_dom_id_string no_dup): gen count_obs1=_n
keep if count_obs1==1 

order strat_id psu_id id_code no_dup my_dom_id_string count_obs1 common

svyset psu_id [pweight= wp_int], strata(strat_id) singleunit(certainty)

drop if wp_int==0
encode my_dom_id_string, gen(my_dom_id)

preserve
keep my_dom_id my_dom_id_string
duplicates drop 
tempfile domains
save `domains', replace 
restore

gen my_dom_id_string2=state+"_"+common_dom
encode my_dom_id_string2, gen(my_dom_id2)

* Create a postfile to collect results
tempfile results1
postfile handle str15 varname str15 domain float total se ll ul using `results1', replace

* Loop over variables
foreach var in sf_keep sf_rel sf_cat bsb_keep bsb_rel bsb_cat scup_keep scup_rel scup_cat {

    * Run svy mean for the variable by domain
    svy: total `var', over(my_dom_id2)

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
use `results1', clear

split domain, parse("@")
drop domain1
split domain2, parse(.)
split domain21, parse(b)
drop domain2 domain21 domain22 domain212
destring domain211, replace
rename domain211 my_dom_id
merge m:1 my_dom_id using `domains' 
sort varname  my_dom_id
keep varname total se ll ul my_dom_id_string

reshape wide total se ll ul, i(my_dom) j(varname) string

ds my_dom_id_string, not
renvarlab `r(varlist)', postfix(_mrip)

split my_dom, parse(_)
rename my_dom_id_string1 state
rename my_dom_id_string2 common_dom
keep if common=="SF"
drop  common 
order my_dom_id_string state 

save "$misc_data_cd\mrip_catch_calib_state.dta", replace 


* Estimates by state, mode, wave
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

* ensure only relevant states/years
keep if inlist(st, 25, 44, 9,  36 , 34, 10, 24, 51, 37)
keep if $calibration_year
 
gen st2 = string(st,"%02.0f")
tostring wave, gen(w2)

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

// classify trips that I care about into the things I care about (caught or targeted sf/bsb) and things I don't care about "ZZ" 
replace prim1_common=subinstr(lower(prim1_common)," ","",.)
replace prim2_common=subinstr(lower(prim1_common)," ","",.)

/* we need to retain 1 observation for each strat_id, psu_id, and id_code.  */
/* A.  Trip (Targeted or Caught) (fluke, sea bass, or scup) then it should be marked in the domain "SF"
   B.  Trip did not (Target or Caught) (fluke, sea bass, or scup) then it is marked in the the domain "ZZ"
*/

gen common_dom="ZZ"
replace common_dom="SF" if inlist(common, "summerflounder") 
replace common_dom="SF" if inlist(common, "blackseabass") 
replace common_dom="SF" if inlist(common, "scup") 

replace common_dom="SF"  if inlist(prim1_common, "summerflounder") 
replace common_dom="SF"  if inlist(prim1_common, "blackseabass") 
replace common_dom="SF"  if inlist(prim1_common, "scup") 

* keep only NC north based on county delineation from Tracey 
replace common_dom="ZZ"  if state=="NC" & !inlist(cnty, 15, 29, 41, 53, 55, 139, 143, 177, 187)

tostring year, gen(yr2)

gen my_dom_id_string=state+"_"+mode1+"_"+w2+"_"+common_dom

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

* Set a variable "no_dup"=0 if the record is "$my_common" catch and no_dup=1 otherwise.
  
gen no_dup=0
replace no_dup=1 if  strmatch(common, "summerflounder")==0
replace no_dup=1 if strmatch(common, "blackseabass")==0
replace no_dup=1 if strmatch(common, "scup")==0

/*
We sort on year, strat_id, psu_id, id_code, "no_dup", and "my_dom_id_string". After sorting, we generate a count variable (count_obs1 from 1....n). We keep only the "first" observations within each "year, strat_id, psu_id, and id_codes" group because we have already computed total catch per trip of the species of interest above and saved these in a single trip-row
*/

bysort year strat_id psu_id id_code (my_dom_id_string no_dup): gen count_obs1=_n
keep if count_obs1==1 

order strat_id psu_id id_code no_dup my_dom_id_string count_obs1 common

svyset psu_id [pweight= wp_int], strata(strat_id) singleunit(certainty)

drop if wp_int==0
encode my_dom_id_string, gen(my_dom_id)

preserve
keep my_dom_id my_dom_id_string
duplicates drop 
tempfile domains
save `domains', replace 
restore

gen my_dom_id_string2=state+"_"+mode1+"_"+w2+"_"+common_dom
encode my_dom_id_string2, gen(my_dom_id2)

* Create a postfile to collect results
tempfile results1
postfile handle str15 varname str15 domain float total se ll ul using `results1', replace

* Loop over variables
foreach var in sf_keep sf_rel sf_cat bsb_keep bsb_rel bsb_cat scup_keep scup_rel scup_cat {

    * Run svy mean for the variable by domain
    svy: total `var', over(my_dom_id2)

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
use `results1', clear

split domain, parse("@")
drop domain1
split domain2, parse(.)
split domain21, parse(b)
drop domain2 domain21 domain22 domain212
destring domain211, replace
rename domain211 my_dom_id
merge m:1 my_dom_id using `domains' 
sort varname  my_dom_id
keep varname total se ll ul my_dom_id_string

reshape wide total se ll ul, i(my_dom) j(varname) string

ds my_dom_id_string, not
renvarlab `r(varlist)', postfix(_mrip)

split my_dom, parse(_)
rename my_dom_id_string1 state
rename my_dom_id_string2 mode
rename my_dom_id_string3 wave
rename my_dom_id_string4 common_dom
keep if common=="SF"
drop  common 
order my_dom_id_string state mode wave

save "$misc_data_cd\mrip_catch_calib_state_mode_wave.dta", replace 
