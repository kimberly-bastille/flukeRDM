/******************************************************************************
 ARCHIVED - NOT PRODUCTION CODE. See Code/archive/README.md.
 
 Script:      weight_per_fish.do
 Purpose:     Estimates average weight per harvested and per discarded fish from
              MRIP data. The input that 'compute SQ weights from averages.R'
              consumed.
 Superseded by: the length-weight conversion applied in the projection stage
 
 This file is retained for reference and is not called by any wrapper,
 script or app in this repository. It is NOT maintained: paths, data
 formats and modeling choices in it may be years out of date, and it
 should not be used to understand how the pipeline currently behaves.
 Per the documentation session's scope, archived files received a header
 only - no inline documentation, and no code was changed.
******************************************************************************/




/*This code uses the MRIP data to estimate average weight per fish of harvest and discards 
 
 *harvest weights come from MRIP 
 *discard weights come from management track assessments (sf, scup) and personal communication with Emily Liljestrand (bsb)
 
*/
		

		
* Compute total harvest weights
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

keep if $sq_weight_per_fish_years
 
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

tostring wave, gen(wv2)
tostring year, gen(yr2)

tempfile new
save `new', replace

global wt
local species "summerflounder blackseabass scup"
foreach s of local species{
u `new', clear 

gen common_dom="ZZ"
replace common_dom="SF" if inlist(common, "`s'") 
gen my_dom_id_string=state+"_"+mode1+"_"+common_dom

svyset psu_id [pweight= wp_int], strata(strat_id) singleunit(certainty)

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

* Create a postfile to collect results
tempfile results
postfile handle str15 varname str15 domain float mean se ll95 ul95 using `results', replace

* Loop over variables
foreach var in wgt_ab1  {

    * Run svy mean for the variable by domain
    svy: total `var', over(my_dom_id)

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
keep varname mean se my_dom_id_string _merge
gen species="`s'"

tempfile wt`s'
save `wt`s'', replace
global wt "$wt "`wt`s''" " 

}	
clear 
dsconcat $wt
drop _merge
drop varname 
split my, parse(_)
rename my_dom_id_string1 state
rename my_dom_id_string2 mode
drop my_dom_id_string3
drop my
order state mode species mean se
replace species="sf" if species=="summerflounder"
replace species="bsb" if species=="blackseabass"
rename mean tot_wt_harv
sort species state mode

tempfile harv
save `harv', replace



* Compute total ahrvest numbers by state, mode for the time frame above
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

* ensure only relevant states/years
keep if inlist(st, 25, 44, 9,  36 , 34, 10, 24, 51, 37)
keep if $sq_weight_per_fish_years
 
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

tostring year, gen(yr2)

gen my_dom_id_string=state+"_"+mode1+"_"+common_dom

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

gen my_dom_id_string2=state+"_"+mode1
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
drop  my_dom_id_string3 
order my_dom_id_string state mode 

keep state mode total*
collapse (sum) total* , by(state mode)
reshape long total, i(state mode) j(new) string
split new, parse(_)

drop new3
rename new1 species
rename new2 disp

keep if disp=="keep"
rename total total_harv_n
drop  new disp

merge 1:1 state mode species using  `harv'
drop if tot_wt_harv==0
drop if total_harv_n==0
drop _merge se

replace  tot_wt_harv=tot_wt_harv*2.20462 //translate to pounds
gen avg_wt_harv=tot_wt_harv/total_harv_n

tempfile harv
save `harv', replace

* compute discard weights from the mgt track assessments
* discard mortality 10% for summer flounder, 15% for black sea bass and scup.

* summer flounder 
import excel using "$input_data_cd\length_data\sf_mt_rec_disc.xlsx", clear first // no data for VA and NC
renvarlab, lower
keep if year==2024
rename st_f state
gen total_mt= dead_b2_mt/.1
collapse (sum) total_mt, by(state)
tempfile one
save `one', replace 

use "$input_data_cd\catch_total_calib_mrip_state_total.dta", clear 
keep state total*
reshape long total, i(state) j(new) string
split new, parse(_)
drop new3
rename new1 species
rename total total_rel
keep if new2=="rel"
keep if species=="sf"
drop species new new2
drop if total==0
merge 1:1 state using `one'
drop if _merge==1
gen total_lbs=total_mt*2204.62
gen avg_wt_rel=total_lbs/total_rel

*use neighboring state values if there is missing values for a state 
*replace VA and NC with MD values
expand 2 if state=="MD", gen(dup)
replace state="VA" if dup==1
drop _merge dup

expand 2 if state=="MD", gen(dup)
replace state="NC" if dup==1

*expand across fishing modes
drop dup
expand 3
bysort state: gen tab=_n
gen mode="sh" if tab==1
replace mode="fh" if tab==2
replace mode="pr" if tab==3

keep state mode avg_wt_rel 
gen species="sf"

tempfile sf_disc
save `sf_disc', replace 

/*
merge 1:m state species using `harv'
drop _merge

save `harv', replace
*/
* scup
import excel using "$input_data_cd\length_data\scup_mt_rec_disc.xlsx", clear first //data by year and semester, I will combine semesters
renvarlab, lower
keep if year==2024
gen total_mt= w_mt/.15
collapse (sum) total_mt
gen tab=1
tempfile two
save `two', replace 

use "$input_data_cd\catch_total_calib_mrip_state_total.dta", clear 
keep state total*
reshape long total, i(state) j(new) string
split new, parse(_)
drop new3
rename new1 species
keep if new2=="rel"
rename total total_rel
keep if species=="scup"
drop species new new2
drop if total==0
collapse (sum) total_rel
gen tab=1

merge 1:1 tab using `two'
drop if _merge==1
gen total_lbs=total_mt*2204.62
gen avg_wt_rel=total_lbs/total_rel
gen species="scup"
keep species avg_wt_rel 

expand 9
gen tab=_n
gen state="MA" if tab==1
replace state="RI" if tab==2
replace state="CT" if tab==3
replace state="NY" if tab==4
replace state="NJ" if tab==5
replace state="DE" if tab==6
replace state="MD" if tab==7
replace state="VA" if tab==8
replace state="NC" if tab==9

drop tab
expand 3
bysort state: gen tab=_n
gen mode="sh" if tab==1
replace mode="fh" if tab==2
replace mode="pr" if tab==3
drop tab

tempfile scup_disc
save `scup_disc', replace 

/*
merge 1:m  species using `harv'
sort species state 
drop se
drop _merge
*/

*bsb
import excel using "$input_data_cd\length_data\bsb_mt_rec_disc.xlsx", clear first //data by North and South 
renvarlab, lower
keep if year==2024

gen total_mt= recdiscmt/.15
collapse (sum) total_mt, by(egion)
rename egion region

tempfile three
save `three', replace 

use "$input_data_cd\catch_total_calib_mrip_state_total.dta", clear 
keep state total*
reshape long total, i(state) j(new) string
split new, parse(_)
drop new3
rename new1 species
rename total total_rel
keep if new2=="rel"
keep if species=="bsb"

gen region="North" if inlist(state, "MA", "RI", "CT", "NY")
replace region="South" if !inlist(state, "MA", "RI", "CT", "NY")
drop species new new2
drop if total==0
collapse (sum) total_rel, by(region)

merge 1:1  region using `three'
gen total_lbs=total_mt*2204.62
gen avg_wt_rel=total_lbs/total_rel
gen species="bsb"
keep species region avg_wt_rel 

expand 4 if region=="North"
bysort region: gen tab=_n
gen state="MA" if tab==1 & region=="North"
replace state="RI" if tab==2 & region=="North"
replace state="CT" if tab==3 & region=="North"
replace state="NY" if tab==4 & region=="North"
drop tab

expand 5 if region=="South"
bysort region: gen tab=_n
replace state="NJ" if tab==1 & region=="South"
replace state="DE" if tab==2 & region=="South"
replace state="MD" if tab==3 & region=="South"
replace state="VA" if tab==4 & region=="South"
replace state="NC" if tab==5 & region=="South"
drop tab

expand 3
bysort state: gen tab=_n
gen mode="sh" if tab==1
replace mode="fh" if tab==2
replace mode="pr" if tab==3
drop tab

append using `scup_disc'
append using `sf_disc'

drop region  
order state mode species

merge 1:1 state mode species using `harv'

drop total_harv_n tot_wt_harv
sort species state mode
drop _merge
*Now fill in missing values using neighboring states 
export excel using  "$input_data_cd\weight_per_catch_missing.xlsx", firstrow(variables) replace 


* Define state order
gen st_order= 1 if state=="MA"
replace st_order=2 if state=="RI"
replace st_order=3 if state=="CT" 
replace st_order=4 if state=="NY"
replace st_order=5 if state=="NJ"
replace st_order=6 if state=="DE" 
replace st_order=7 if state=="MD" 
replace st_order=8 if state=="VA"
replace st_order=9 if state=="NC"


* Sort by species, mode, and state order
sort species mode st_order

* For each species/mode/state combo, carry neighbor values
bysort species mode (st_order): gen harv_prev = avg_wt_harv[_n-1]
bysort species mode (st_order): gen harv_next = avg_wt_harv[_n+1]

* If missing, replace with neighbor averages
gen avg_wt_harv_filled = avg_wt_harv
replace avg_wt_harv_filled = (harv_prev + harv_next)/2 if missing(avg_wt_harv) & !missing(harv_prev) & !missing(harv_next)
replace avg_wt_harv_filled = harv_prev if missing(avg_wt_harv_filled) & !missing(harv_prev)
replace avg_wt_harv_filled = harv_next if missing(avg_wt_harv_filled) & !missing(harv_next)

drop harv_prev harv_next

* Loop over distance outward from 1 to 8 states
forvalues d = 1/8 {
    * Look backward (up-coast)
    bysort species mode (st_order): ///
        replace avg_wt_harv_filled = avg_wt_harv_filled[_n-`d'] ///
        if missing(avg_wt_harv_filled) & !missing(avg_wt_harv_filled[_n-`d'])

    * Look forward (down-coast)
    bysort species mode (st_order): ///
        replace avg_wt_harv_filled = avg_wt_harv_filled[_n+`d'] ///
        if missing(avg_wt_harv_filled) & !missing(avg_wt_harv_filled[_n+`d'])
}

replace avg_wt_harv=avg_wt_harv_filled
drop st_order avg_wt_harv_filled

export excel using  "$input_data_cd\SQ_weight_per_catch.xlsx", firstrow(variables) replace 

