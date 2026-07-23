/*******************************************************************************
 Script:       calibration_catch_at_length.do
 Purpose:      Builds the calibration-year catch-at-length distribution for
               each species - the length composition of the fish anglers
               encounter, which is what makes a minimum size limit bite. Built
               separately for released and harvested fish, because the two
               have very different length distributions, and then combined.

               Released-at-length is assembled from several partial sources
               because no single survey measures released fish well: the CT,
               NJ and RI volunteer angler surveys, American Littoral Society
               tag-and-recapture records, and MRIP's own B2 (released alive)
               length records. Harvested-at-length comes from MRIP measured
               harvest. Each proportion-at-length is then scaled by the total
               discards and total harvest that
               compare_calibration_data_to_MRIP.do produced, giving NUMBERS at
               length rather than proportions, and the two are summed to
               catch-at-length. Finally a gamma distribution is fitted to
               smooth the result.
 Inputs:       2024 CT VAS SFL SCUP BSB.xlsx and the other volunteer-survey
               and tag workbooks, the MRIP size and B2 files named by
               $sizelist and $b2list, and simulated_catch_totals.dta.
 Outputs:      baseline_catch_at_length.csv,
               baseline_catch_at_length_region.dta,
               baseline_catch_at_length_state.csv,
               baseline_observed_catch_at_length.csv
 Dependencies: Globals $misc_data_cd, $sizelist, $b2list, $ndraws. Requires
               compare_calibration_data_to_MRIP.do to have produced the
               harvest and discard totals. Uses the user-written commands
               renvarlab, dsconcat and gammafit.
 Pipeline:     Step 7 of model_wrapper.do, gated by the toggle
               generate_baseline. Its output is read by the R calibration as
               the length distribution to draw simulated fish from, and by
               projected_catch_at_length.do as the baseline that the
               assessment-driven projection is applied to.

 Why a gamma fit rather than the raw empirical distribution: the pooled length
 samples are thin for some species x region cells, so the empirical
 proportions are ragged and contain zero-count lengths that would make certain
 fish impossible to draw. Fitting a smooth parametric distribution fills those
 gaps and keeps the simulation from inheriting sampling noise as if it were
 real structure.

 Note: the bare `cd' near the top takes no argument. In Stata that only
 DISPLAYS the working directory rather than changing it, so it is inert - but
 it reads as an unfinished line.
*******************************************************************************/

display "calibration_catch_at_length.do: assembling baseline catch-at-length from volunteer survey, tag and MRIP length data, then fitting gamma distributions. This may take several minutes."



************************************************************************************************************************************************
* This file generates calibration-year catch-at-length distributions for each species

* The general strategy is:
			* 1) pull release length data from various sources and compute proportion released-at-length
					* CT/NJ/RI volunteer angler survey, American Littoral Society tag and recapture data, MRIP b2
			* 2) pull harvest length data and compute proportion harvest-at-length
			* 3) multiply (1) and (2) by an estimate of total discards and harvest to get numbers released- and harvested-at-length, 
			*     sum across length categories to get catch-at-length 
			* 4) generate gamma-fitted projected catch-at-length distribtion  
			
*************************************************************************************************************************************************

* 1) 			
* CT VAS
cd 
import excel "$misc_data_cd/2024 CT VAS SFL SCUP BSB.xlsx", clear first 
renvarlab, lower
gen species="sf" if catch_common_name== "FLOUNDER, SUMMER"
replace species="scup" if catch_common_name== "SCUP"
replace species="bsb" if catch_common_name== "BASS, BLACK SEA"
keep if disp=="RELEASED"
expand quantity
gen nfish=1
rename length length_inches
gen length_cm=length_inches*2.54
gen year=year(tripdate)
gen state="CT"
gen source="CT_VAS"
rename tripdate date 
format date %td

keep year length* state source species nfish 

tempfile ct_vas
save `ct_vas', replace

* NJ VAS
import excel "$misc_data_cd/NJ-VAS BSB.xlsx", clear first 
tempfile njbsb
save `njbsb', replace

import excel "$misc_data_cd/NJ-VAS SF.xlsx", clear first 
append using `njbsb'
renvarlab, lower
tab species
replace species="bsb" if species== "blkseabass"
replace species="sf" if species== "sumflndr"
keep if disp=="Rel"
rename length length_inches
gen length_cm=length_inches*2.54
rename fishyear year
gen state="NJ"
gen source="NJ_VAS"
gen nfish=1
keep year length* state source species nfish
tempfile nj_vas
save `nj_vas', replace

* RI VAS
import excel "$misc_data_cd/sfl_scu_bsb_24_25_RI_release.xlsx", clear first 
renvarlab, lower
tab common
gen species="sf" if common== "FLOUNDER, SUMMER"
replace species="scup" if common== "SCUP"
replace species="bsb" if common== "BASS, BLACK SEA"
expand reportedquantity
rename specieslength length_inches
gen length_cm=length_inches*2.54
drop state
gen state="RI"
gen source="RI_VAS"
gen nfish=1
drop lengthunit
keep year length* state source species nfish
tempfile ri_vas
save `ri_vas', replace

* ALS tag
import excel "$misc_data_cd/FLK_ALS_TAG_DATA_2025.xlsx", clear first 
keep Date Place Species Length zonedescription
tempfile als_fluke_tag
save `als_fluke_tag', replace 

import excel "$misc_data_cd/BSB_ALS_TAG_DATA_2025.xlsx", clear first 
keep Date Place Species Length zonedescription
tempfile als_bsb_tag
save `als_bsb_tag', replace 

import excel "$misc_data_cd/SCP_ALS_TAG_DATA_2025.xlsx", clear first 
keep Date Place Species Length zonedescription
append using `als_bsb_tag'
append using `als_fluke_tag'

renvarlab, lower
split date, pars("/")
destring date3, replace
tab date3, missing
rename date3 year
keep if year==$calibration_year_num
drop date1 date2

split placetagged, pars(",")
replace placetagged2=ltrim(rtrim(placetagged2))
replace placetagged3=ltrim(rtrim(placetagged3))

gen state="MA" if placetagged2=="MA"
replace state="RI" if placetagged2=="RI"
replace state="CT" if placetagged2=="CT"
replace state="NY" if placetagged2=="NY"
replace state="NJ" if placetagged2=="NJ"
replace state="DE" if placetagged2=="DE"
replace state="MD" if placetagged2=="MD"
replace state="VA" if placetagged2=="VA"
replace state="NC" if placetagged2=="NC"

replace state="MA" if placetagged3=="MA" & state==""
replace state="RI" if placetagged3=="RI" & state==""
replace state="CT" if placetagged3=="CT" & state==""
replace state="NY" if placetagged3=="NY" & state==""
replace state="NJ" if placetagged3=="NJ" & state==""
replace state="DE" if placetagged3=="DE" & state==""
replace state="MD" if placetagged3=="MD" & state==""
replace state="VA" if placetagged3=="VA" & state==""
replace state="NC" if placetagged3=="NC" & state==""

drop if strmatch(placetagged, "* SC")==1
replace state= "NJ" if strmatch(placetagged, "* NJ")==1
replace state= "NY" if strmatch(placetagged, "* NY")==1
replace state= "NY" if strmatch(placetagged, "* NYC")==1
replace state= "DE" if strmatch(zonedescription, "* DE")==1
replace state= "MD" if strmatch(zonedescription, "* MD")==1
replace state= "NJ" if strmatch(placetagged, "* Nj")==1
replace state= "NY" if strmatch(placetagged, "Long Island Sound")==1
replace state= "NY" if strmatch(placetagged, "Cold Spring Harbor")==1
replace state= "NJ" if strmatch(placetagged, "Axel Carson Reef")==1
replace state= "CT" if strmatch(placetagged, "*CT")==1
replace state= "NJ" if strmatch(zonedescription, "NJ*")==1
replace state= "NY" if strmatch(placetagged, "*NY*")==1
replace state= "RI" if strmatch(zonedescription, "*Rhode Island*")==1
replace state= "NY" if strmatch(zonedescription, "*NY*")==1
replace state= "NJ" if strmatch(placetagged, "*South Amboy*")==1
replace state= "NJ" if strmatch(placetagged, "*S. Amboy*")==1
replace state= "NY" if strmatch(placetagged, "*Brooklyn*")==1
replace state= "NY" if strmatch(placetagged, "*Bklyn*")==1
replace state= "NY" if strmatch(placetagged, "*Rockaway*")==1
replace state= "NJ" if strmatch(placetagged, "*Atlantic Beach Reef*")==1
replace state= "NY" if strmatch(placetagged, "*Coney Island*")==1
replace state= "NY" if strmatch(zonedescription, "*Fire Island Inlet*")==1 & state==""
replace state= "NJ" if strmatch(placetagged, "*Keansburg*")==1 & state==""
replace state= "NJ" if strmatch(placetagged, "*Keyport*")==1 & state==""
replace state= "NY" if strmatch(placetagged, "*Mattituck, LIS*")==1 & state==""
replace state= "NY" if strmatch(placetagged, "*Middle Ground Light, LIS*")==1 & state==""
replace state= "NJ" if strmatch(placetagged, "*Navesink*")==1 & state==""
replace state= "CT" if strmatch(placetagged, "*Sandy Hook*")==1 & state==""
replace state= "NJ" if strmatch(placetagged, "*Perth Amboy*")==1 & state==""
replace state= "NY" if strmatch(placetagged, "*Verizano Bridge*")==1 & state==""
replace state= "NY" if strmatch(placetagged, "*Raritan Bay, Buoy 41*")==1 & state==""
replace state= "NY" if strmatch(zonedescription, "*Raritan Bay*")==1 & state==""
replace state= "NY" if strmatch(zonedescription, "*Port Jefferson*")==1 & state==""
*browse if state==""
drop if state==""

rename length length_inches
gen length_cm=length_inches*2.54
gen nfish=1
gen source="ALS"
replace species="sf" if species== "Fluke"
replace species="scup" if species== "Scup"
replace species="bsb" if species== "Black Sea Bass"
keep state source length* species nfish year

tempfile als_tag
save `als_tag', replace 

* ALS recapture
import excel "$misc_data_cd/FLK_ALS_REC_DATA_2025.xlsx", clear first 
keep RecaptureDate PlaceRecaptured Species Length zonedesc
tempfile als_fluke_rec
save `als_fluke_rec', replace 

import excel "$misc_data_cd/BSB_ALS_REC_DATA_2025.xlsx", clear first 
keep RecaptureDate PlaceRecaptured Species Length zonedesc
append using `als_fluke_rec'

renvarlab, lower
split recapturedate, pars("/")
destring recapturedate3, replace
tab recapturedate3, missing
rename recapturedate3 year
tab year 
keep if year==$calibration_year_num

drop recapturedate1 recapturedate2

split placerecaptured, pars(",")
replace placerecaptured2=ltrim(rtrim(placerecaptured2))
replace placerecaptured3=ltrim(rtrim(placerecaptured3))

gen state="MA" if placerecaptured2=="MA"
replace state="RI" if placerecaptured2=="RI"
replace state="CT" if placerecaptured2=="CT"
replace state="NY" if placerecaptured2=="NY"
replace state="NJ" if placerecaptured2=="NJ"
replace state="DE" if placerecaptured2=="DE"
replace state="MD" if placerecaptured2=="MD"
replace state="VA" if placerecaptured2=="VA"
replace state="NC" if placerecaptured2=="NC"

replace state="MA" if placerecaptured3=="MA" & state==""
replace state="RI" if placerecaptured3=="RI" & state==""
replace state="CT" if placerecaptured3=="CT" & state==""
replace state="NY" if placerecaptured3=="NY" & state==""
replace state="NJ" if placerecaptured3=="NJ" & state==""
replace state="DE" if placerecaptured3=="DE" & state==""
replace state="MD" if placerecaptured3=="MD" & state==""
replace state="VA" if placerecaptured3=="VA" & state==""
replace state="NC" if placerecaptured3=="NC" & state==""

*browse if state==""

drop if strmatch(placerecaptured, "* SC")==1
replace state= "NJ" if strmatch(placerecaptured, "* NJ")==1
replace state= "NY" if strmatch(placerecaptured, "* NY")==1
replace state= "NY" if strmatch(placerecaptured, "* NYC")==1
replace state= "DE" if strmatch(zonedesc, "* DE")==1
replace state= "MD" if strmatch(zonedesc, "* MD")==1
replace state= "NJ" if strmatch(placerecaptured, "* Nj")==1
replace state= "NY" if strmatch(placerecaptured, "Long Island Sound")==1
replace state= "NY" if strmatch(placerecaptured, "Cold Spring Harbor")==1
replace state= "NJ" if strmatch(placerecaptured, "Axel Carson Reef")==1
replace state= "CT" if strmatch(placerecaptured, "*CT")==1
replace state= "NJ" if strmatch(zonedesc, "NJ*")==1
replace state= "NY" if strmatch(placerecaptured, "*NY*")==1
replace state= "RI" if strmatch(zonedesc, "*Rhode Island*")==1
replace state= "NY" if strmatch(zonedesc, "*NY*")==1
replace state= "NJ" if strmatch(placerecaptured, "*South Amboy*")==1
replace state= "NJ" if strmatch(placerecaptured, "*S. Amboy*")==1
replace state= "NY" if strmatch(placerecaptured, "*Brooklyn*")==1
replace state= "NY" if strmatch(placerecaptured, "*Bklyn*")==1
replace state= "NY" if strmatch(placerecaptured, "*Rockaway*")==1
replace state= "NJ" if strmatch(placerecaptured, "*Atlantic Beach Reef*")==1
replace state= "NY" if strmatch(placerecaptured, "*Coney Island*")==1
replace state= "NY" if strmatch(zonedesc, "*Fire Island Inlet*")==1 & state==""
replace state= "NJ" if strmatch(placerecaptured, "*Keansburg*")==1 & state==""
replace state= "NJ" if strmatch(placerecaptured, "*Keyport*")==1 & state==""
replace state= "NY" if strmatch(placerecaptured, "*Mattituck, LIS*")==1 & state==""
replace state= "NY" if strmatch(placerecaptured, "*Middle Ground Light, LIS*")==1 & state==""
replace state= "NJ" if strmatch(placerecaptured, "*Navesink*")==1 & state==""
replace state= "CT" if strmatch(placerecaptured, "*Sandy Hook*")==1 & state==""
replace state= "NJ" if strmatch(placerecaptured, "*Perth Amboy*")==1 & state==""
replace state= "NY" if strmatch(placerecaptured, "*Verizano Bridge*")==1 & state==""
replace state= "NY" if strmatch(placerecaptured, "*Raritan Bay, Buoy 41*")==1 & state==""
replace state= "NY" if strmatch(zonedesc, "*Raritan Bay*")==1 & state==""
replace state= "NY" if strmatch(zonedesc, "*Port Jefferson*")==1 & state==""
*browse if state==""
drop if state==""

rename length length_inches
gen length_cm=length_inches*2.54
gen nfish=1
gen source="ALS"
replace species="sf" if species== "Fluke"
replace species="scup" if species== "Scup"
replace species="bsb" if species== "Black Sea Bass"
keep state source length* species nfish year

tempfile als_rec
save `als_rec', replace 


* MRIP data 
dsconcat $b2list
sort year strat_id psu_id id_code
drop if strmatch(id_code, "*xx*")==1
replace common=subinstr(lower(common)," ","",.)

sort year strat_id psu_id id_code
replace common=subinstr(lower(common)," ","",.)

* keep management unit states
keep if inlist(st,25, 44, 9, 36, 34, 51, 10, 24, 37)

gen state="MA" if st==25
replace state="MD" if st==24
replace state="RI" if st==44
replace state="CT" if st==9
replace state="NY" if st==36
replace state="NJ" if st==34
replace state="DE" if st==10
replace state="VA" if st==51
replace state="NC" if st==37

* keep only NC north based on county delineation from Tracey 
drop if state=="NC" & !inlist(15, 29, 41, 53, 55, 139, 143, 177, 187) // okay to drop here because we are not estimating SE's

keep if $calibration_year

gen species="sf" if inlist(common, "summerflounder") 
replace species="scup" if inlist(common, "scup") 
replace species="bsb" if inlist(common, "blackseabass") 

keep if inlist(common, "summerflounder",  "scup", "blackseabass") 
gen source="MRIP"
rename l_cm_bin length_cm 
rename l_in_bin length_inches

gen nfish=1
keep length* state species year nfish source
tempfile mrip
save `mrip', replace 

* Append all the discard length data together, aggregate to regions
u `mrip', clear
append using `als_rec'
append using `als_tag'
append using `ri_vas'
append using `nj_vas'
append using `ct_vas'

gen region="NO" if inlist(state, "MA", "RI", "CT", "NY") & inlist(species, "sf", "bsb")
replace region="NJ" if inlist(state, "NJ") & inlist(species, "sf", "bsb")
replace region="SO" if inlist(state, "DE", "MD", "VA", "NC") & inlist(species, "sf", "bsb")
replace region="CST" if inlist(species, "scup")

drop if  length_cm==. | length_in==.

replace length_cm = round(length_cm)
replace length_in = round(length_in)

collapse (sum) nfish, by(species region length_cm)
sort species region length nfish
order species region length nfish

gen spec_reg=species+"_"+region
tabstat nfish, stat(sum) by(spec_reg)

egen sumfish=sum(nfish), by(species region)
gen prop_b2=nfish/sumfish

rename length length 

tempfile prop_b2
save `prop_b2', replace 


* 2) 
* Pull harvest lengths from MRIP 
clear
mata: mata clear
tempfile tl1 sl1
dsconcat $triplist
sort year strat_id psu_id id_code
save `tl1'

clear
dsconcat $sizelist
sort year strat_id psu_id id_code
replace common=subinstr(lower(common)," ","",.)
save `sl1'

use `tl1'
merge 1:m year strat_id psu_id id_code using `sl1', keep(1 3) nogen

keep if $calibration_year

* keep management unit states
keep if inlist(st,25, 44, 9, 36, 34, 51, 10, 24, 37)

gen state="MA" if st==25
replace state="MD" if st==24
replace state="RI" if st==44
replace state="CT" if st==9
replace state="NY" if st==36
replace state="NJ" if st==34
replace state="DE" if st==10
replace state="VA" if st==51
replace state="NC" if st==37

drop region
gen region="NO" if inlist(state, "MA", "RI", "CT", "NY") 
replace region="NJ" if inlist(state, "NJ") 
replace region="SO" if inlist(state, "DE", "MD", "VA", "NC")


/*classify catch into the things I care about (common==$mycommon) and things I don't care about "ZZ" */
gen common_dom="ZZ"
replace common_dom="SF" if strmatch(common, "summerflounder") 
replace common_dom="BS" if strmatch(common, "blackseabass") 
replace common_dom="SC" if strmatch(common, "scup") 

* keep only NC north based on county delineation from Tracey 
replace common_dom="ZZ"  if state=="NC" & !inlist(cnty, 15, 29, 41, 53, 55, 139, 143, 177, 187)


/*
*Example of illegal harvest 
keep if state  == "NJ" & strmatch(common, "summerflounder") 
* Compute the number of imputated observations 
drop if lngth_imp==1
drop if area=="E"
gen nfish=1
collapse (sum) nfish, by(l_cm_bin)


twoway scatter nfish l_cm_bin, ///
    mcolor(red) ///
    ylab(#20, angle(horizontal) labsize(small)) ///
	xlab(#20, angle(horizontal) labsize(small)) ///
    xtitle("length (cm's)") ///
    xline(45.72, lcolor(black)) ///
	ytitle("") ///
    title("# of harvested fluke measured by MRIP in NJ in 2024", size(medium)) 
	
su nfish if l_cm_bin<45.72
return list
su nfish 
return list
*/

tostring wave, gen(w2)
tostring year, gen(year2)

gen my_dom_id_string=region+"_"+common_dom
replace my_dom_id_string=subinstr(ltrim(rtrim(my_dom_id_string))," ","",.)

preserve
keep my_dom_id my_dom_id_string
duplicates drop 
tempfile domains
save `domains', replace 
restore

/* this might speed things up if I re-classify all length=0 for the species I don't care about */
replace l_cm_bin=0 if !inlist(common, "summerflounder", "blackseabass", "scup")
replace l_in_bin=0 if !inlist(common, "summerflounder", "blackseabass", "scup")

sort year2  w2 strat_id psu_id id_code common_dom
svyset psu_id [pweight= wp_size], strata(strat_id) singleunit(certainty)

svy: tab l_cm_bin my_dom_id_string, count

	/*save some stuff:matrix of proportions, row names, column names, estimate of total population size*/
	mat eP=e(Prop)
	mat eR=e(Row)'
	mat eC=e(Col)
	local PopN=e(N_pop)

	local mycolnames: colnames(eC)
	mat colnames eP=`mycolnames'
	
	clear
	/*read the eP into a dataset and convert proportion of population into numbers*/
	svmat eP, names(col)
	foreach var of varlist *{
		replace `var'=`var'*`PopN'
	}
	
/*read in the "row" */
svmat eR
order eR
rename eR l_cm_bin
	
drop *ZZ	
ds l, not
renvarlab `r(varlist)', prefix(i)
	
reshape long i, i(l_) j(new) string
split new, parse(_)
rename new1 region
rename new2 species
drop new
rename i nfish
replace species="sf" if species=="SF"
replace species="bsb" if species=="BS"
replace species="scup" if species=="SC"
order region species l n
sort region species l n
drop if l_cm_bin==0

preserve
keep if species=="scup"
replace region="CST"
collapse (sum) n, by(species region l)
tempfile scup
save `scup',replace
restore

drop if species=="scup"
append using `scup'

rename l_ length 
egen sumfish=sum(nfish), by(species region)
gen prop_ab1=nfish/sumfish

tempfile prop_ab1
save `prop_ab1', replace 

* Merge proportion discards and harvest-at-length together 
merge 1:1 species region length using `prop_b2'
drop _merge spec_reg sumfish

expand $ndraws
bysort region species l: gen draw=_n
tempfile props
save `props', replace 

*3) 
*Pull estimates of total harvest and discards from MRIP by region, multiply by proportions-at-length, sum across length categories  
u "$misc_data_cd\simulated_catch_totals.dta", replace 

collapse (sum) tot_sf_keep_sim tot_sf_rel_sim tot_bsb_keep_sim tot_bsb_rel_sim tot_scup_keep_sim tot_scup_rel_sim, by(state draw)
rename tot_sf_keep_sim harvest_sf
rename tot_bsb_keep_sim harvest_bsb
rename tot_scup_keep_sim harvest_scup

rename tot_sf_rel_sim discards_sf
rename tot_bsb_rel_sim discards_bsb
rename tot_scup_rel_sim discards_scup

reshape long harvest discards, i(draw state) j(disp) string
rename disp species
replace species="bsb" if species=="_bsb"
replace species="sf" if species=="_sf"
replace species="scup" if species=="_scup"

gen region="NO" if inlist(state, "MA", "RI", "CT", "NY") 
replace region="NJ" if inlist(state, "NJ") 
replace region="SO" if inlist(state, "DE", "MD", "VA", "NC")

collapse (sum) harvest discards, by(species region draw)

preserve
keep if species=="scup"
replace region="CST"
collapse (sum) harvest discards, by(species region draw)
tempfile scup
save `scup',replace
restore

drop if species=="scup"
append using `scup'

merge 1:m species region draw using `props'
drop _merge
replace prop_ab1=0 if prop_ab1==.
replace prop_b2=0 if prop_b2==.

replace harvest=harvest*prop_ab1
replace discards=discards*prop_b2
gen catch=harvest+discards
collapse catch, by(region species length draw)
sort region species length
tostring draw, gen(draw2)

gen domain=region+"_"+species +"_"+draw2

replace catch=round(catch)

egen sumcatch=sum(catch), by(domain)
format sumcatch %12.0gc
sort draw region species length
gen observed_prob = catch/sumcatch
drop sumcatch 

*4) 
* generate gamma-fitted projected catch-at-length distribtion  
preserve 
rename length fitted_length
keep fitted_length observed_prob catch species region domain draw
duplicates drop
export delimited using "$misc_data_cd/baseline_observed_catch_at_length.csv", replace 
tempfile observed_prob
save `observed_prob', replace
restore


* new code using MOM to avoid non-convergence 
tempfile new
save `new', replace
global fitted_sizes

levelsof domain, local(regs)

foreach r of local regs {
    use `new', clear
    keep if domain=="`r'"
    di "`r'"

    keep length catch
    drop if missing(length) | missing(catch)
    drop if catch<=0
	replace catch=round(catch)
	su catch
	local tot_n_fish=`r(sum)'
	
    * Gamma needs strictly positive support
    drop if length<=0

    * Estimate gamma parameters robustly (MOM with freq weights)
    quietly summarize length [fw=catch], meanonly
    local mu = r(mean)
    local Nw = r(sum_w)

    * Weighted variance: Var = E[x^2] - (E[x])^2 using the same freq weights
    gen double length2 = length^2
    quietly summarize length2 [fw=catch], meanonly
    local ex2 = r(mean)
    local v   = `ex2' - (`mu'^2)

    * Guard: if variance is 0 or numerically tiny, make it a near-degenerate gamma
    if (`v'<=1e-10 | missing(`v') | missing(`mu') | `mu'<=0) {
        * Put essentially all mass at mu by using huge alpha
        local alpha = 1e6
        local beta  = `mu'/`alpha'
    }
    else {
        local alpha = (`mu'^2)/`v'
        local beta  = `v'/`mu'
    }

    * Simulate a truncated gamma sample via rejection sampling
    local ndraw = `tot_n_fish'   // sample size for the simulated distribution
    clear
    set obs `ndraw'

    * draw
    gen double gammafit = rgamma(`alpha', `beta')
    replace gammafit = round(gammafit)


    * If rejection killed everything, try again with more draws (once)
    if _N==0 {
        clear
        set obs `=5*`ndraw''
        gen double gammafit = rgamma(`alpha', `beta')
        replace gammafit = round(gammafit)
        if _N==0 continue
    }

    gen nfish = 1
    collapse (sum) nfish, by(gammafit)
    egen sumnfish = total(nfish)
    gen double fitted_prob = nfish/sumnfish
    gen domain = "`r'"

    tempfile fitted_sizes_`=_N'   
    save `fitted_sizes_`=_N'', replace
    global fitted_sizes "$fitted_sizes `fitted_sizes_`=_N''"
}

clear
dsconcat $fitted_sizes
rename gammafit fitted_length

merge 1:1 fitted_length domain using `observed_prob'
sort domain fitted_length 
mvencode fitted_prob observed_prob, mv(0) override 

split domain, parse(_)
replace species=domain2
replace region=domain1
drop domain1 domain2
replace domain=region+"_"+species+"_"+domain3

drop draw
rename domain3 draw
destring draw, replace 

rename fitted_length length 

* truncate the fitted distribution to the observed range
levelsof domain, local(doms)
foreach d of local doms{
quietly summarize length if observed_prob!=0 & !missing(observed_prob) & domain=="`d'"
return list
local minL = `r(min)'
local maxL = `r(max)'
drop if (length<`minL' | length>`maxL' ) & domain=="`d'"
}

egen sum_fitted_prob=sum(fitted_prob), by(domain)
replace fitted_prob=fitted_prob/sum_fitted_prob
sort species region draw length

egen sum_nfish_catch=sum(catch), by(species region draw)
gen nfish_catch_from_fitted=fitted_prob*sum_nfish_catch
gen nfish_catch_from_raw=observed_prob*sum_nfish_catch

drop _merge
drop sum*
save "$misc_data_cd/baseline_catch_at_length_region.dta", replace 


* Prepare the data for export to simulation
keep length fitted species region draw
drop if fitted==0

preserve
keep if species=="scup"
expand 9 
bysort species length draw: gen n=_n 
gen state="MA" if n==1
replace state="MD" if n==2
replace state="RI" if n==3
replace state="CT" if n==4
replace state="NY" if n==5
replace state="NJ" if n==6
replace state="DE" if n==7
replace state="VA" if n==8
replace state="NC" if n==9
drop n
tempfile scup
save `scup', replace 
restore 

preserve
keep if species=="sf"
expand 4 if region=="NO"
bysort region length draw: gen n=_n if region=="NO"
gen state="MA" if n==1
replace state="RI" if n==2
replace state="CT" if n==3
replace state="NY" if n==4
drop n

expand 4 if region=="SO"
bysort region length draw: gen n=_n if region=="SO"
replace state="MD" if n==1
replace state="VA" if n==2
replace state="DE" if n==3
replace state="NC" if n==4
drop n
replace state="NJ" if region=="NJ"
tempfile sf
save `sf', replace 
restore 

preserve
keep if species=="bsb"
expand 4 if region=="NO"
bysort region length draw: gen n=_n if region=="NO"
gen state="MA" if n==1
replace state="RI" if n==2
replace state="CT" if n==3
replace state="NY" if n==4
drop n

expand 4 if region=="SO"
bysort region length draw: gen n=_n if region=="SO"
replace state="MD" if n==1
replace state="VA" if n==2
replace state="DE" if n==3
replace state="NC" if n==4
drop n
replace state="NJ" if region=="NJ"
tempfile bsb
save `bsb', replace 
restore 

clear
u `scup', clear 
append using `sf'
append using `bsb'

destring draw, replace
drop region
order state species draw length
sort state species draw length
compress
export delimited using "$misc_data_cd/baseline_catch_at_length_state.csv", replace 

