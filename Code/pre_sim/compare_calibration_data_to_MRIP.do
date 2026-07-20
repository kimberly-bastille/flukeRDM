/*******************************************************************************
 Script:       compare_calibration_data_to_MRIP.do
 Purpose:      Validation gate for the calibration stage, and the producer of
               the harvest and discard totals the baseline catch-at-length
               step needs. Computes mean catch-per-trip from the simulated
               daily draws, scales those means by the number of trips on each
               day to get simulated TOTALS, and compares both means and totals
               to MRIP - at state x mode x wave and again at the coarser
               state x mode level. Writes comparison figures for human review
               and saves the simulated totals.
 Inputs:       directed_trips_calibration_<ST>.csv,
               calib_catch_draws_<ST>_<i>.dta,
               baseline_mrip_catch_processed.xlsx, and the MRIP catch and
               directed-trip totals at two levels of aggregation:
               mrip_catch_calib_state_mode.dta,
               mrip_catch_calib_state_mode_wave.dta,
               mrip_dtrip_calib_state_mode.dta,
               mrip_dtrip_calib_state_mode_wave.dta
 Outputs:      simulated_catch_totals3.dta, simulated_catch_totals.dta, and
               per-state comparison figures under $figure_cd.
 Dependencies: Globals $misc_data_cd, $calib_catch_data_cd, $figure_cd and
               $ndraws. Requires calibration_catch_per_trip_part2.do to have
               produced the calibration catch draws. Uses the user-written
               commands renvarlab and grc1leg.
 Pipeline:     Step 6 of model_wrapper.do, gated by the toggle
               compare_calibration_MRIP. Its output is not merely diagnostic:
               simulated_catch_totals.dta is the file the R calibration reads
               as its MRIP target, so this script sits ON the critical path,
               not to one side of it. Its projection-year analog,
               compare_projection_data_to_MRIP.do, is much shorter because it
               does not have to produce totals for a downstream stage.

 Why totals and not just means: a stratum can match MRIP on mean
 catch-per-trip and still miss badly on totals if the effort estimate is off,
 and it is the totals that the harvest limits bind on. Both are checked.
*******************************************************************************/

display "compare_calibration_data_to_MRIP.do: computing simulated calibration totals and comparing to MRIP. This reads every calib_catch_draws file and may take a long time."





*A) The copula simulated data is used to generate daily catch-draw data, so here, I:
		*1) compute mean catch-per-trip from the daily catch-draw data
		*2) compute total catch/harvest/discards from the daily catch-draw data by multiplying
		*    mean catch/harvest/discards-per trip by the number of trips in that day
		*3) compare catch-per-trip means and total simulated catch from 2) with estimates from MRIP, both at the state-mode-wave level and the state-mode level

*B1 and B2)  
clear
tempfile master
save `master', emptyok

local statez "MA RI CT NY NJ DE MD VA NC"

foreach s of local statez{
	
*local i=1
*local s="RI"

import delimited using  "$misc_data_cd\directed_trips_calibration_`s'.csv", clear 
drop if dtrip==0

gen wave=1 if inlist(month, 1,2)
replace wave=2 if inlist(month, 3, 4)
replace wave=3 if inlist(month, 5,6)
replace wave=4 if inlist(month, 7,8)
replace wave=5 if inlist(month, 9,10)
replace wave=6 if inlist(month, 11,12)
collapse (sum) dtrip, by(mode wave state draw)	

tempfile trips
save `trips', replace

forv i=1/$ndraws{

use "$calib_catch_data_cd\calib_catch_draws_`s'_`i'.dta", clear 

collapse (mean) sf_keep_sim sf_cat sf_rel_sim bsb_keep_sim bsb_rel_sim bsb_cat scup_keep_sim scup_rel_sim scup_cat, by(mode state wave)
gen draw = `i'
merge 1:1 mode wave state draw using `trips', keep(3) nogen

rename sf_cat sf_cat_sim
rename bsb_cat bsb_cat_sim
rename scup_cat scup_cat_sim

local vars sf_keep_sim sf_cat_sim sf_rel_sim bsb_keep_sim bsb_rel_sim bsb_cat_sim scup_keep_sim scup_rel_sim scup_cat_sim 
foreach v of local vars{
	gen tot_`v'= dtrip*`v'
	
}

append using `master'
save `master', replace
}
}
use `master', clear

save "$misc_data_cd\simulated_catch_totals3.dta", replace 


*B3 compare means @ state, mode, wave level
u "$misc_data_cd\simulated_catch_totals3.dta", clear 
rename dtrip tot_dtrip_sim

ds draw mode state wave, not
local vars `r(varlist)'
foreach v of local vars{
	mvencode `v', mv(0) override
}

tostring draw, gen(draw2)
gen domain=state+"_"+mode+"_"+draw2
encode domain, gen(domain2)
xtset domain2 wave
tsfill, full

decode domain2, gen(domain3)
split domain3, parse(_)
replace state=domain31
replace mode=domain32
replace draw2=domain33

drop draw draw2  domain domain2 domain3 domain31 domain32
destring domain33, replace
rename domain33 draw


order mode state wave draw
ds draw mode state wave, not
local vars `r(varlist)'
foreach v of local vars{
	mvencode `v', mv(0) override
}

collapse (mean) sf_keep_sim sf_rel_sim sf_cat_sim bsb_keep_sim bsb_rel_sim bsb_cat_sim scup_keep_sim scup_rel_sim scup_cat_sim 	///
						(sd) sd_sf_keep_sim=sf_keep_sim  ///
						sd_sf_cat_sim=sf_cat_sim  ///
						sd_sf_rel_sim=sf_rel_sim ///
						sd_bsb_keep_sim=bsb_keep_sim ///
						sd_bsb_rel_sim=bsb_rel_sim ///
						sd_bsb_cat_sim=bsb_cat_sim ///
						sd_scup_keep_sim=scup_keep_sim ///
						sd_scup_rel_sim=scup_rel_sim ///
						sd_scup_cat_sim=scup_cat_sim, by(state mode wave)
						
renvarlab sf* bsb* scup*, prefix(tot_)					

reshape long tot_ sd_, i(state wave mode) j(new) string
rename tot_ sim_total 
rename sd_ sim_sd
split new, parse(_)
rename new1 species
rename new2 disp
drop new3
drop new
tostring wave, replace 
tempfile sim
save `sim', replace


import excel using "$misc_data_cd\baseline_mrip_catch_processed.xlsx", clear first 
keep my_dom_id_string-missing_sesf_rel
drop missing*
duplicates drop
split my, parse(_)
rename my_dom_id_string1 state
rename my_dom_id_string2 wave
rename my_dom_id_string3 mode
drop my_dom_id_string4
reshape long mean se, i(state wave mode) j(new) string
rename mean mrip_total 
rename se mrip_sd
split new, parse(_)
rename new1 species
rename new2 disp
drop new
merge 1:1 state wave mode  species disp using `sim'
browse if _merge==2
browse

gen mrip_ul=mrip_total+1.96*mrip_sd
gen mrip_ll=mrip_total-1.96*mrip_sd
gen sim_ul=sim_total+1.96*sim_sd
gen sim_ll=sim_total-1.96*sim_sd

drop if mrip_total==0 & sim_total==0
drop if disp=="cat"
drop if mrip_total==. & sim_total==0

gen domain=species+"_"+disp

gen pct_diff = ((sim_total-mrip_total)/mrip_total)*100
gen diff= sim_total-mrip_total
sort pct_diff

tempfile new
save `new', replace 

levelsof state, local(state_list)

foreach s in `state_list'{
	u `new', clear 
	keep if state=="`s'"

	tempfile new1
	save `new1', replace
	
	levelsof domain, local(domain_list)
		
		foreach d in `domain_list' {
		u `new1', clear
		keep if domain=="`d'"
	
		encode my_dom_id_string, gen(my_dom_id)
		gen my_dom_id_mrip = my_dom_id+0.1 
		gen my_dom_id_sim = my_dom_id-0.1  

* Start by clearing any existing macro
local xlabels ""

* Loop over the levels of the encoded variable
levelsof my_dom_id, local(levels)

foreach l of local levels {
    local label : label (my_dom_id) `l'
    local xlabels `xlabels' `l' "`label'" 
}

qui twoway (rcap mrip_ul mrip_ll my_dom_id_mrip if domain=="`d'", color(blue)  ) ///
			(scatter mrip_total my_dom_id_mrip if domain=="`d'",  msymbol(o) mcolor(blue)) ///
			(rcap sim_ul sim_ll my_dom_id_sim if domain=="`d'",  color(red)) ///
			(scatter sim_total my_dom_id_sim if domain=="`d'", msymbol(o) mcolor(red)), ///
			legend(order(2 "MRIP estimate" 4 "Simulated estimate") size(small) rows(1)) ///
			ytitle("") xtitle("") ylabel(#10,  angle(horizontal) ) ///
			xlabel(`xlabels',  labsize(small) angle(45)) ///
			title("`d'", size(medium)) name(`d'_`s'_new, replace) 
		}
  }

 sort state wave mode 

*------------------------------------------------------------
* Combine mean catch-per-trip graphs that exist in memory
*------------------------------------------------------------
local states MA RI CT NY NJ DE MD VA NC
local domains bsb_keep sf_keep scup_keep bsb_rel sf_rel scup_rel

foreach s of local states {

    local graph_list ""

    foreach d of local domains {

        local graph_name `d'_`s'_new

        * Check whether graph exists in memory
        capture graph describe `graph_name'

        if !_rc {
            local graph_list `graph_list' `graph_name'
        }
        else {
            display as text ///
                "Graph `graph_name' does not exist; excluding it from `s' combined graph."
        }
    }

    * Only combine/export when at least one graph exists
    if `"`graph_list'"' != "" {

        grc1leg `graph_list', ///
            cols(3) ///
            title("Mean catch-per-trip, MRIP vs. simulated estimates `s'", ///
                  size(small)) ///
            name(mean_catch_`s', replace)

        graph export ///
            "$figure_cd/mean_catch_MRIP_simulated_`s'.png", ///
            as(png) replace
    }
    else {
        display as error ///
            "No mean catch-per-trip graphs exist for state `s'; no combined graph exported."
    }
}



*B3 compare catch totals @ state, mode, wave level
u "$misc_data_cd\simulated_catch_totals3.dta", clear 
rename dtrip tot_dtrip_sim

ds draw mode state wave, not
local vars `r(varlist)'
foreach v of local vars{
	mvencode `v', mv(0) override
}

	
collapse (sum) tot_sf_keep_sim tot_sf_cat_sim tot_sf_rel_sim ///
						  tot_bsb_keep_sim tot_bsb_rel_sim tot_bsb_cat_sim ///
						  tot_scup_keep_sim tot_scup_rel_sim tot_scup_cat_sim tot_dtrip_sim , by(state mode wave draw)

collapse (mean) tot_sf_keep_sim tot_sf_cat_sim tot_sf_rel_sim ///
						  tot_bsb_keep_sim tot_bsb_rel_sim tot_bsb_cat_sim ///
						  tot_scup_keep_sim tot_scup_rel_sim tot_scup_cat_sim tot_dtrip_sim ///
				(sd)	sd_sf_keep_sim=tot_sf_keep_sim sd_sf_cat_sim =tot_sf_cat_sim sd_sf_rel_sim =tot_sf_rel_sim ///
						  sd_bsb_keep_sim=tot_bsb_keep_sim sd_bsb_rel_sim =tot_bsb_rel_sim sd_bsb_cat_sim =tot_bsb_cat_sim ///
						  sd_scup_keep_sim = tot_scup_keep_sim sd_scup_rel_sim = tot_scup_rel_sim sd_scup_cat_sim = tot_scup_cat_sim sd_dtrip_sim=tot_dtrip_sim, by(state mode wave)
						  
reshape long tot_ sd_, i(mode state wave) j(new) string
rename tot_ sim_total 
rename sd_ sim_sd
split new, parse(_)
rename new1 species
rename new2 disp
drop new3
drop new
tostring wave, replace
replace disp="dtrip" if species=="dtrip"
replace species="NA" if disp=="dtrip"

preserve
keep if disp=="dtrip"
gen sim_ul = sim_total+1.96*sim_sd
gen sim_ll = sim_total-1.96*sim_sd
tempfile simdtrip
save `simdtrip', replace
restore 

drop if disp=="dtrip"
tempfile sim
save `sim', replace

u "$misc_data_cd\mrip_catch_calib_state_mode_wave.dta", clear 

reshape long total se ll ul , i(mode state wave) j(new) string
rename tot mrip_total 
rename ll mrip_ll
rename ul mrip_ul
drop se
split new, parse(_)
rename new1 species
rename new2 disp
drop new3

merge 1:1 state mode species wave disp using `sim',  keep(3) nogen
gen sim_ul = sim_total+1.96*sim_sd
gen sim_ll = sim_total-1.96*sim_sd

sort state wave species disp mode
tempfile catch
save `catch', replace 


u "$misc_data_cd\mrip_dtrip_calib_state_mode_wave.dta", clear 
rename se_mrip se_dtrip_mrip
rename ll ll_dtrip_mrip
rename ul ul_dtrip_mrip
rename dtrip_mrip tot_dtrip_mrip
reshape long tot_ se_ ll_ ul_, i(mode state wave ) j(new) string
rename tot_ mrip_total 
rename ll mrip_ll
rename ul_ mrip_ul
drop se_

drop year 
rename new disp
replace disp="dtrip"
gen species="NA"
drop my_dom_id_string
merge 1:1 state wave mode species disp  using `simdtrip', keep(3)

append using `catch'

drop _merge new 

replace disp="discards" if disp=="rel"
replace disp="harvest" if disp=="keep"
replace disp="catch" if disp=="cat"

gen domain=species+"_"+disp
replace domain="dtrip" if domain=="NA_dtrip"


gen pct_diff = ((sim_total-mrip_total)/mrip_total)*100
gen diff= sim_total-mrip_total

sort pct_diff
sort diff

local vars mrip_total mrip_ll mrip_ul sim_total sim_ul sim_ll
foreach v of local vars{
	replace `v'=`v'/1000
}

replace  my_dom_id_string=state+"_"+wave+"_"+mode

tempfile new
save `new', replace 


levelsof state, local(state_list)

foreach s in `state_list'{
	u `new', clear 
	keep if state=="`s'"
	
	encode my_dom_id_string , gen(my_dom_id)  
	gen my_dom_id_mrip = my_dom_id+0.1 
	gen my_dom_id_sim = my_dom_id-0.1  
		
	levelsof domain, local(domain_list)
		foreach d in `domain_list' {


* Start by clearing any existing macro
local xlabels ""

* Loop over the levels of the encoded variable
levelsof my_dom_id, local(levels)

foreach l of local levels {
    local label : label (my_dom_id) `l'
    local xlabels `xlabels' `l' "`label'" 
}


qui twoway (rcap mrip_ul mrip_ll my_dom_id_mrip if domain=="`d'", color(blue)  ) ///
			(scatter mrip_total my_dom_id_mrip if domain=="`d'",  msymbol(o) mcolor(blue)) ///
			(rcap sim_ul sim_ll my_dom_id_sim if domain=="`d'",  color(red)) ///
			(scatter sim_total my_dom_id_sim if domain=="`d'", msymbol(o) mcolor(red)), ///
			legend(order(2 "MRIP estimate" 4 "Simulated estimate") size(small) rows(1)) ///
			ytitle("# ('000s)", xoffset(-3)) xtitle("") ylabel(#10,  angle(horizontal) ) ///
			xlabel(`xlabels',  angle(45) labsize(small)) ///
			title("`d'", size(medium)) name(`d'_`s', replace) 
		}
  }

*------------------------------------------------------------
* Combine existing directed-trip graphs across states
*------------------------------------------------------------
local states MA RI CT NY NJ DE MD VA NC
local graph_list ""

foreach s of local states {

    local graph_name dtrip_`s'

    capture graph describe `graph_name'

    if !_rc {
        local graph_list `graph_list' `graph_name'
    }
    else {
        display as text ///
            "Graph `graph_name' does not exist; excluding it."
    }
}

if `"`graph_list'"' != "" {

    grc1leg `graph_list', ///
        cols(3) ///
        title("Directed trips, MRIP vs. simulated estimates", ///
              size(small)) ///
        name(dtrip_all_states, replace)

    graph export ///
        "$figure_cd/dtrip_wave_MRIP_simulated_all_states.png", ///
        as(png) replace
}
else {
    display as error ///
        "No directed-trip graphs exist; combined graph was not exported."
}


*------------------------------------------------------------
* Combine catch-total graphs that exist in memory
*------------------------------------------------------------
local states MA RI CT NY NJ DE MD VA NC
local domains ///
    bsb_catch sf_catch scup_catch ///
    bsb_harvest sf_harvest scup_harvest

foreach s of local states {

    local graph_list ""

    foreach d of local domains {

        local graph_name `d'_`s'

        capture graph describe `graph_name'

        if !_rc {
            local graph_list `graph_list' `graph_name'
        }
        else {
            display as text ///
                "Graph `graph_name' does not exist; excluding it from `s' combined graph."
        }
    }

    if `"`graph_list'"' != "" {

        grc1leg `graph_list', ///
            cols(3) ///
            title("Catch totals, MRIP vs. simulated estimates `s'", ///
                  size(small)) ///
            name(catch_totals_`s', replace)

        graph export ///
            "$figure_cd/catch_total_wave_MRIP_simulated_`s'.png", ///
            as(png) replace
    }
    else {
        display as error ///
            "No catch-total graphs exist for state `s'; no combined graph exported."
    }
}


*B3 compare catch totals @ state and mode
u "$misc_data_cd\simulated_catch_totals3.dta", clear 
rename dtrip tot_dtrip_sim

ds draw mode state wave, not
local vars `r(varlist)'
foreach v of local vars{
	mvencode `v', mv(0) override
}
	
collapse (sum) tot_sf_keep_sim tot_sf_cat_sim tot_sf_rel_sim ///
						  tot_bsb_keep_sim tot_bsb_rel_sim tot_bsb_cat_sim ///
						  tot_scup_keep_sim tot_scup_rel_sim tot_scup_cat_sim tot_dtrip_sim , by(state mode draw)

		  
collapse (mean) tot_sf_keep_sim tot_sf_cat_sim tot_sf_rel_sim ///
						  tot_bsb_keep_sim tot_bsb_rel_sim tot_bsb_cat_sim ///
						  tot_scup_keep_sim tot_scup_rel_sim tot_scup_cat_sim tot_dtrip_sim ///
				(sd)	sd_sf_keep_sim=tot_sf_keep_sim sd_sf_cat_sim =tot_sf_cat_sim sd_sf_rel_sim =tot_sf_rel_sim ///
						  sd_bsb_keep_sim=tot_bsb_keep_sim sd_bsb_rel_sim =tot_bsb_rel_sim sd_bsb_cat_sim =tot_bsb_cat_sim ///
						  sd_scup_keep_sim = tot_scup_keep_sim sd_scup_rel_sim = tot_scup_rel_sim sd_scup_cat_sim = tot_scup_cat_sim sd_dtrip_sim=tot_dtrip_sim, by(state mode)



reshape long tot_ sd_, i(mode state ) j(new) string
rename tot_ sim_total 
rename sd_ sim_sd
split new, parse(_)
rename new1 species
rename new2 disp
drop new3
drop new
replace disp="dtrip" if species=="dtrip"
replace species="NA" if disp=="dtrip"


preserve
keep if disp=="dtrip"
gen sim_ul = sim_total+1.96*sim_sd
gen sim_ll = sim_total-1.96*sim_sd
tempfile simdtrip
save `simdtrip', replace
restore 

drop if disp=="dtrip"
tempfile sim
save `sim', replace


u "$misc_data_cd\mrip_catch_calib_state_mode.dta", clear 
reshape long total se ll ul , i(mode state) j(new) string
rename tot mrip_total 
rename ll mrip_ll
rename ul mrip_ul
drop se
split new, parse(_)
rename new1 species
rename new2 disp
drop new3


merge 1:1 state mode species disp using `sim',  keep(3) nogen
gen sim_ul = sim_total+1.96*sim_sd
gen sim_ll = sim_total-1.96*sim_sd

sort state species disp mode
tempfile catch
save `catch', replace 


u "$misc_data_cd\mrip_dtrip_calib_state_mode.dta", clear 
rename se_mrip se_dtrip_mrip
rename ll ll_dtrip_mrip
rename ul ul_dtrip_mrip
rename dtrip_mrip tot_dtrip_mrip
reshape long tot_ se_ ll_ ul_, i(mode state ) j(new) string
rename tot_ mrip_total 
rename ll mrip_ll
rename ul_ mrip_ul
drop se_

drop year 
rename new disp
replace disp="dtrip"
gen species="NA"
drop my_dom_id_string
merge 1:1 state mode species disp  using `simdtrip', keep(3)

append using `catch'

drop _merge new 

replace disp="discards" if disp=="rel"
replace disp="harvest" if disp=="keep"
replace disp="catch" if disp=="cat"

gen domain=species+"_"+disp
replace domain="dtrip" if domain=="NA_dtrip"

gen pct_diff = ((sim_total-mrip_total)/mrip_total)*100
gen diff= sim_total-mrip_total

sort pct_diff
sort diff

local vars mrip_total mrip_ll mrip_ul sim_total sim_ul sim_ll
foreach v of local vars{
	replace `v'=`v'/1000
}

drop my_dom_id_string
gen  my_dom_id_string=state+"_"+mode

tempfile new
save `new', replace 


levelsof state, local(state_list)

foreach s in `state_list'{
	u `new', clear 
	keep if state=="`s'"
	
	encode my_dom_id_string , gen(my_dom_id)  
	gen my_dom_id_mrip = my_dom_id+0.1 
	gen my_dom_id_sim = my_dom_id-0.1  
		
	levelsof domain, local(domain_list)
		foreach d in `domain_list' {


* Start by clearing any existing macro
local xlabels ""

* Loop over the levels of the encoded variable
levelsof my_dom_id, local(levels)

foreach l of local levels {
    local label : label (my_dom_id) `l'
    local xlabels `xlabels' `l' "`label'" 
}


qui twoway (rcap mrip_ul mrip_ll my_dom_id_mrip if domain=="`d'", color(blue)  ) ///
			(scatter mrip_total my_dom_id_mrip if domain=="`d'",  msymbol(o) mcolor(blue)) ///
			(rcap sim_ul sim_ll my_dom_id_sim if domain=="`d'",  color(red)) ///
			(scatter sim_total my_dom_id_sim if domain=="`d'", msymbol(o) mcolor(red)), ///
			legend(order(2 "MRIP estimate" 4 "Simulated estimate") size(small) rows(1)) ///
			ytitle("# ('000s)", xoffset(-3)) xtitle("") ylabel(#10,  angle(horizontal) ) ///
			xlabel(`xlabels',  angle(45) labsize(small)) ///
			title("`d'", size(medium)) name(`d'_`s', replace) 
		}
  }

*------------------------------------------------------------
* Combine total-by-state graphs that exist in memory
*------------------------------------------------------------

* These are the states used in the graph-generation loop
local states MA RI CT NY NJ DE MD VA NC


*============================================================
* 1. Directed-trip graphs across all states
*============================================================
local dtrip_graphs ""

foreach s of local states {

    local graph_name dtrip_`s'

    capture graph describe `graph_name'

    if !_rc {
        local dtrip_graphs `dtrip_graphs' `graph_name'
    }
    else {
        display as text ///
            "Graph `graph_name' does not exist; excluding it from the combined directed-trip graph."
    }
}

if `"`dtrip_graphs'"' != "" {

    grc1leg `dtrip_graphs', ///
        cols(3) ///
        title("Directed trips, MRIP vs. simulated estimates", ///
              size(small)) ///
        name(dtrip_state_all, replace)

    graph export ///
        "$figure_cd/dtrip_state_MRIP_simulated_all_states.png", ///
        as(png) replace
}
else {
    display as error ///
        "No state-level directed-trip graphs exist; combined graph was not exported."
}


*============================================================
* 2. Catch-total graphs separately for each state
*============================================================
local catch_domains ///
    bsb_catch ///
    sf_catch ///
    scup_catch ///
    bsb_harvest ///
    sf_harvest ///
    scup_harvest

foreach s of local states {

    local catch_graphs ""

    foreach d of local catch_domains {

        local graph_name `d'_`s'

        capture graph describe `graph_name'

        if !_rc {
            local catch_graphs `catch_graphs' `graph_name'
        }
        else {
            display as text ///
                "Graph `graph_name' does not exist; excluding it from the `s' catch-total graph."
        }
    }

    if `"`catch_graphs'"' != "" {

        grc1leg `catch_graphs', ///
            cols(3) ///
            title("Catch totals, MRIP vs. simulated estimates `s'", ///
                  size(small)) ///
            name(catch_total_state_`s', replace)

        graph export ///
            "$figure_cd/catch_total_state_MRIP_simulated_`s'.png", ///
            as(png) replace
    }
    else {
        display as error ///
            "No catch-total graphs exist for `s'; no combined graph was exported."
    }
}
	






** FINAL STEP

* Once the simulated totals approximate MRIP, save the data to be used in the R code simulation
u "$misc_data_cd\simulated_catch_totals3.dta", clear 
rename dtrip tot_dtrip_sim

ds draw mode state wave, not
local vars `r(varlist)'
foreach v of local vars{
	mvencode `v', mv(0) override
}
	
collapse (sum) tot_sf_keep_sim tot_sf_cat_sim tot_sf_rel_sim ///
						  tot_bsb_keep_sim tot_bsb_rel_sim tot_bsb_cat_sim ///
						  tot_scup_keep_sim tot_scup_rel_sim tot_scup_cat_sim tot_dtrip_sim , by(state mode draw)
						  
save "$misc_data_cd\simulated_catch_totals.dta", replace 
					  




* Remove extraneous columns from the catch-per-trip data
mata: mata clear
clear

local statez "MA RI CT NY NJ DE MD VA NC"

foreach s of local statez {
	forvalues i = 1/$ndraws {
       
	   u "$calib_catch_data_cd\calib_catch_draws_`s'_`i'.dta", clear
	   drop  month wave  sf_keep_sim sf_rel_sim bsb_keep_sim bsb_rel_sim scup_keep_sim scup_rel_sim state
	   order draw mode date  tripid catch_draw sf bsb scup cost age tot 
	   compress
	   
	   save  "$calib_catch_data_cd\calib_catch_draws_`s'_`i'.dta", replace
		}
}	

		

	
	
	