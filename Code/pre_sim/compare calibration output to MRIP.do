


* First compare copula-simulated mean catch-per-trip to MRIP 
* Estimates were generated at the state, wave, mode level

*A) 
clear
tempfile master
save `master', emptyok

local statez "MA RI CT NY NJ DE MD VA NC"
foreach s of local statez{
forv i=1/$ndraws{

import excel using "$input_data_cd\calib_catch_draws_`s'_`i'.xlsx", clear firstrow
gen sf_cat_sim  = sf_keep_sim + sf_rel_sim
gen bsb_cat_sim = bsb_keep_sim + bsb_rel_sim
gen scup_cat_sim = scup_keep_sim + scup_rel_sim
collapse (mean) sf_keep_sim sf_rel_sim sf_cat_sim bsb_keep_sim bsb_rel_sim bsb_cat_sim scup_keep_sim scup_rel_sim scup_cat_sim, by(my_dom_id_string)
gen draw=`i'

append using `master'
save `master', replace
}
}
use `master', clear
save "$input_data_cd\simulated_means_copula.dta", replace 



u "$input_data_cd\simulated_means_copula.dta", clear 
ds draw my_dom_id, not
local vars `r(varlist)'
foreach v of local vars{
	mvencode `v', mv(0) override
}

collapse (mean) sf_keep_sim sf_rel_sim sf_cat_sim bsb_keep_sim bsb_rel_sim bsb_cat_sim scup_keep_sim scup_rel_sim scup_cat_sim 	///
						(sd) sd_sf_keep_sim=sf_keep_sim sd_sf_cat_sim=sf_cat_sim sd_sf_rel_sim=sf_rel_sim ///
						sd_bsb_keep_sim=bsb_keep_sim sd_bsb_rel_sim=bsb_rel_sim sd_bsb_cat_sim=bsb_cat_sim ///
						sd_scup_keep_sim=scup_keep_sim sd_scup_rel_sim=scup_rel_sim sd_scup_cat_sim=scup_cat_sim, by(my)
						
renvarlab sf* bsb* scup*, prefix(tot_)					
split my, parse(_)
rename my_dom_id_string1 state
rename my_dom_id_string2 wave
rename my_dom_id_string3 mode
drop my_dom_id_string4

reshape long tot_ sd_, i(state wave mode) j(new) string
rename tot_ sim_total 
rename sd_ sim_sd
split new, parse(_)
rename new1 species
rename new2 disp
drop new3
drop new
tempfile sim
save `sim', replace

*Pull in the MRIP means/SEs dataset
import excel using "$input_data_cd\baseline_mrip_catch_processed.xlsx", clear first 
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

*Join simulated means to MRIP means
merge 1:1 state wave mode my species disp using `sim'
browse if _merge==1
browse

gen mrip_ul=mrip_total+1.96*mrip_sd
gen mrip_ll=mrip_total-1.96*mrip_sd
gen sim_ul=sim_total+1.96*sim_sd
gen sim_ll=sim_total-1.96*sim_sd

drop if mrip_total==0 & sim_total==0
drop if disp=="cat"

gen domain=species+"_"+disp

gen pct_diff = ((sim_total-mrip_total)/mrip_total)*100
gen diff= sim_total-mrip_total
sort pct_diff
sort my_dom

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
			title("`d'", size(medium)) name(`d'_`s', replace) 
		}
  }
  
u `new', clear 
sort state wave mode 
grc1leg  bsb_keep_MA sf_keep_MA  scup_keep_MA bsb_rel_MA sf_rel_MA   scup_rel_MA , cols(3) title("Mean catch-per-trip, MRIP vs. copula estimates MA", size(small))
graph export "$figure_cd/mean_catch_MRIP_copula_MA.png", as(png) replace

grc1leg  bsb_keep_RI sf_keep_RI scup_keep_RI  bsb_rel_RI sf_rel_RI scup_rel_RI    , cols(3)	title("Mean catch-per-trip, MRIP vs. copula estimates RI", size(small))	
graph export "$figure_cd/mean_catch_MRIP_copula_RI.png", as(png) replace
		
grc1leg  bsb_keep_CT sf_keep_CT scup_keep_CT  bsb_rel_CT sf_rel_CT  scup_rel_CT  , cols(3) title("Mean catch-per-trip, MRIP vs. copula estimates CT", size(small))		
graph export "$figure_cd/mean_catch_MRIP_copula_CT.png", as(png) replace
		
grc1leg  bsb_keep_NY sf_keep_NY scup_keep_NY  bsb_rel_NY sf_rel_NY  scup_rel_NY  , cols(3) title("Mean catch-per-trip, MRIP vs. copula estimates NY", size(small))	
graph export "$figure_cd/mean_catch_MRIP_copula_NY.png", as(png) replace

grc1leg  bsb_keep_NJ sf_keep_NJ scup_keep_NJ  bsb_rel_NJ sf_rel_NJ  scup_rel_NJ  , cols(3) title("Mean catch-per-trip, MRIP vs. copula estimates NJ", size(small))
graph export "$figure_cd/mean_catch_MRIP_copula_NJ.png", as(png) replace

grc1leg  bsb_keep_DE sf_keep_DE scup_keep_DE  bsb_rel_DE sf_rel_DE  scup_rel_DE  , cols(3) title("Mean catch-per-trip, MRIP vs. copula estimates DE", size(small))
graph export "$figure_cd/mean_catch_MRIP_copula_DEA.png", as(png) replace

grc1leg  bsb_keep_MD sf_keep_MD scup_keep_MD  bsb_rel_MD sf_rel_MD  scup_rel_MD  , cols(3) title("Mean catch-per-trip, MRIP vs. copula estimates MD", size(small))	
graph export "$figure_cd/mean_catch_MRIP_copula_MD.png", as(png) replace

grc1leg  bsb_keep_VA sf_keep_VA scup_keep_VA  bsb_rel_VA sf_rel_VA  scup_rel_VA  , cols(3) title("Mean catch-per-trip, MRIP vs. copula estimates VA", size(small))	
graph export "$figure_cd/mean_catch_MRIP_copula_VA.png", as(png) replace

grc1leg  bsb_keep_NC  bsb_rel_NC scup_keep_NC, cols(3) title("Mean catch-per-trip, MRIP vs. copula estimates NC", size(small))
graph export "$figure_cd/mean_catch_MRIP_copula_NC.png", as(png) replace



*B) The copula model data is used to generate daily catch-draw data, so here, I:
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
forv i=1/$ndraws{

*local i=1
*local s="RI"

use "$iterative_input_data_cd\calib_catch_draws_`s'_`i'.dta", clear 

drop if dtrip==0

collapse (mean) sf_keep_sim sf_cat sf_rel_sim bsb_keep_sim bsb_rel_sim bsb_cat scup_keep_sim scup_rel_sim scup_cat, by(my_dom_id_string)
split my_dom_id_string, parse(_)
rename my_dom_id_string1 state
rename my_dom_id_string2 wave
rename my_dom_id_string3 mode
drop my_dom_id_string4

tempfile catch
save `catch', replace 

import delimited using  "$iterative_input_data_cd\directed_trips_calibration_`s'.csv", clear 
drop if dtrip==0

keep if draw==`i'
gen wave="1" if inlist(month, 1,2)
replace wave="2" if inlist(month, 3, 4)
replace wave="3" if inlist(month, 5,6)
replace wave="4" if inlist(month, 7,8)
replace wave="5" if inlist(month, 9,10)
replace wave="6" if inlist(month, 11,12)
collapse (sum) dtrip, by(mode wave state)

merge 1:1 mode wave state using `catch'
drop _merge

rename sf_cat sf_cat_sim
rename bsb_cat bsb_cat_sim
rename scup_cat scup_cat_sim

local vars sf_keep_sim sf_cat_sim sf_rel_sim bsb_keep_sim bsb_rel_sim bsb_cat_sim scup_keep_sim scup_rel_sim scup_cat_sim 
foreach v of local vars{
	gen tot_`v'= dtrip*`v'
	
}

gen draw=`i'

append using `master'
save `master', replace
}
}
use `master', clear

save "$input_data_cd\simulated_catch_totals3.dta", replace 


*B3 compare means @ state, mode, wave level
u "$input_data_cd\simulated_catch_totals3.dta", clear 
rename dtrip tot_dtrip_sim

ds draw mode state wave, not
local vars `r(varlist)'
foreach v of local vars{
	mvencode `v', mv(0) override
}

tostring draw, gen(draw2)
gen domain=state+"_"+mode+"_"+draw2
encode domain, gen(domain2)
encode wave, gen(wave2)
xtset domain2 wave2
tsfill, full

decode domain2, gen(domain3)
split domain3, parse(_)
replace state=domain31
replace mode=domain32
replace draw2=domain33

drop draw draw2  domain domain2 domain3 domain31 domain32
destring domain33, replace
rename domain33 draw
drop wave
decode wave2, gen(wave)
drop wave2

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
tempfile sim
save `sim', replace


import excel using "$input_data_cd\baseline_mrip_catch_processed.xlsx", clear first 
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
u `new', clear 
sort state wave mode 

grc1leg  bsb_keep_MA_new sf_keep_MA_new  scup_keep_MA_new bsb_rel_MA_new sf_rel_MA_new scup_rel_MA_new , cols(3) title("Mean catch-per-trip, MRIP vs. simulated estimates MA", size(small))
graph export "$figure_cd/mean_catch_MRIP_simulated_MA.png", as(png) replace

grc1leg  bsb_keep_RI_new sf_keep_RI_new scup_keep_RI_new  bsb_rel_RI_new sf_rel_RI_new scup_rel_RI_new, cols(3) title("Mean catch-per-trip, MRIP vs. simulated estimates RI", size(small))
graph export "$figure_cd/mean_catch_MRIP_simulated_RI.png", as(png) replace		

grc1leg  bsb_keep_CT_new sf_keep_CT_new scup_keep_CT_new  bsb_rel_CT_new sf_rel_CT_new  scup_rel_CT_new, cols(3)	title("Mean catch-per-trip, MRIP vs. simulated estimates CT", size(small))
graph export "$figure_cd/mean_catch_MRIP_simulated_CT.png", as(png) replace

grc1leg  bsb_keep_NY_new sf_keep_NY_new scup_keep_NY_new  bsb_rel_NY_new sf_rel_NY_new  scup_rel_NY_new  , cols(3) title("Mean catch-per-trip, MRIP vs. simulated estimates NY", size(small))
graph export "$figure_cd/mean_catch_MRIP_simulated_NYA.png", as(png) replace	

grc1leg  bsb_keep_NJ_new sf_keep_NJ_new scup_keep_NJ_new  bsb_rel_NJ_new sf_rel_NJ_new  scup_rel_NJ_new  , cols(3) title("Mean catch-per-trip, MRIP vs. simulated estimates NJ", size(small))
graph export "$figure_cd/mean_catch_MRIP_simulated_NJ.png", as(png) replace

grc1leg  bsb_keep_DE_new sf_keep_DE_new scup_keep_DE_new  bsb_rel_DE_new sf_rel_DE_new  scup_rel_DE_new  , cols(3) title("Mean catch-per-trip, MRIP vs. simulated estimates DE", size(small))
graph export "$figure_cd/mean_catch_MRIP_simulated_DE.png", as(png) replace

grc1leg  bsb_keep_MD_new sf_keep_MD_new scup_keep_MD_new  bsb_rel_MD_new sf_rel_MD_new  scup_rel_MD_new  , cols(3) title("Mean catch-per-trip, MRIP vs. simulated estimates MD", size(small))
graph export "$figure_cd/mean_catch_MRIP_simulated_MD.png", as(png) replace	

grc1leg  bsb_keep_VA_new sf_keep_VA_new scup_keep_VA_new  bsb_rel_VA_new sf_rel_VA_new  scup_rel_VA_new  , cols(3)	 title("Mean catch-per-trip, MRIP vs. simulated estimates VA", size(small))
graph export "$figure_cd/mean_catch_MRIP_simulated_VA.png", as(png) replace

grc1leg  bsb_keep_NC_new    bsb_rel_NC scup_keep_NC_new , cols(3) title("Mean catch-per-trip, MRIP vs. simulated estimates NC", size(small))
graph export "$figure_cd/mean_catch_MRIP_simulated_NC.png", as(png) replace



*B3 compare catch totals @ state, mode, wave level
u "$input_data_cd\simulated_catch_totals3.dta", replace 
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

u  "$input_data_cd\catch_total_calib_mrip_state_mode_wave.dta", clear 
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


u  "$input_data_cd\directed_trip_calib_mrip_state_wave_total.dta", clear 
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

u `new', clear 
grc1leg  dtrip_MA  dtrip_RI dtrip_CT dtrip_NY dtrip_NJ dtrip_DE dtrip_MD dtrip_VA dtrip_NC , cols(3)  title("Directed trips, MRIP vs. simulated estimates", size(small))
graph export "$figure_cd/dtrip_wave_MRIP_simulated_all_states.png", as(png) replace

grc1leg  bsb_catch_MA sf_catch_MA  scup_catch_MA bsb_harvest_MA sf_harvest_MA   scup_harvest_MA , cols(3) title("Catch totals, MRIP vs. simulated estimates MA", size(small))
graph export "$figure_cd/catch_total_wave_MRIP_simulated_MA.png", as(png) replace
	
grc1leg  bsb_catch_RI sf_catch_RI scup_catch_RI  bsb_harvest_RI sf_harvest_RI scup_harvest_RI    , cols(3)	title("Catch totals, MRIP vs. simulated estimates RI", size(small))
graph export "$figure_cd/catch_total_wave_MRIP_simulated_RI.png", as(png) replace
	
grc1leg  bsb_catch_CT sf_catch_CT scup_catch_CT  bsb_harvest_CT sf_harvest_CT  scup_harvest_CT  , cols(3)	title("Catch totals, MRIP vs. simulated estimates CT", size(small))
graph export "$figure_cd/catch_total_wave_MRIP_simulated_CT.png", as(png) replace
	
grc1leg  bsb_catch_NY sf_catch_NY scup_catch_NY  bsb_harvest_NY sf_harvest_NY  scup_harvest_NY  , cols(3) title("Catch totals, MRIP vs. simulated estimates NY", size(small))
graph export "$figure_cd/catch_total_wave_MRIP_simulated_NY.png", as(png) replace
	
grc1leg  bsb_catch_NJ sf_catch_NJ scup_catch_NJ  bsb_harvest_NJ sf_harvest_NJ  scup_harvest_NJ  , cols(3) title("Catch totals, MRIP vs. simulated estimates NJ", size(small))
graph export "$figure_cd/catch_total_wave_MRIP_simulated_NJ.png", as(png) replace
	
grc1leg  bsb_catch_DE sf_catch_DE scup_catch_DE  bsb_harvest_DE sf_harvest_DE  scup_harvest_DE  , cols(3) title("Catch totals, MRIP vs. simulated estimates DE", size(small))
graph export "$figure_cd/catch_total_wave_MRIP_simulated_DE.png", as(png) replace
	
grc1leg  bsb_catch_MD sf_catch_MD scup_catch_MD  bsb_harvest_MD sf_harvest_MD  scup_harvest_MD  , cols(3) title("Catch totals, MRIP vs. simulated estimates MD", size(small))
graph export "$figure_cd/catch_total_wave_MRIP_simulated_MD.png", as(png) replace
	
grc1leg  bsb_catch_VA sf_catch_VA scup_catch_VA  bsb_harvest_VA sf_harvest_VA  scup_harvest_VA  , cols(3)	title("Catch totals, MRIP vs. simulated estimates VA", size(small))
graph export "$figure_cd/catch_total_wave_MRIP_simulated_VA.png", as(png) replace
	
grc1leg  bsb_catch_NC sf_catch_NC scup_catch_NC  bsb_harvest_NC sf_harvest_NC  scup_harvest_NC  , cols(3) title("Catch totals, MRIP vs. simulated estimates NC", size(small))
graph export "$figure_cd/catch_total_wave_MRIP_simulated_NC.png", as(png) replace
	


*B3 compare catch totals @ state and mode
u "$input_data_cd\simulated_catch_totals3.dta", replace 
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

u  "$input_data_cd\catch_total_calib_mrip.dta", clear  
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


u  "$input_data_cd\directed_trip_calib_mrip.dta", clear  
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

u `new', clear 
grc1leg  dtrip_MA  dtrip_RI dtrip_CT dtrip_NY dtrip_NJ dtrip_DE dtrip_MD dtrip_VA dtrip_NC , cols(3)  title("Directed trips, MRIP vs. simulated estimates", size(small))
graph export "$figure_cd/dtrip_state_MRIP_simulated_all_states.png", as(png) replace

grc1leg  bsb_catch_MA sf_catch_MA  scup_catch_MA bsb_harvest_MA sf_harvest_MA   scup_harvest_MA , cols(3) title("Catch totals, MRIP vs. simulated estimates MA", size(small))
graph export "$figure_cd/catch_total_state_MRIP_simulated_MA.png", as(png) replace
	
grc1leg  bsb_catch_RI sf_catch_RI scup_catch_RI  bsb_harvest_RI sf_harvest_RI scup_harvest_RI    , cols(3)	title("Catch totals, MRIP vs. simulated estimates RI", size(small))
graph export "$figure_cd/catch_total_state_MRIP_simulated_RI.png", as(png) replace
	
grc1leg  bsb_catch_CT sf_catch_CT scup_catch_CT  bsb_harvest_CT sf_harvest_CT  scup_harvest_CT  , cols(3)	title("Catch totals, MRIP vs. simulated estimates CT", size(small))
graph export "$figure_cd/catch_total_state_MRIP_simulated_CT.png", as(png) replace
	
grc1leg  bsb_catch_NY sf_catch_NY scup_catch_NY  bsb_harvest_NY sf_harvest_NY  scup_harvest_NY  , cols(3) title("Catch totals, MRIP vs. simulated estimates NY", size(small))
graph export "$figure_cd/catch_total_state_MRIP_simulated_NY.png", as(png) replace
	
grc1leg  bsb_catch_NJ sf_catch_NJ scup_catch_NJ  bsb_harvest_NJ sf_harvest_NJ  scup_harvest_NJ  , cols(3) title("Catch totals, MRIP vs. simulated estimates NJ", size(small))
graph export "$figure_cd/catch_total_state_MRIP_simulated_NJ.png", as(png) replace
	
grc1leg  bsb_catch_DE sf_catch_DE scup_catch_DE  bsb_harvest_DE sf_harvest_DE  scup_harvest_DE  , cols(3) title("Catch totals, MRIP vs. simulated estimates DE", size(small))
graph export "$figure_cd/catch_total_state_MRIP_simulated_DE.png", as(png) replace
	
grc1leg  bsb_catch_MD sf_catch_MD scup_catch_MD  bsb_harvest_MD sf_harvest_MD  scup_harvest_MD  , cols(3) title("Catch totals, MRIP vs. simulated estimates MD", size(small))
graph export "$figure_cd/catch_total_state_MRIP_simulated_MD.png", as(png) replace
	
grc1leg  bsb_catch_VA sf_catch_VA scup_catch_VA  bsb_harvest_VA sf_harvest_VA  scup_harvest_VA  , cols(3)	title("Catch totals, MRIP vs. simulated estimates VA", size(small))
graph export "$figure_cd/catch_total_state_MRIP_simulated_VA.png", as(png) replace
	
grc1leg  bsb_catch_NC sf_catch_NC scup_catch_NC  bsb_harvest_NC sf_harvest_NC  scup_harvest_NC  , cols(3) title("Catch totals, MRIP vs. simulated estimates NC", size(small))
graph export "$figure_cd/catch_total_state_MRIP_simulated_NC.png", as(png) replace
	






** FINAL STEP

* Once the simulated totals approximate MRIP, save the data to be used in the R code simulation
u "$input_data_cd\simulated_catch_totals3.dta", replace 
rename dtrip tot_dtrip_sim

ds draw mode state wave, not
local vars `r(varlist)'
foreach v of local vars{
	mvencode `v', mv(0) override
}
	
collapse (sum) tot_sf_keep_sim tot_sf_cat_sim tot_sf_rel_sim ///
						  tot_bsb_keep_sim tot_bsb_rel_sim tot_bsb_cat_sim ///
						  tot_scup_keep_sim tot_scup_rel_sim tot_scup_cat_sim tot_dtrip_sim , by(state mode draw)
						  
save "$output_data_cd\simulated_catch_totals.dta", replace 
					  




* Remove extraneous columns from the catch-per-trip data
local states 
mata: mata clear
clear

local statez "MA RI CT NY NJ DE MD VA NC"

foreach s of local statez {
	forvalues i = 1/$ndraws {
       u "$input_data_cd\calib_catch_draws_`s'_`i'.dta", clear
	   drop my_dom_id_string day month wave day_i sf_keep_sim sf_rel_sim bsb_keep_sim bsb_rel_sim scup_keep_sim scup_rel_sim dtrip
	   compress
	   save  "$input_data_cd\calib_catch_draws_`s'_`i'.dta", replace
		}
}	

		

	
	
	