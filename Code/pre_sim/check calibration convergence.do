
* This script identifies which runs out of 125 for each state-mode combination convereged in the calibration, 
* meaning that the algorithm was able to tweak the voluntary release/sublegal harvest parameters such that
* simulated total harvest for that state-mode combination was within 5% or within 500 fish of MRIP estimated harvest. 
* In some cases the alogorithm did not properly indicate convergence when it was achieved, so first I identify and 
* account for these cases. Then, I identfy which state-mode combinations did not converge for at least 100 of the 125 draws. 
* NC and VA for-hire converged 93 and 95 times, respectively. To fill in the additionally needed runs, I selected 
* runs that did not converge but were closest in terms of absolute or percent differences in # of harvested fish. 
* The two strata that did not converge accounted for little harvest. 

import excel using "E:\Lou_projects\flukeRDM\flukeRDM_iterative_data\archive\miscellaneous\calibrated_model_stats.xlsx", clear firstrow
drop keep_to_rel* rel_to_keep* p_* prop* n_*
gen abs_diff_keep=abs(diff_keep)
gen abs_diff_pct_keep=abs(pct_diff_keep)

gen sf_converge2=1 if (abs_diff_keep<500 | abs_diff_pct_keep<5) & species=="sf"
gen bsb_converge2=1 if (abs_diff_keep<500 | abs_diff_pct_keep<5) & species=="bsb"
gen scup_converge2=1 if (abs_diff_keep<500 | abs_diff_pct_keep<5) & species=="scup"

egen  sum_sf_converge2= sum(sf_converge2), by(state draw mode)
egen  sum_bsb_converge2= sum(bsb_converge2), by(state draw mode)
egen  sum_scup_converge2= sum(scup_converge2), by(state draw mode)

replace sf_convergence=sum_sf_converge2
replace bsb_convergence=sum_bsb_converge2
replace scup_convergence=sum_scup_converge2

gen all_three=1 if sf_convergence==1 &  bsb_convergence==1 & scup_convergence==1 
gen domain=state+"_"+mode
tempfile calib_reformat
save `calib_reformat', replace

keep mode species state draw all_three sf_convergence bsb_convergence scup_convergence
collapse (sum) all_three, by(state mode draw)
gen tab=1 if all_three==3
tostring draw, gen(draw2)
gen domain=state+"_"+mode+"_"+draw2
collapse (sum) tab, by(domain)
split domain, parse(_)
egen sum_tab=sum(tab), by(domain1 domain2)
keep if sum_tab<100
keep if tab==0
keep domain

tempfile fail
save `fail', replace

u `calib_reformat', clear 
drop domain
tostring draw, gen(draw2)
gen domain=state+"_"+mode+"_"+draw2
merge m:1 domain using `fail'

preserve 
keep if _merge==1
tempfile good
save `good', replace
restore

keep if _merge==3

sort state draw  species mode
order mode species state draw  all_three sf_convergence bsb_convergence scup_convergence MRIP_keep model_keep diff_keep pct_diff_keep  MRIP_catch model_catch diff_catch pct_diff_catch MRIP_rel model_rel diff_rel pct_diff_rel 

egen tot_conv=rowtotal(sf_convergence bsb_convergence scup_convergence)
order mode species state draw  all_three tot_conv sf_convergence bsb_convergence scup_convergence MRIP_keep model_keep diff_keep pct_diff_keep  MRIP_catch model_catch diff_catch pct_diff_catch MRIP_rel model_rel diff_rel pct_diff_rel 

distinct domain if tot_conv<3

*For NC, all non-converge was bsb
browse if state=="NC" & species=="bsb" & tot_conv<3
sort  diff_keep
gen bsb_convergence1=1 if state=="NC" & mode=="fh" & species=="bsb" & tot_conv==2 & pct_diff_keep>-12 

*For NC, most non-converge was sf
browse
sort state draw  species mode
browse if state=="DE" & tot_conv==2 & species=="sf" & sf_convergence==0 
browse if state=="DE" & tot_conv==2 & sf_convergence==0 

sort  diff_keep
gen sf_convergence1=1 if  state=="DE" & mode=="fh" & species=="sf" & tot_conv==2 & sf_convergence==0 & diff_keep>-600
browse if state=="DE" & tot_conv==2 
browse
sort state draw  species mode

egen sum_sf_converge1=sum(sf_convergence1) , by(state draw mode)
egen sum_bsb_converge1=sum(bsb_convergence1) , by(state draw mode)

preserve
keep if sum_sf_converge1==1 
replace sf_convergence=sum_sf_converge1
tempfile reformatsf
save `reformatsf', replace 
restore

preserve
keep if sum_bsb_converge1==1
replace bsb_convergence=sum_bsb_converge1
tempfile reformatbsb
save `reformatbsb', replace 
restore

drop if sum_sf_converge1==1 | sum_bsb_converge1==1
append using `reformatsf'
append using `reformatbsb'

drop sf_converge2 bsb_converge2 scup_converge2 domain _merge bsb_convergence1 sf_convergence1 sum_sf_converge1 sum_bsb_converge1
append using `good'

drop all_three tot_conv draw2 sum_sf_converge2 sum_bsb_converge2 sum_scup_converge2 sf_converge2 bsb_converge2 scup_converge2 _merge
browse
gen all_three=1 if sf_convergence==1 &  bsb_convergence==1 & scup_convergence==1 

/*
 check to make sure we have 100 good run of each state/mode
collapse (sum) all_three, by(state mode draw)
gen tab=1 if all_three==3
collapse (sum) tab, by(state mode)
*/

keep if all_three==1
drop domain
tostring draw, gen(draw2)
gen domain=state+"_"+mode+"_"+draw2
order mode species state draw  domain all_three  sf_convergence bsb_convergence scup_convergence MRIP_keep model_keep diff_keep pct_diff_keep  MRIP_catch model_catch diff_catch pct_diff_catch MRIP_rel model_rel diff_rel pct_diff_rel 
keep  mode species state draw domain
drop species
duplicates drop 
sort state mode  draw
bysort mode state (draw): gen n=_n

*Extra draws for kim
preserve
keep if inlist(state, "RI", "VA", "MD")
keep if n>100 & n<=105
rename n draw2
drop domain
export excel "$iterative_input_data_cd\calibration_good_draws_extras.xlsx", firstrow(variables) replace
restore

keep if n<=100

rename n draw2
drop domain
export excel "$iterative_input_data_cd\calibration_good_draws.xlsx", firstrow(variables) replace










