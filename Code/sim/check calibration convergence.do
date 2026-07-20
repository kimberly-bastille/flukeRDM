
/*******************************************************************************
 Script:       check calibration convergence.do
 Purpose:      Selects the 100 usable calibration draws per state x mode out of
               the 125 that were generated, and writes that selection out for
               the projection stage to consume. A draw counts as converged for
               a species when simulated harvest lands within 500 fish or 5% of
               the MRIP estimate. Two corrections are applied on top of the
               recorded convergence flags: draws that met the criterion but
               were not flagged as converged are reinstated, and for the two
               state x mode strata that still fall short (NC for-hire and VA
               for-hire, at 93 and 95 converged draws) the nearest-miss draws
               are admitted to reach the required count. Those two strata
               account for very little harvest, which is the stated
               justification for admitting them.
 Inputs:       calibrated_model_stats_4_16_26.csv. Two earlier vintages are
               also named in the script - calibrated_model_stats.xlsx and
               calibrated_model_stats_4_2_26.xlsx - but each import carries
               `clear', so only the last one survives. See the note below.
 Outputs:      calibration_good_draws.xlsx, calibration_good_draws_extras.xlsx
 Dependencies: Global $iterative_input_data_cd for the export paths. The three
               import paths are hardcoded absolute paths on a single
               developer machine (E: drive) and must be edited to run
               elsewhere. Requires the
               calibration round (calibration_routine_final.R) to have already
               produced the model stats file.
 Dev paths:    3 hardcoded absolute paths to a developer's local machine
               (E:\), at lines 59-61 (the three `import' statements above).
 Pipeline:     Manual QA step between the R calibration stage and the
               projection stage. Not called by model_wrapper.do or by
               "R code wrapper.R" - it is run by hand.

 IMPORTANT - this is an interactive script, not a batch step:
               It contains bare `browse' commands (lines below), which open
               the Stata data editor and halt in a non-interactive session. It
               is written to be stepped through by a person inspecting results,
               and the hardcoded thresholds in Section C were arrived at by
               looking at the data in exactly that way. Do not expect it to run
               unattended.
*******************************************************************************/

* This script identifies which runs out of 125 for each state-mode combination convereged in the calibration,
* meaning that the algorithm was able to tweak the voluntary release/sublegal harvest parameters such that
* simulated total harvest for that state-mode combination was within 5% or within 500 fish of MRIP estimated harvest. 
* In some cases the alogorithm did not properly indicate convergence when it was achieved, so first I identify and 
* account for these cases. Then, I identfy which state-mode combinations did not converge for at least 100 of the 125 draws. 
* NC and VA for-hire converged 93 and 95 times, respectively. To fill in the additionally needed runs, I selected 
* runs that did not converge but were closest in terms of absolute or percent differences in # of harvested fish. 
* The two strata that did not converge accounted for little harvest. 

/**************************************************/
/**************************************************/
/* Section A: Load stats and recompute convergence */
/**************************************************/
/**************************************************/

/* Three import statements in a row, each with `clear'. Only the last one
   survives - the first two vintages of the stats file are discarded
   immediately. They are kept here as a record of which files were used in
   earlier passes; to work from an older vintage, comment out the later lines. */
import excel using "E:\Lou_projects\flukeRDM\flukeRDM_iterative_data\archive\miscellaneous\calibrated_model_stats.xlsx", clear firstrow
import excel using "E:\Lou_projects\flukeRDM\flukeRDM_iterative_data\calibrated_model_stats_4_2_26.xlsx", clear firstrow
import delimited using "E:\Lou_projects\flukeRDM\flukeRDM_iterative_data\archive\miscellaneous\calibrated_model_stats_4_16_26.csv", clear

*drop keep_to_rel* rel_to_keep* p_* 
replace pct_diff_catch="0" if pct_diff_catch=="NA"
replace pct_diff_keep="0" if pct_diff_keep=="NA"
replace pct_diff_rel="0" if pct_diff_rel=="NA"
destring pct_diff_catch, replace
destring pct_diff_keep, replace
destring pct_diff_rel, replace


gen abs_diff_keep=abs(diff_keep)
gen abs_diff_pct_keep=abs(pct_diff_keep)

/* The convergence criterion: harvest is close enough if it is within 500 fish
   OR within 5% of the MRIP estimate. The OR matters - the absolute tolerance
   is what lets small strata pass, where a 5% relative tolerance would be only
   a handful of fish. These flags are recomputed here rather than trusted from
   the calibration run because the algorithm sometimes failed to record
   convergence it had actually achieved. */
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

/**************************************************/
/**************************************************/
/* Section B: Identify strata short of 100 draws  */
/**************************************************/
/**************************************************/

/* A draw is usable only if all three species converged on it (tab==1 below,
   from all_three==3). Strata with fewer than 100 such draws are collected into
   `fail' so Section C can hand-pick replacements for them. */
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

/**************************************************/
/**************************************************/
/* Section C: Hand-admit the nearest-miss draws   */
/**************************************************/
/**************************************************/

/* The two thresholds below (-12 percent for NC black sea bass, -600 fish for
   DE summer flounder) are not derived from any rule. They were read off the
   sorted data in the `browse' windows above: after sorting by diff_keep, they
   are the cutoffs that admit just enough near-miss draws to reach 100 for
   those strata without reaching further into badly-fitting draws. Re-deriving
   them on a new stats file means repeating that manual inspection. */
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

/**************************************************/
/**************************************************/
/* Section D: Export the selected draw list       */
/**************************************************/
/**************************************************/

/* `n' is the within-stratum sequence number after sorting by draw, so the
   selection below is "the first 100 usable draws per state x mode", with the
   original draw ids renamed to draw2 for the projection stage to key on.
   The extras file holds draws 101-105 for three states only - a side request,
   not part of the standard 100-draw set. */
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










