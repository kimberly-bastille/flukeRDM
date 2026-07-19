

/*******************************************************************************
 Script:       compare_projection_data_to_MRIP.do
 Purpose:      Validation and cleanup step for the projection catch draws.
               Computes mean catch-per-trip from the simulated daily draws,
               plots those means against the MRIP survey estimates with 95%
               confidence intervals so the two can be compared by eye, then
               strips the columns the R projection stage does not need and
               rewrites the draw files in place.
 Inputs:       proj_catch_draws_<ST>_<i>.dta,
               projected_mrip_catch_processed.xlsx
 Outputs:      simulated_projected_catch_means3.dta,
               mean_proj_catch_MRIP_simulated_<ST>.png (one per state),
               and proj_catch_draws_<ST>_<i>.dta overwritten with a reduced
               column set.
 Dependencies: Globals $misc_data_cd, $proj_catch_data_cd, $figure_cd and
               $ndraws. Requires catch_per_trip_projection_part2.do to have
               run. Uses the user-written commands renvarlab and grc1leg.
 Pipeline:     Step 13d of model_wrapper.do, the last script inside the
               `catch_per_trip_project' meta-toggle. Its calibration analog is
               compare_calibration_data_to_MRIP.do, which is substantially
               longer. The column-stripping in Step 3 is what the R projection
               stage actually depends on; the figures are for human review.

 IMPORTANT - two behaviors to know before running:
   1. Step 3 OVERWRITES the proj_catch_draws files in place with a reduced
      column set. It is therefore not safe to re-run this script twice without
      re-running catch_per_trip_projection_part2.do first - the second pass
      would try to drop columns that no longer exist.
   2. The script contains bare `browse' commands in Step 2, which open the
      data editor and halt in a non-interactive session.
*******************************************************************************/

display "compare_projection_data_to_MRIP.do: computing simulated catch-per-trip means, comparing to MRIP, and reducing the projection draw files. This reads every proj_catch_draws file twice and may take a long time."

*The copula model data is used to generate daily catch-draw data, so here I:
		*1) compute mean catch-per-trip from the daily catch-draw data
		*2) compare catch-per-trip means from 1) with estimates from MRIP
		*3) remove extranneous information and save dataset for R projection code. 
		

/**************************************************/
/**************************************************/
/* Section A: Mean catch-per-trip from the draws  */
/**************************************************/
/**************************************************/

* Step 1)
clear
tempfile master
save `master', emptyok

local statez "MA RI CT NY NJ DE MD VA NC"

foreach s of local statez{
forv i=1/$ndraws{

*local i=1
*local s="RI"

	use "$proj_catch_data_cd\proj_catch_draws_`s'_`i'.dta", clear 

	collapse (mean) sf_keep sf_cat sf_rel bsb_keep bsb_rel bsb_cat scup_keep scup_rel scup_cat, by(state wave mode)

	local vars sf_keep sf_cat sf_rel bsb_keep bsb_rel bsb_cat scup_keep scup_rel scup_cat
	foreach v of local vars{
	rename `v' `v'_sim
	}

	tempfile catch
	save `catch', replace 

	gen draw=`i'

	append using `master'
	save `master', replace
}
}
use `master', clear

save "$misc_data_cd\simulated_projected_catch_means3.dta", replace 


/**************************************************/
/**************************************************/
/* Section B: Compare to MRIP and plot            */
/**************************************************/
/**************************************************/

/* The xtset/tsfill block below fills in state x mode x draw x wave cells that
   never appeared in the simulation - a stratum with no directed trips in a
   given wave produces no rows at all, but the comparison needs it present as
   a zero rather than absent. Treating the domain as a panel id and the wave as
   the time index is what lets tsfill, full manufacture those rows; the
   subsequent split/replace recovers state, mode and draw from the packed
   domain string because tsfill leaves them missing on the new rows. The two
   mvencode passes turn the resulting missings into zeros. */
* Step 2)
u "$misc_data_cd\simulated_projected_catch_means3.dta", clear

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
tempfile sim
save `sim', replace

import excel using "$misc_data_cd\projected_mrip_catch_processed.xlsx", clear first 
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
destring wave, replace 
merge 1:1 state wave mode  species disp using `sim'
browse if _merge==2
browse

/* 1.96 is the normal 95% critical value. The MRIP interval uses the survey
   standard error, so it expresses sampling uncertainty in the survey estimate;
   the simulated interval uses the standard deviation ACROSS DRAWS, so it
   expresses uncertainty introduced by the model's own draws. The two bars in
   the plots below therefore are not the same quantity - overlapping intervals
   indicate the simulation is consistent with MRIP, not that they agree. */
gen mrip_ul=mrip_total+1.96*mrip_sd
gen mrip_ll=mrip_total-1.96*mrip_sd
gen sim_ul=sim_total+1.96*sim_sd
gen sim_ll=sim_total-1.96*sim_sd

drop if mrip_total==0 & sim_total==0
keep if disp=="cat"
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

grc1leg  sf_cat_MA_new bsb_cat_MA_new  scup_cat_MA_new  , cols(3) title("Mean catch-per-trip, MRIP vs. simulated estimates MA", size(small))
graph export "$figure_cd/mean_proj_catch_MRIP_simulated_MA.png", as(png) replace

grc1leg  sf_cat_RI_new bsb_cat_RI_new  scup_cat_RI_new , cols(3) title("Mean catch-per-trip, MRIP vs. simulated estimates RI", size(small))
graph export "$figure_cd/mean_proj_catch_MRIP_simulated_RI.png", as(png) replace		

grc1leg  sf_cat_CT_new bsb_cat_CT_new  scup_cat_CT_new , cols(3)	title("Mean catch-per-trip, MRIP vs. simulated estimates CT", size(small))
graph export "$figure_cd/mean_proj_catch_MRIP_simulated_CT.png", as(png) replace

grc1leg  sf_cat_NY_new bsb_cat_NY_new  scup_cat_NY_new   , cols(3) title("Mean catch-per-trip, MRIP vs. simulated estimates NY", size(small))
graph export "$figure_cd/mean_proj_catch_MRIP_simulated_NY.png", as(png) replace	

grc1leg  sf_cat_NJ_new bsb_cat_NJ_new  scup_cat_NJ_new  , cols(3) title("Mean catch-per-trip, MRIP vs. simulated estimates NJ", size(small))
graph export "$figure_cd/mean_proj_catch_MRIP_simulated_NJ.png", as(png) replace

grc1leg  sf_cat_DE_new bsb_cat_DE_new  scup_cat_DE_new  , cols(3) title("Mean catch-per-trip, MRIP vs. simulated estimates DE", size(small))
graph export "$figure_cd/mean_proj_catch_MRIP_simulated_DE.png", as(png) replace

grc1leg  sf_cat_MD_new bsb_cat_MD_new  scup_cat_MD_new  , cols(3) title("Mean catch-per-trip, MRIP vs. simulated estimates MD", size(small))
graph export "$figure_cd/mean_proj_catch_MRIP_simulated_MD.png", as(png) replace	

grc1leg  sf_cat_VA_new bsb_cat_VA_new  scup_cat_VA_new   , cols(3)	 title("Mean catch-per-trip, MRIP vs. simulated estimates VA", size(small))
graph export "$figure_cd/mean_proj_catch_MRIP_simulated_VA.png", as(png) replace

/* BUG, flagged not fixed: the NC panel below combines sf_cat_NC_new with
   bsb_cat_VA_new and scup_cat_VA_new. Two of the three sub-plots in
   mean_proj_catch_MRIP_simulated_NC.png therefore show VIRGINIA black sea
   bass and scup, not North Carolina, while the title says NC. This is a
   copy-paste artifact from the VA block just above. Anyone who has used that
   figure to judge NC black sea bass or scup fit was reading VA data. */
grc1leg sf_cat_NC_new bsb_cat_VA_new  scup_cat_VA_new  , cols(3) title("Mean catch-per-trip, MRIP vs. simulated estimates NC", size(small))
graph export "$figure_cd/mean_proj_catch_MRIP_simulated_NC.png", as(png) replace



/**************************************************/
/**************************************************/
/* Section C: Strip unused columns from the draws */
/**************************************************/
/**************************************************/

/* This is the step the R projection stage actually depends on. The keep/rel
   columns and the calendar helpers are dropped because the projection
   recomputes them from catch totals under the alternative regulations - a
   projection that inherited the calibration-year keep/release split would be
   assuming the answer. Files are rewritten in place; see the re-run warning
   in the header. */
display "compare_projection_data_to_MRIP.do: reducing proj_catch_draws files in place (Step 3)."

* Step 3)
clear
mata: mata clear

local statez "MA RI CT NY NJ DE MD VA NC"

foreach s of local statez {
	forvalues i = 1/$ndraws{
		use "$proj_catch_data_cd\proj_catch_draws_`s'_`i'.dta", clear 
	    drop  day month wave day_i sf_keep sf_rel bsb_keep bsb_rel scup_keep scup_rel draw
	    compress
		save  "$proj_catch_data_cd\proj_catch_draws_`s'_`i'.dta", replace 
		}
}	
	

display "compare_projection_data_to_MRIP.do: finished."
