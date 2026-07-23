

/*******************************************************************************
 Script:       seasonal mean catch per -trips.do
 Purpose:      Diagnostic box plots of simulated projected catch per trip for
               Massachusetts, one panel per wave, with private, for-hire and
               shore modes side by side. Used to eyeball whether the copula
               simulation produces sensible seasonal and by-mode patterns
               before the draws are carried into the projection.
 Inputs:       simulated_projected_means_copula.dta
 Outputs:      None. Graphs are drawn to screen and not exported.
 Dependencies: Global $input_data_cd. Interactive - run in a Stata session
               where the graph windows are visible.
 Pipeline:     Development/QA scratch. Not called by any wrapper.

 Note: only summer flounder and black sea bass are plotted; scup is kept in
 the data but has no graph command. The trailing fragment at the end of the
 file (title and ytitle options with no graph command in front of them) is an
 unfinished third block, left over from copying the pattern for a scup or
 regional plot. It is inert as written.
*******************************************************************************/

u "$input_data_cd\simulated_projected_means_copula.dta", clear

/* The domain id is a single string packing state, wave, mode and a fourth
   field together, e.g. "MA_4_pr_...". split unpacks it into components; the
   fourth is not needed and is dropped. */
split my, parse(_)
drop my_dom_id_string4
rename my_dom_id_string1 state
rename my_dom_id_string2 wave
rename my_dom_id_string3 mode

encode wave, gen(wave2)


keep sf_cat_sim bsb_cat_sim scup_cat_sim draw state wave mode wave2
replace mode="_"+mode
reshape wide sf bsb scup, i(draw state wave wave2) j(mode) string

/* Zeros are recoded to missing so they drop out of the box plots. A zero here
   means the state x wave x mode stratum was not sampled or not simulated, not
   that anglers caught nothing - including them would drag the boxes toward
   zero and misrepresent the distribution among trips that actually occurred. */
ds draw state wave wave2, not
local vars "`r(varlist)'"
foreach v of local vars{
	replace `v'=. if `v'==0
}

graph box sf_cat_sim_pr sf_cat_sim_fh sf_cat_sim_sh if state=="MA" , ///
    over(wave, label(labsize(small))) ///
    box(1, fcolor(navy)   lcolor(navy)) ///
    box(2, fcolor(maroon) lcolor(maroon)) ///
	box(3, fcolor(gray) lcolor(gray)) ///
    marker(1, msymbol(O) msize(tiny) mcolor(navy)) ///
    marker(2, msymbol(O) msize(tiny) mcolor(maroon)) ///
	marker(3, msymbol(O) msize(tiny) mcolor(gray)) ///
    legend(order(1 "Private" 2 "For-hire" 3 "Shore") position(6) cols(3)) /// 
	ylab(, labsize(small))

	
	
graph box bsb_cat_sim_pr bsb_cat_sim_fh bsb_cat_sim_sh if state=="MA" , ///
    over(wave, label(labsize(small))) ///
    box(1, fcolor(navy)   lcolor(navy)) ///
    box(2, fcolor(maroon) lcolor(maroon)) ///
	box(3, fcolor(gray) lcolor(gray)) ///
    marker(1, msymbol(O) msize(tiny) mcolor(navy)) ///
    marker(2, msymbol(O) msize(tiny) mcolor(maroon)) ///
	marker(3, msymbol(O) msize(tiny) mcolor(gray)) ///
    legend(order(1 "Private" 2 "For-hire" 3 "Shore") position(6) cols(3)) /// 
	ylab(, labsize(small))
	
	
	///
    ///
	
	///
    title("Black sea bass North", size(medium)) ///
	ytitle(Proportion of fish that are length-{it:l}) ///



