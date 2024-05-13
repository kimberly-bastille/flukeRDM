

cd "C:\Users\andrew.carr-harris\Desktop\RDM data and code\length data"
u "fluke_catch_at_length_2022.dta", clear 

*keep if draw==0
keep if state=="VA"

gen mode="pr"

expand 2, gen(dup)
replace mode="fh" if dup==1
drop dup

expand 2 if mode=="fh", gen(dup)
replace mode="sh" if dup==1
drop dup 

keep length state year fitted mode catch_at_length 
tempfile base 
save `base', replace 

tempfile base 
save `base', replace 

global draws
forv d=1/100{
	
u `base', clear 	
preserve 
import delimited using "pstar_VA_test1.csv", clear 
replace p_star="." if p_star=="NA"
destring p_star, replace
replace p_star=1 if p_star>1 | p_star==.

drop v1

keep if species=="SF"
keep if run_num==`d'

su p_star if mode=="sh"
local p_star_sh=`r(mean)'

su p_star if mode=="fh"
local p_star_fh=`r(mean)'

su p_star if mode=="pr"
local p_star_pr=`r(mean)'
restore

*scatter fitted_prob length , connect(direct) lcol(black)   lwidth(medthick)  lpat(solid) msymbol(i) $graphoptions 
gen p_star=`p_star_pr' if length== 15.5 & mode=="pr"
replace p_star=`p_star_fh' if  length== 15.5 & mode=="fh"
replace p_star= `p_star_sh' if  length== 15.5 & mode=="sh"




gen below=.
gen above=.
levelsof mode, local(sts)
foreach s of local sts{
	su length if p_star!=. & mode=="`s'"
	replace below=1 if length<= `r(max)' & mode=="`s'"
	replace above=1 if length> `r(max)' & mode=="`s'"
}


egen sum_below=sum(fitted_prob), by(mode below)
replace sum_below=. if below==.

egen sum_above=sum(fitted_prob), by(mode above)
replace sum_above=. if above==.


bysort mode (leng): gen first=1 if _n==1
gen cdf_star=fitted_prob if first==1
egen pstar_all= sum(p_star), by(mode)

*check
gen prob_below_adj=fitted_prob/sum_below if below==1 
gen prob_above_adj=fitted_prob/sum_above if above==1 
mvencode prob_below_adj prob_above_adj, mv(0) overr

*gen F_l = p_star if p_star!=.
gen f_l=prob_below_adj *pstar_all if below==1 
replace f_l=prob_above_adj *(1-pstar_all) if above==1  

drop prob_above_adj prob_below_adj pstar_all cdf_star first sum_above sum_below above below p_star fitted_prob


gen draw=`d'

rename f_l fitted_prob

	tempfile draws`d'
	save `draws`d'', replace
	global draws "$draws "`draws`d''" " 
	
}

dsconcat $draws

egen sum_catch=sum(catch_at_length), by(mode draw)



twoway 	(scatter fitted_prob length if mode=="pr" & draw==1, connect(direct) lcol(black)  lwidth(vthin)  lpat(solid) msymbol(i) $graphoptions) ///
				(scatter fitted_prob length if mode=="pr" & draw==10, connect(direct) lcol(black)  lwidth(vthin)  lpat(solid) msymbol(i) $graphoptions)  ///
				(scatter fitted_prob length if mode=="pr" & draw==20, connect(direct) lcol(black)  lwidth(vthin)  lpat(solid) msymbol(i) $graphoptions) ///
				(scatter fitted_prob length if mode=="pr" & draw==30, connect(direct) lcol(black)  lwidth(vthin)  lpat(solid) msymbol(i) $graphoptions) ///
				(scatter fitted_prob length if mode=="pr" & draw==40, connect(direct) lcol(black)  lwidth(vthin)  lpat(solid) msymbol(i) $graphoptions) 

save "fluke_prob_star_2022_VA.dta", replace





*BSB
u "bsb_projected_catch_at_lengths.dta", clear 
keep if state=="VA"

gen mode="pr"

expand 2, gen(dup)
replace mode="fh" if dup==1
drop dup

expand 2 if mode=="fh", gen(dup)
replace mode="sh" if dup==1
drop dup 


tempfile base 
save `base', replace 

global draws
forv d=1/100{
	
u `base', clear 	
preserve 
import delimited using "pstar_VA_test1.csv", clear 
drop v1
replace p_star="." if p_star=="NA"
destring p_star, replace
replace p_star=1 if p_star>1 | p_star==.

keep if species=="BSB"
keep if run_num==`d'

su p_star if mode=="sh"
local p_star_sh=`r(mean)'

su p_star if mode=="fh"
local p_star_fh=`r(mean)'

su p_star if mode=="pr"
local p_star_pr=`r(mean)'
restore

*scatter fitted_prob length , connect(direct) lcol(black)   lwidth(medthick)  lpat(solid) msymbol(i) $graphoptions 
gen p_star=`p_star_pr' if length== 12.5 & mode=="pr"
replace p_star=`p_star_fh' if  length== 12.5 & mode=="fh"
replace p_star= `p_star_sh' if  length== 12.5 & mode=="sh"



gen below=.
gen above=.
levelsof mode, local(sts)
foreach s of local sts{
	su length if p_star!=. & mode=="`s'"
	replace below=1 if length<= `r(max)' & mode=="`s'"
	replace above=1 if length> `r(max)' & mode=="`s'"
}


egen sum_below=sum(fitted_prob), by(mode below)
replace sum_below=. if below==.

egen sum_above=sum(fitted_prob), by(mode above)
replace sum_above=. if above==.


bysort mode (leng): gen first=1 if _n==1
gen cdf_star=fitted_prob if first==1
egen pstar_all= sum(p_star), by(mode)

*check
gen prob_below_adj=fitted_prob/sum_below if below==1 
gen prob_above_adj=fitted_prob/sum_above if above==1 
mvencode prob_below_adj prob_above_adj, mv(0) overr

*gen F_l = p_star if p_star!=.
gen f_l=prob_below_adj *pstar_all if below==1 
replace f_l=prob_above_adj *(1-pstar_all) if above==1  

drop prob_above_adj prob_below_adj pstar_all cdf_star first sum_above sum_below above below p_star fitted_prob


gen draw=`d'

rename f_l fitted_prob

tempfile draws`d'
save `draws`d'', replace
global draws "$draws "`draws`d''" " 

	
}

dsconcat $draws

twoway 	(scatter fitted_prob length if mode=="pr" & draw==1, connect(direct) lcol(black)  lwidth(vthin)  lpat(solid) msymbol(i) $graphoptions) ///
				(scatter fitted_prob length if mode=="pr" & draw==10, connect(direct) lcol(black)  lwidth(vthin)  lpat(solid) msymbol(i) $graphoptions)  ///
				(scatter fitted_prob length if mode=="pr" & draw==20, connect(direct) lcol(black)  lwidth(vthin)  lpat(solid) msymbol(i) $graphoptions) ///
				(scatter fitted_prob length if mode=="pr" & draw==30, connect(direct) lcol(black)  lwidth(vthin)  lpat(solid) msymbol(i) $graphoptions) ///
				(scatter fitted_prob length if mode=="pr" & draw==40, connect(direct) lcol(black)  lwidth(vthin)  lpat(solid) msymbol(i) $graphoptions) 

				
save "bsb_prob_star_2022_VA.dta", replace






*Scup
u "scup_catch_at_length_2022.dta", clear 
keep if state=="VA"

gen mode="pr"

expand 2, gen(dup)
replace mode="fh" if dup==1
drop dup

expand 2 if mode=="fh", gen(dup)
replace mode="sh" if dup==1
drop dup 


tempfile base 
save `base', replace 

global draws
forv d=1/100{
	
u `base', clear 	
preserve 
import delimited using "pstar_VA_test1.csv", clear 
drop v1
replace p_star="." if p_star=="NA"
destring p_star, replace
replace p_star=1 if p_star>1 | p_star==.

keep if species=="SCUP"
keep if run_num==`d'

su p_star if mode=="sh"
local p_star_sh=`r(mean)'

su p_star if mode=="fh"
local p_star_fh=`r(mean)'

su p_star if mode=="pr"
local p_star_pr=`r(mean)'
restore

*scatter fitted_prob length , connect(direct) lcol(black)   lwidth(medthick)  lpat(solid) msymbol(i) $graphoptions 
gen p_star=`p_star_pr' if length== 8.5 & mode=="pr"
replace p_star=`p_star_fh' if  length== 8.5 & mode=="fh"
replace p_star= `p_star_sh' if  length== 8.5 & mode=="sh"


gen below=.
gen above=.
levelsof mode, local(sts)
foreach s of local sts{
	su length if p_star!=. & mode=="`s'"
	replace below=1 if length<= `r(max)' & mode=="`s'"
	replace above=1 if length> `r(max)' & mode=="`s'"
}


egen sum_below=sum(fitted_prob), by(mode below)
replace sum_below=. if below==.

egen sum_above=sum(fitted_prob), by(mode above)
replace sum_above=. if above==.


bysort mode (leng): gen first=1 if _n==1
gen cdf_star=fitted_prob if first==1
egen pstar_all= sum(p_star), by(mode)

*check
gen prob_below_adj=fitted_prob/sum_below if below==1 
gen prob_above_adj=fitted_prob/sum_above if above==1 
mvencode prob_below_adj prob_above_adj, mv(0) overr

*gen F_l = p_star if p_star!=.
gen f_l=prob_below_adj *pstar_all if below==1 
replace f_l=prob_above_adj *(1-pstar_all) if above==1  

drop prob_above_adj prob_below_adj pstar_all cdf_star first sum_above sum_below above below p_star fitted_prob


gen draw=`d'

rename f_l fitted_prob

	tempfile draws`d'
	save `draws`d'', replace
	global draws "$draws "`draws`d''" " 
	
}

dsconcat $draws


twoway 	(scatter fitted_prob length if mode=="fh" & draw==1, connect(direct) lcol(black)  lwidth(vthin)  lpat(solid) msymbol(i) $graphoptions) ///
				(scatter fitted_prob length if mode=="fh" & draw==10, connect(direct) lcol(black)  lwidth(vthin)  lpat(solid) msymbol(i) $graphoptions)  ///
				(scatter fitted_prob length if mode=="fh" & draw==20, connect(direct) lcol(black)  lwidth(vthin)  lpat(solid) msymbol(i) $graphoptions) ///
				(scatter fitted_prob length if mode=="fh" & draw==30, connect(direct) lcol(black)  lwidth(vthin)  lpat(solid) msymbol(i) $graphoptions) ///
				(scatter fitted_prob length if mode=="fh" & draw==40, connect(direct) lcol(black)  lwidth(vthin)  lpat(solid) msymbol(i) $graphoptions) 

				
save "scup_prob_star_2022_VA.dta", replace

