/*******************************************************************************
 Script:       directed_trips_calibration.do
 Purpose:      Estimates recreational fishing EFFORT for the calibration year
               and turns it into the day-by-day trip calendar the rest of the
               pipeline simulates over. Five steps, as the source comment
               below lists: estimate directed trips and their standard errors
               by stratum from MRIP; draw $ndraws random realizations of
               directed trips per stratum from those estimates, so that effort
               uncertainty propagates into the results; divide each draw by
               the number of days in the stratum to get trips per day; compute
               a calendar adjustment factor reconciling the differing day
               counts of the calibration and projection years; and attach the
               regulations in force on each day.

               "Directed trips" means trips that targeted or caught one of the
               three managed species - the denominator the whole model works
               in. A stratum here is state x wave x mode x kind-of-day.
 Inputs:       The MRIP trip and catch files named by $triplist and $catchlist.
 Outputs:      directed_trips_calibration_<ST>.csv,
               directed_trips_imputations_<ST>.dta,
               proj_year_calendar_adjustments_<ST>.csv,
               and the MRIP directed-trip totals at four levels of
               aggregation, which compare_calibration_data_to_MRIP.do later
               validates against:
               mrip_dtrip_calib_state.dta,
               mrip_dtrip_calib_state_mode.dta,
               mrip_dtrip_calib_state_mode_wave.dta,
               mrip_dtrip_calib_state_month.dta
 Dependencies: Globals $triplist, $catchlist, $misc_data_cd, $input_data_cd,
               $ndraws, $seed, $calibration_year, $calibration_start_date,
               $calibration_end_date, $projection_date_start,
               $projection_date_end, $fed_holidays, $fed_holidays_y2 and
               $leap_yr_days. Requires MRIP_lists.do to have run first, since
               that is the only source of $triplist and $catchlist. Uses the
               user-written commands dsconcat and gammafit.
 Pipeline:     Step 2 of model_wrapper.do, gated by the toggle estimate_dtrips.
               The longest script in the repo. Its output is the calendar that
               every later stage expands into simulated trips.

 CALLS set_regulations.do UNCONDITIONALLY, once per state, from inside this
 script's state loop. That file is not listed in model_wrapper.do and has no
 toggle of its own, so it runs whenever this one does. It holds the
 hand-entered season dates and bag/size limits and MUST BE UPDATED EVERY YEAR.
 See its header - including the requirement that only one state be in memory
 when it runs, which is why it is called from inside the loop rather than once
 at the end.

 Kind-of-day: MRIP treats federal holidays as weekend days for effort
 estimation, which is why $fed_holidays exists and why effort is estimated at
 the month x weekend/weekday level rather than by calendar date.
*******************************************************************************/

display "directed_trips_calibration.do: estimating directed trips from MRIP and drawing $ndraws effort realizations per stratum across 9 states. This is the longest step in the Stata pipeline."



/*This code uses the MRIP data to 
	1) estimate dirrected trips and their standard error during the calibration period, 
	2) use those paramters to create random draws of directed trips for each stratum,
	3) divide each random draw by the number of days in that stratum to obtain an estimate of trips-per-day, 
	4) compute for each stratum a calendar year adjustment = (# of calendar days in that stratum for the projection period)/(# of calendar days in that stratum for the calibration period). 
		We will multiply projection results by this scalar to account for differences in the number of calibration- versus projection-year calendar days in each stratum
	5) set the baseline year and projection year regulations ("$input_code_cd/set regulations.do")
*/
		


clear

* Pull in MRIP data
tempfile tl1 cl1
dsconcat $triplist

// dtrip will be used to estimate total directed trips
gen dtrip=1

sort year strat_id psu_id id_code
save `tl1'

clear

dsconcat $catchlist
sort year strat_id psu_id id_code
replace common=subinstr(lower(common)," ","",.)
save `cl1'

use `tl1'
merge 1:m year strat_id psu_id id_code using `cl1', keep(1 3) nogen // keep all trips including those w/catch==0


* Format MRIP data for estimation 

replace common=subinstr(lower(common)," ","",.)
replace prim1_common=subinstr(lower(prim1_common)," ","",.)
replace prim2_common=subinstr(lower(prim2_common)," ","",.)

* ensure only relevant states 
keep if inlist(st, 25, 44, 9,  36 , 34, 10, 24, 51, 37)

gen state="MA" if st==25
replace state="MD" if st==24
replace state="RI" if st==44
replace state="CT" if st==9
replace state="NY" if st==36
replace state="NJ" if st==34
replace state="DE" if st==10
replace state="VA" if st==51
replace state="NC" if st==37

tempfile basefile
save `basefile', replace

levelsof state, local(sts) clean
foreach s of local sts{
u `basefile', clear

keep if inlist(state,"`s'")

// classify trips into dom_id=1 (DOMAIN OF INTEREST) and dom_id=2 ('OTHER' DOMAIN)
gen str1 dom_id="2"
replace dom_id="1" if strmatch(common, "summerflounder") 
replace dom_id="1" if strmatch(prim1_common, "summerflounder") 

replace dom_id="1" if strmatch(common, "blackseabass") 
replace dom_id="1" if strmatch(prim1_common, "blackseabass") 

replace dom_id="1" if strmatch(common, "scup") 
replace dom_id="1" if strmatch(prim1_common, "scup") 

* keep only NC north based on county delineation from Tracey 
replace dom_id="2"  if state=="NC" & !inlist(cnty, 15, 29, 41, 53, 55, 139, 143, 177, 187)


tostring wave, gen(w2)
tostring year, gen(year2)
gen st2 = string(st,"%02.0f")
gen date=substr(id_code, 6,8)
gen month1=substr(date, 5, 2)
gen day1=substr(date, 7, 2)
drop if inlist(day1,"9x", "xx") 
destring day1, replace

gen mode1="sh" if inlist(mode_fx, "1", "2", "3")
replace mode1="pr" if inlist(mode_fx, "7")
replace mode1="fh" if inlist(mode_fx, "4", "5")


// Deal with Group Catch: 
	// This bit of code generates a flag for each year-strat_id psu_id leader. (equal to the lowest of the dom_id)
	// Then it generates a flag for claim equal to the largest claim.  
	// Then it re-classifies the trip into dom_id=1 if that trip had catch of species in dom_id1 

replace claim=0 if claim==.

gen domain_claim=claim if inlist(common, "summerflounder", "blackseabass", "scup") 
mvencode domain_claim, mv(0) override

bysort strat_id psu_id leader (dom_id): gen gc_flag=dom_id[1]
bysort strat_id psu_id leader (domain_claim): gen claim_flag=domain_claim[_N]
replace dom_id="1" if strmatch(dom_id,"2") & claim_flag>0 & claim_flag!=. & strmatch(gc_flag,"1")

// generate the estimation strata - year, month, kind-of-day (weekend including fed holidays/weekday), mode (pr/fh)
gen my_dom_id_string=state+"_"+year2+"_"+month1+"_"+kod+"_"+mode1+"_"+ dom_id+"_"+w2

replace my_dom_id_string=ltrim(rtrim(my_dom_id_string))

encode my_dom_id_string, gen(my_dom_id) // total with over(<overvar>) requires a numeric variable 

// keep 1 observation per year-strat-psu-id_code. This will have dom_id=1 if it targeted or caught my_common1 or my_common2. Else it will be dom_id=2
bysort year wave strat_id psu_id id_code (dom_id): gen count_obs1=_n
keep if count_obs1==1

replace wp_int=0 if wp_int<=0
svyset psu_id [pweight= wp_int], strata(strat_id) singleunit(certainty)

// retain estimation strata names (xsvmat below seems to retain encoded rather than string strata names)
preserve
keep my_dom_id my_dom_id_string
duplicates drop 
tostring my_dom_id, gen(my_dom_id2)
keep my_dom_id2 my_dom_id_string
tempfile domains
save `domains', replace 
restore

encode mode1, gen(mode2)

svy: total dtrip, over(my_dom_id) // estimate total # trips by domain

xsvmat, from(r(table)') rownames(rname) names(col) norestor // save point estimates and SEs
split rname, parse("@")
drop rname1
split rname2, parse(.)
drop rname2 rname22
rename rname21 my_dom_id2
merge 1:1 my_dom_id2 using `domains' // merge back to strata names dataset 
drop rname my_dom_id2 _merge 
order my_dom_id_string
// estimate total # trips by domain

keep my b se 

split my, parse(_)
rename my_dom_id_string1 state
rename my_dom_id_string2 year
rename my_dom_id_string3 month1
rename my_dom_id_string4 kod
rename my_dom_id_string5 mode
rename my_dom_id_string6 dom_id
rename my_dom_id_string7 wave

rename b dtrip

keep if dom_id=="1" // keep directed trip estimates for species group of interest 

drop if dtrip==.
replace se=. if dtrip==se

// Deal with strata containing no estimated SEs. The estimation strata is: current year +  month + kind-of-day + state + mode 
// For these strata, I'll impute a PSE from other strata and apply it to the missing-SE strata. Up to 7 rounds of imputation are conducted:
	// 1) month + kind-of-day + state + mode, averaged across the last two prior years
	// 2) month + kind-of-day + state + mode, most recent prior year
	// 3) month + kind-of-day + state + mode, second-most recent prior year
	// 4) current year + kind-of-day + state + mode, averaged across the prior month and the next month 
	// 5) current year + kind-of-day + state + mode, prior month 
	// 6) current year + kind-of-day + state + mode, next month 
	// 7) current year + month + kind-of-day + state, averaged across all other fishing modes

	*browse if se==.
	destring month1, gen(month2)
	destring year, gen(year2)
	gen ym=ym(year2, month2)
	format ym %tm
	gen ym_prev=ym-1
	gen ym_next=ym+1
	format ym_next ym_prev %tm
	gen pse=se/dtrip
	gen pse_impute=.
	destring year, replace 
	destring wave, replace
	
	gen one_yr_prev=ym-12
	gen two_yr_prev=ym-24
	
	gen missing_SE_imputation=""
	
	format one_yr_prev two_yr_prev %tm

	gen round1=.
	gen round2=.
	gen round3=.
	gen round4=.
	gen round5=.
	gen round6=.
	gen round7=.

	levelsof my_dom if se==. & $calibration_year , local(missings)
	foreach m of local missings{
		di "`m'"
		
		*local m "DE_2024_11_wd_sh_1_6"
		
		levelsof kod if my_dom=="`m'", clean
		local kod="`r(levels)'"
		di "`kod'"
		
		levelsof mode if my_dom=="`m'", clean
		local mode="`r(levels)'"
		di "`mode'"

		levelsof state if my_dom=="`m'", clean
		local state="`r(levels)'"
		di "`state'"

		levelsof ym if my_dom=="`m'"
		local ym=`r(levels)'
		di `ym'
		
		levelsof ym_prev if my_dom=="`m'"
		local ym_prev=`r(levels)'
		di `ym_prev'

		levelsof ym_next if my_dom=="`m'"
		local ym_next=`r(levels)'
		di `ym_next'

		levelsof one_yr_prev if my_dom=="`m'"
		local one_yr_prev=`r(levels)'
		di `one_yr_prev'
		
		levelsof two_yr_prev if my_dom=="`m'"
		local two_yr_prev=`r(levels)'
		di `two_yr_prev'

		su pse if kod=="`kod'" & state=="`state'" & mode=="`mode'" & (ym==`one_yr_prev' | ym==`two_yr_prev') 
		return list
		if `r(N)'>1{
		replace round1=`r(mean)' if my_dom=="`m'"
		}
		
		su pse if kod=="`kod'" & state=="`state'" & mode=="`mode'" & (ym==`one_yr_prev') 
		return list
		if `r(N)'>0{
		replace round2=`r(mean)' if my_dom=="`m'"
		}	
		
		su pse if kod=="`kod'" & state=="`state'" & mode=="`mode'" & (ym==`two_yr_prev') 
		return list
		if `r(N)'>0{
		replace round3=`r(mean)' if my_dom=="`m'"
		}	
		
		su pse if kod=="`kod'" & state=="`state'" & mode=="`mode'" & (ym==`ym_prev' | ym==`ym_next') 
		return list
		if `r(N)'>1{
		replace round4=`r(mean)' if my_dom=="`m'"
		}	
		
		su pse if kod=="`kod'" & state=="`state'" & mode=="`mode'" & (ym==`ym_prev') 
		return list
		if `r(N)'>0{
		replace round5=`r(mean)' if my_dom=="`m'"
		}	
		
		su pse if kod=="`kod'" & state=="`state'" & mode=="`mode'" & (ym==`ym_next') 
		return list
		if `r(N)'>0{
		replace round6=`r(mean)' if my_dom=="`m'"
		}	
		
		su pse if kod=="`kod'" & state=="`state'" & mode!="`mode'" & (ym==`ym') 
		return list
		if `r(N)'>0{
		replace round7=`r(mean)' if my_dom=="`m'"
		}	
		
		
		replace pse_impute = round1 if my_dom=="`m'" & round1!=. & pse_impute==.
			replace missing_SE_imputation="past two years avg." if my_dom=="`m'" & round1!=.
			
		replace pse_impute = round2 if my_dom=="`m'" & round2!=. & pse_impute==.
			replace missing_SE_imputation="last year" if my_dom=="`m'" & round1==. & round2!=.

		replace pse_impute = round3 if my_dom=="`m'" & round3!=. & pse_impute==.
			replace missing_SE_imputation="two years previous" if my_dom=="`m'" & round1==. & round2==. & round3!=.
		
		replace pse_impute = round4 if my_dom=="`m'" & round4!=. & pse_impute==.
			replace missing_SE_imputation="average of previous and next month" if my_dom=="`m'" & round1==. & round2==. & round3==. & round4!=.

		replace pse_impute = round5 if my_dom=="`m'" & round5!=. & pse_impute==.
			replace missing_SE_imputation="previous month" if my_dom=="`m'" & pse_impute!=. & round1==. & round2==. & round3==. & round4==.  & round5!=. 

		replace pse_impute = round6 if my_dom=="`m'" & round6!=. & pse_impute==.
			replace missing_SE_imputation="next month" if my_dom=="`m'" & pse_impute!=.  & round1==. & round2==. & round3==. & round4==.  & round5==. & round6!=. 

		replace pse_impute = round7 if my_dom=="`m'" & round7!=. & pse_impute==.
			replace missing_SE_imputation="same month other modes" if my_dom=="`m'" & pse_impute!=. & round1==. & round2==. & round3==. & round4==.  & round5==. & round6==. & round7!=. 
				
				
		replace missing_SE_imputation="no imputation" if my_dom=="`m'" & pse_impute==.  & round1==. & round2==. & round3==. & round4==.  & round5==. & round6==. & round7==. 
	}
		
keep if $calibration_year
replace se=dtrip*pse_impute if se==.			

* If SE still missing, set SE=mean, so pse=100% (highly uncertain)
replace se=dtrip if se==.

/*
*Code to retain imputation strata 
preserve 
keep my_dom_id_string  missing_SE_imputation
duplicates drop 
save "$input_data_cd\directed_trips_imputations_`s'.dta", replace
restore 
*/

keep dtrip se state year month2 kod mode
rename month2 month
tostring month, gen(month1)

count
local num=`r(N)'
di `num'

tempfile new
save `new', replace 

global drawz
forv d = 1/`num'{
u `new', clear 

keep if _n==`d'
su dtrip
local est = `r(mean)'

su se
local sd = `r(mean)'

expand $ndraws
gen dtrip_not_trunc=rnormal(`est', `sd')
gen dtrip_new=max(dtrip_not_trunc, 0)

/*
*simulate using lognormal 
scalar mu_log = log((`est'^2)/sqrt(`sd'^2+`est'^2))
scalar sigma_log =sqrt(log(1 + (`sd'^2 / `est'^2)))
gen dtrip_logn_draw = exp(rnormal(mu_log, sigma_log))
 */
gen draw=_n

tempfile drawz`d'
save `drawz`d'', replace
global drawz "$drawz "`drawz`d''" " 
}

clear
dsconcat $drawz



/*The following attempts to correct for bias that occurs when drawing from uncertain MRIP estimates. 
	*When an MRIP estimate is very uncertain, some draws of x from a normal distribution can result in x_i<0. Because trip outcomes cannot 
	*be negative, I change these to 0. But doing so results in an upwardly shifted mean across draws. To correct for this, I sum x_i
	*across draws where x_i<0, and subtract this value from each draw where x_i>0 in proportion to the amount that each x_i>0 contributes to the total 
	*number of trips across x_i's>0.
	*This partly corrects for the issue; however, subtracting a fixed value from x_i where x_i>0 leads to some of these x_i's now <0. I replace these values as 0. */

*I have tried paramaterizing non-negative distributions using the MRIP point estimate and SE, but these resulted in larger differences in the mean trip estimates across all draws by domain (month, kindo-of-day, and mode) than the approach used here. Can work on this in the future. 
 
gen domain=state+"_"+month1+"_"+kod+"_"+mode

egen sum_neg=sum(dtrip_not) if dtrip_not<0, by(domain)
sort domain
egen mean_sum_neg=mean(sum_neg), by(domain) //sum of trips across draws where trips<0 

egen sum_non_neg=sum(dtrip_not) if dtrip_not>0 , by(domain) //sum of trips across draws where trips<0 
gen prop=dtrip_not/sum_non_neg //proportion that each x_i>0 contributes to the sum(x_i) across x_i's>0
gen tab=1 if  dtrip_not>0
egen sumtab=sum(tab), by(domain)
gen adjust=prop*mean_sum_neg //adjustment factor used to adjust each x_i>0

gen dtrip_new2=dtrip_new+adjust if dtrip_new!=0 & adjust !=. //subtract the adjustment factor
replace dtrip_new2=dtrip_new if dtrip_new2==. 
replace dtrip_new2=0 if dtrip_new2<0 //if an adjustment leads to a negative x_i for x_i's that were originally>0, replace these as zeros

*twoway (kdensity dtrip_not  if domain=="NY_08_wd_sh") (kdensity dtrip_new2  if domain=="NY_08_wd_sh")
*tabstat se dtrip  dtrip_new2, stat(mean sd) by(domain)


*check differences between original and adjusted draws 
/*
su dtrip_new2 
return list
local new = `r(sum)'

su dtrip_not
return list
local not_truc = `r(sum)'
di ((`new'-`not_truc')/`not_truc')*100

su dtrip_new2
return list
local new = `r(sum)'

su dtrip_not
return list
local old = `r(sum)'

di ((`new'-`old')/`old')*100
*/
replace dtrip_new=dtrip_new2

drop  domain  dtrip_not sum_neg sum_non_neg prop mean_sum_neg adjust dtrip_new2 tab sumtab month1

sort state mode year month kod draw 
tostring month, replace format("%02.0f")
tostring year, replace format("%02.0f")

tempfile new1
save `new1'

*now need to make a dataset for the calender year and average out the directed trips across days

global drawz2

forv d = 1/$ndraws{
	u `new1', clear 
	keep if draw==`d'

	tempfile dtrips`d'
	save `dtrips`d'', replace 
	
clear 
set obs 2
gen day=$calibration_start_date if _n==1
replace day=$calibration_end_date if _n==2
format day %td
drop if day==$leap_yr_days
tsset day
tsfill, full
gen day_i=_n

gen dow = dow(day)  //0=Sunday,...,6=Saturday

gen kod="we" if inlist(dow, 5, 6, 0)
replace kod="wd" if inlist(dow, 1, 2, 3, 4)

//add the 12 federal holidays as weekends	
replace kod="we" if $fed_holidays 

gen year=year(day)				
gen month=month(day)				
gen month2 = string(month,"%02.0f")
tostring year, replace
drop month
rename month2 month
gen mode="sh"
expand 2, gen(dup)
replace mode="pr" if dup==1
drop dup
expand 2 if mode=="pr", gen(dup)
replace mode="fh" if dup==1
drop dup

merge m:1  kod month mode using `dtrips`d''
*gen draw=`d'
tempfile drawz2`d'
save `drawz2`d'', replace
global drawz2 "$drawz2 "`drawz2`d''" " 

}
clear
dsconcat $drawz2
sort day  mode draw
*browse if _merge==1
bysort day mode: gen draw2=_n
order draw2
replace draw=draw2 if draw==.
drop draw2

mvencode dtrip dtrip_new, mv(0) override

*number of weekend/weekday days per state, month, and mode, and draw
gen tab=1
bysort month kod mode draw:egen sum_days=sum(tab)
order sum_days

sort draw mode day 
order draw
sort day
drop dtrip
rename dtrip_new dtrip
mvencode dtrip, mv(0) override
gen trips_per_day=dtrip/sum_days
mvencode trips_per_day, mv(0) override 
order dtrip trips_per_day

order mode year month kod dow day day_i trips_per_day draw
drop dtrip sum_days se _merge tab 

sort  draw mode day 
rename trips_per_day dtrip


gen day1=day(day)
gen month1=month(day)

su dtrip
return list
*call the regulations file	

replace state="`s'" if state==""

do "$input_code_cd/set_regulations.do"	

/*
preserve
keep if cod_bag!=0
keep day
duplicates drop 
rename day date
gen cod_season_open=1
save  "$input_data_cd\cod_open_season_dates.dta",  replace 
restore 
*/

compress
export delimited using "$misc_data_cd\directed_trips_calibration_`s'.csv",  replace 



**Now adjust for the differences in directed trips due to changes in kod between calibration year y and y+1
tostring month, gen(month_y1) format("%02.0f")
gen month2 = string(month,"%02.0f")

tempfile base 
save `base', replace 

global drawz

levelsof draw, local(drawss)
foreach d of local drawss{

u `base', clear

keep if draw==`d'
gen domain_y1=mode+"_"+month_y1+"_"+kod
gen domain_y2=mode+"_"+month_y2+"_"+kod_y2

gen dtrip_y2=dtrip if domain_y1==domain_y2 

levelsof domain_y2 if dtrip_y2==., local(domains)
foreach p of local domains{
	su dtrip if domain_y1=="`p'"
	return list
	replace dtrip_y2=`r(mean)' if domain_y2=="`p'" & dtrip_y2==.
	
}
collapse (sum) dtrip dtrip_y2, by(month mode)
gen expansion_factor = dtrip_y2/dtrip
gen draw=`d'

tempfile drawz`d'
save `drawz`d'', replace
global drawz "$drawz "`drawz`d''" " 
}

dsconcat $drawz

gen state="`s'" 

mvencode expansion_factor, mv(1) override

su dtrip
return list

su dtrip_y2
return list

gen check =dtrip*expansion
su check
return list

drop check 
compress

export delimited using "$misc_data_cd\proj_year_calendar_adjustments_`s'.csv",  replace 
}






************** Part B  **************
* Compute totals estimates to compare with calibration output

** Estimates by state and mode
cd $misc_data_cd

clear

* Pull in MRIP data
tempfile tl1 cl1
dsconcat $triplist

// dtrip will be used to estimate total directed trips
gen dtrip=1

sort year strat_id psu_id id_code
save `tl1'

clear

dsconcat $catchlist
sort year strat_id psu_id id_code
replace common=subinstr(lower(common)," ","",.)
save `cl1'

use `tl1'
merge 1:m year strat_id psu_id id_code using `cl1', keep(1 3) nogen // keep all trips including those w/catch==0

keep if $calibration_year

* Format MRIP data for estimation 

replace common=subinstr(lower(common)," ","",.)
replace prim1_common=subinstr(lower(prim1_common)," ","",.)
replace prim2_common=subinstr(lower(prim2_common)," ","",.)

* ensure only relevant states 
keep if inlist(st, 25, 44, 9,  36 , 34, 10, 24, 51, 37)

gen state="MA" if st==25
replace state="MD" if st==24
replace state="RI" if st==44
replace state="CT" if st==9
replace state="NY" if st==36
replace state="NJ" if st==34
replace state="DE" if st==10
replace state="VA" if st==51
replace state="NC" if st==37


// classify trips into dom_id=1 (DOMAIN OF INTEREST) and dom_id=2 ('OTHER' DOMAIN)
gen str1 dom_id="2"
replace dom_id="1" if strmatch(common, "summerflounder") 
replace dom_id="1" if strmatch(prim1_common, "summerflounder") 

replace dom_id="1" if strmatch(common, "blackseabass") 
replace dom_id="1" if strmatch(prim1_common, "blackseabass") 

replace dom_id="1" if strmatch(common, "scup") 
replace dom_id="1" if strmatch(prim1_common, "scup") 

* keep only NC north based on county delineation from Tracey 
replace dom_id="2"  if state=="NC" & !inlist(cnty, 15, 29, 41, 53, 55, 139, 143, 177, 187)

tostring wave, gen(w2)
tostring year, gen(year2)
gen st2 = string(st,"%02.0f")
gen date=substr(id_code, 6,8)
gen month1=substr(date, 5, 2)
gen day1=substr(date, 7, 2)
drop if inlist(day1,"9x", "xx") 
destring day1, replace

gen mode1="sh" if inlist(mode_fx, "1", "2", "3")
replace mode1="pr" if inlist(mode_fx, "7")
replace mode1="fh" if inlist(mode_fx, "4", "5")


// Deal with Group Catch: 
	// This bit of code generates a flag for each year-strat_id psu_id leader. (equal to the lowest of the dom_id)
	// Then it generates a flag for claim equal to the largest claim.  
	// Then it re-classifies the trip into dom_id=1 if that trip had catch of species in dom_id1 

replace claim=0 if claim==.

gen domain_claim=claim if inlist(common, "summerflounder", "blackseabass", "scup") 
mvencode domain_claim, mv(0) override

bysort strat_id psu_id leader (dom_id): gen gc_flag=dom_id[1]
bysort strat_id psu_id leader (domain_claim): gen claim_flag=domain_claim[_N]
replace dom_id="1" if strmatch(dom_id,"2") & claim_flag>0 & claim_flag!=. & strmatch(gc_flag,"1")

gen my_dom_id_string=state+"_"+year2+"_"+mode1+"_"+ dom_id
replace my_dom_id_string=ltrim(rtrim(my_dom_id_string))

encode my_dom_id_string, gen(my_dom_id) // total with over(<overvar>) requires a numeric variable 

// keep 1 observation per year-strat-psu-id_code. This will have dom_id=1 if it targeted or caught my_common1 or my_common2. Else it will be dom_id=2
bysort year wave strat_id psu_id id_code (dom_id): gen count_obs1=_n
keep if count_obs1==1

replace wp_int=0 if wp_int<=0
svyset psu_id [pweight= wp_int], strata(strat_id) singleunit(certainty)


// retain estimation strata names (xsvmat below seems to retain encoded rather than string strata names)
preserve
keep my_dom_id my_dom_id_string
duplicates drop 
tostring my_dom_id, gen(my_dom_id2)
keep my_dom_id2 my_dom_id_string
tempfile domains
save `domains', replace 
restore

encode mode1, gen(mode2)

svy: total dtrip, over(my_dom_id) // estimate total # trips by domain

xsvmat, from(r(table)') rownames(rname) names(col) norestor // save point estimates and SEs
split rname, parse("@")
drop rname1
split rname2, parse(.)
drop rname2 rname22
rename rname21 my_dom_id2
merge 1:1 my_dom_id2 using `domains' // merge back to strata names dataset 
drop rname my_dom_id2 _merge 
order my_dom_id_string

keep my b se ll ul

split my, parse(_)
rename my_dom_id_string1 state
rename my_dom_id_string2 year
rename my_dom_id_string3 mode
rename my_dom_id_string4 dom_id
rename b dtrip_mrip
rename se se_mrip
rename ll ll_mrip
rename ul ul_mrip

keep if dom_id=="1" // keep directed trip estimates for species group of interest 
drop dom

save "$misc_data_cd\mrip_dtrip_calib_state_mode.dta", replace 


** Estimates by state 
clear

* Pull in MRIP data
tempfile tl1 cl1
dsconcat $triplist

// dtrip will be used to estimate total directed trips
gen dtrip=1

sort year strat_id psu_id id_code
save `tl1'

clear

dsconcat $catchlist
sort year strat_id psu_id id_code
replace common=subinstr(lower(common)," ","",.)
save `cl1'

use `tl1'
merge 1:m year strat_id psu_id id_code using `cl1', keep(1 3) nogen // keep all trips including those w/catch==0

keep if $calibration_year

* Format MRIP data for estimation 

replace common=subinstr(lower(common)," ","",.)
replace prim1_common=subinstr(lower(prim1_common)," ","",.)
replace prim2_common=subinstr(lower(prim2_common)," ","",.)

* ensure only relevant states 
keep if inlist(st, 25, 44, 9,  36 , 34, 10, 24, 51, 37)

gen state="MA" if st==25
replace state="MD" if st==24
replace state="RI" if st==44
replace state="CT" if st==9
replace state="NY" if st==36
replace state="NJ" if st==34
replace state="DE" if st==10
replace state="VA" if st==51
replace state="NC" if st==37

// classify trips into dom_id=1 (DOMAIN OF INTEREST) and dom_id=2 ('OTHER' DOMAIN)
gen str1 dom_id="2"
replace dom_id="1" if strmatch(common, "summerflounder") 
replace dom_id="1" if strmatch(prim1_common, "summerflounder") 

replace dom_id="1" if strmatch(common, "blackseabass") 
replace dom_id="1" if strmatch(prim1_common, "blackseabass") 

replace dom_id="1" if strmatch(common, "scup") 
replace dom_id="1" if strmatch(prim1_common, "scup") 

* keep only NC north based on county delineation from Tracey 
replace dom_id="2"  if state=="NC" & !inlist(cnty, 15, 29, 41, 53, 55, 139, 143, 177, 187)


tostring wave, gen(w2)
tostring year, gen(year2)
gen st2 = string(st,"%02.0f")
gen date=substr(id_code, 6,8)
gen month1=substr(date, 5, 2)
gen day1=substr(date, 7, 2)
drop if inlist(day1,"9x", "xx") 
destring day1, replace

gen mode1="sh" if inlist(mode_fx, "1", "2", "3")
replace mode1="pr" if inlist(mode_fx, "7")
replace mode1="fh" if inlist(mode_fx, "4", "5")


// Deal with Group Catch: 
	// This bit of code generates a flag for each year-strat_id psu_id leader. (equal to the lowest of the dom_id)
	// Then it generates a flag for claim equal to the largest claim.  
	// Then it re-classifies the trip into dom_id=1 if that trip had catch of species in dom_id1 

replace claim=0 if claim==.

gen domain_claim=claim if inlist(common, "summerflounder", "blackseabass", "scup") 
mvencode domain_claim, mv(0) override

bysort strat_id psu_id leader (dom_id): gen gc_flag=dom_id[1]
bysort strat_id psu_id leader (domain_claim): gen claim_flag=domain_claim[_N]
replace dom_id="1" if strmatch(dom_id,"2") & claim_flag>0 & claim_flag!=. & strmatch(gc_flag,"1")

gen my_dom_id_string=state+"_"+year2+"_"+ dom_id
replace my_dom_id_string=ltrim(rtrim(my_dom_id_string))

encode my_dom_id_string, gen(my_dom_id) // total with over(<overvar>) requires a numeric variable 

// keep 1 observation per year-strat-psu-id_code. This will have dom_id=1 if it targeted or caught my_common1 or my_common2. Else it will be dom_id=2
bysort year wave strat_id psu_id id_code (dom_id): gen count_obs1=_n
keep if count_obs1==1

replace wp_int=0 if wp_int<=0
svyset psu_id [pweight= wp_int], strata(strat_id) singleunit(certainty)


// retain estimation strata names (xsvmat below seems to retain encoded rather than string strata names)
preserve
keep my_dom_id my_dom_id_string
duplicates drop 
tostring my_dom_id, gen(my_dom_id2)
keep my_dom_id2 my_dom_id_string
tempfile domains
save `domains', replace 
restore

encode mode1, gen(mode2)

svy: total dtrip, over(my_dom_id) // estimate total # trips by domain

xsvmat, from(r(table)') rownames(rname) names(col) norestor // save point estimates and SEs
split rname, parse("@")
drop rname1
split rname2, parse(.)
drop rname2 rname22
rename rname21 my_dom_id2
merge 1:1 my_dom_id2 using `domains' // merge back to strata names dataset 
drop rname my_dom_id2 _merge 
order my_dom_id_string

keep my b se ll ul

split my, parse(_)
rename my_dom_id_string1 state
rename my_dom_id_string2 year
rename my_dom_id_string3 dom_id
rename b dtrip_mrip
rename se se_mrip
rename ll ll_mrip
rename ul ul_mrip

keep if dom_id=="1" // keep directed trip estimates for species group of interest 
drop dom

save "$misc_data_cd\mrip_dtrip_calib_state.dta", replace 


** Estimates by state, mode, and wave 
clear

* Pull in MRIP data
tempfile tl1 cl1
dsconcat $triplist

// dtrip will be used to estimate total directed trips
gen dtrip=1

sort year strat_id psu_id id_code
save `tl1'

clear

dsconcat $catchlist
sort year strat_id psu_id id_code
replace common=subinstr(lower(common)," ","",.)
save `cl1'

use `tl1'
merge 1:m year strat_id psu_id id_code using `cl1', keep(1 3) nogen // keep all trips including those w/catch==0

keep if $calibration_year

* Format MRIP data for estimation 

replace common=subinstr(lower(common)," ","",.)
replace prim1_common=subinstr(lower(prim1_common)," ","",.)
replace prim2_common=subinstr(lower(prim2_common)," ","",.)

* ensure only relevant states 
keep if inlist(st, 25, 44, 9,  36 , 34, 10, 24, 51, 37)

gen state="MA" if st==25
replace state="MD" if st==24
replace state="RI" if st==44
replace state="CT" if st==9
replace state="NY" if st==36
replace state="NJ" if st==34
replace state="DE" if st==10
replace state="VA" if st==51
replace state="NC" if st==37

// classify trips into dom_id=1 (DOMAIN OF INTEREST) and dom_id=2 ('OTHER' DOMAIN)
gen str1 dom_id="2"
replace dom_id="1" if strmatch(common, "summerflounder") 
replace dom_id="1" if strmatch(prim1_common, "summerflounder") 

replace dom_id="1" if strmatch(common, "blackseabass") 
replace dom_id="1" if strmatch(prim1_common, "blackseabass") 

replace dom_id="1" if strmatch(common, "scup") 
replace dom_id="1" if strmatch(prim1_common, "scup") 

* keep only NC north based on county delineation from Tracey 
replace dom_id="2"  if state=="NC" & !inlist(cnty, 15, 29, 41, 53, 55, 139, 143, 177, 187)

tostring wave, gen(w2)
tostring year, gen(year2)
gen st2 = string(st,"%02.0f")
gen date=substr(id_code, 6,8)
gen month1=substr(date, 5, 2)
gen day1=substr(date, 7, 2)
drop if inlist(day1,"9x", "xx") 
destring day1, replace

gen mode1="sh" if inlist(mode_fx, "1", "2", "3")
replace mode1="pr" if inlist(mode_fx, "7")
replace mode1="fh" if inlist(mode_fx, "4", "5")


// Deal with Group Catch: 
	// This bit of code generates a flag for each year-strat_id psu_id leader. (equal to the lowest of the dom_id)
	// Then it generates a flag for claim equal to the largest claim.  
	// Then it re-classifies the trip into dom_id=1 if that trip had catch of species in dom_id1 

replace claim=0 if claim==.

gen domain_claim=claim if inlist(common, "summerflounder", "blackseabass", "scup") 
mvencode domain_claim, mv(0) override

bysort strat_id psu_id leader (dom_id): gen gc_flag=dom_id[1]
bysort strat_id psu_id leader (domain_claim): gen claim_flag=domain_claim[_N]
replace dom_id="1" if strmatch(dom_id,"2") & claim_flag>0 & claim_flag!=. & strmatch(gc_flag,"1")

gen my_dom_id_string=state+"_"+year2+"_"+ dom_id + "_"+ w2 +"_"+mode1
replace my_dom_id_string=ltrim(rtrim(my_dom_id_string))

encode my_dom_id_string, gen(my_dom_id) // total with over(<overvar>) requires a numeric variable 

// keep 1 observation per year-strat-psu-id_code. This will have dom_id=1 if it targeted or caught my_common1 or my_common2. Else it will be dom_id=2
bysort year wave strat_id psu_id id_code (dom_id): gen count_obs1=_n
keep if count_obs1==1

replace wp_int=0 if wp_int<=0
svyset psu_id [pweight= wp_int], strata(strat_id) singleunit(certainty)


// retain estimation strata names (xsvmat below seems to retain encoded rather than string strata names)
preserve
keep my_dom_id my_dom_id_string
duplicates drop 
tostring my_dom_id, gen(my_dom_id2)
keep my_dom_id2 my_dom_id_string
tempfile domains
save `domains', replace 
restore

encode mode1, gen(mode2)

svy: total dtrip, over(my_dom_id) // estimate total # trips by domain

xsvmat, from(r(table)') rownames(rname) names(col) norestor // save point estimates and SEs
split rname, parse("@")
drop rname1
split rname2, parse(.)
drop rname2 rname22
rename rname21 my_dom_id2
merge 1:1 my_dom_id2 using `domains' // merge back to strata names dataset 
drop rname my_dom_id2 _merge 
order my_dom_id_string

keep my b se ll ul

split my, parse(_)
rename my_dom_id_string1 state
rename my_dom_id_string2 year
rename my_dom_id_string3 dom_id
rename my_dom_id_string4 wave
rename my_dom_id_string5 mode

rename b dtrip_mrip
rename se se_mrip
rename ll ll_mrip
rename ul ul_mrip

keep if dom_id=="1" // keep directed trip estimates for species group of interest 
drop dom

save "$misc_data_cd\mrip_dtrip_calib_state_mode_wave.dta", replace 



** Estimates by state and month
clear

* Pull in MRIP data
tempfile tl1 cl1
dsconcat $triplist

// dtrip will be used to estimate total directed trips
gen dtrip=1

sort year strat_id psu_id id_code
save `tl1'

clear

dsconcat $catchlist
sort year strat_id psu_id id_code
replace common=subinstr(lower(common)," ","",.)
save `cl1'

use `tl1'
merge 1:m year strat_id psu_id id_code using `cl1', keep(1 3) nogen // keep all trips including those w/catch==0

keep if $calibration_year

* Format MRIP data for estimation 

replace common=subinstr(lower(common)," ","",.)
replace prim1_common=subinstr(lower(prim1_common)," ","",.)
replace prim2_common=subinstr(lower(prim2_common)," ","",.)

* ensure only relevant states 
keep if inlist(st, 25, 44, 9,  36 , 34, 10, 24, 51, 37)

gen state="MA" if st==25
replace state="MD" if st==24
replace state="RI" if st==44
replace state="CT" if st==9
replace state="NY" if st==36
replace state="NJ" if st==34
replace state="DE" if st==10
replace state="VA" if st==51
replace state="NC" if st==37


// classify trips into dom_id=1 (DOMAIN OF INTEREST) and dom_id=2 ('OTHER' DOMAIN)
gen str1 dom_id="2"
replace dom_id="1" if strmatch(common, "summerflounder") 
replace dom_id="1" if strmatch(prim1_common, "summerflounder") 

replace dom_id="1" if strmatch(common, "blackseabass") 
replace dom_id="1" if strmatch(prim1_common, "blackseabass") 

replace dom_id="1" if strmatch(common, "scup") 
replace dom_id="1" if strmatch(prim1_common, "scup") 

* keep only NC north based on county delineation from Tracey 
replace dom_id="2"  if state=="NC" & !inlist(cnty, 15, 29, 41, 53, 55, 139, 143, 177, 187)


tostring wave, gen(w2)
tostring year, gen(year2)
gen st2 = string(st,"%02.0f")
gen date=substr(id_code, 6,8)
gen month1=substr(date, 5, 2)
gen day1=substr(date, 7, 2)
drop if inlist(day1,"9x", "xx") 
destring day1, replace

gen mode1="sh" if inlist(mode_fx, "1", "2", "3")
replace mode1="pr" if inlist(mode_fx, "7")
replace mode1="fh" if inlist(mode_fx, "4", "5")


// Deal with Group Catch: 
	// This bit of code generates a flag for each year-strat_id psu_id leader. (equal to the lowest of the dom_id)
	// Then it generates a flag for claim equal to the largest claim.  
	// Then it re-classifies the trip into dom_id=1 if that trip had catch of species in dom_id1 

replace claim=0 if claim==.

gen domain_claim=claim if inlist(common, "summerflounder", "blackseabass", "scup") 
mvencode domain_claim, mv(0) override

bysort strat_id psu_id leader (dom_id): gen gc_flag=dom_id[1]
bysort strat_id psu_id leader (domain_claim): gen claim_flag=domain_claim[_N]
replace dom_id="1" if strmatch(dom_id,"2") & claim_flag>0 & claim_flag!=. & strmatch(gc_flag,"1")

gen my_dom_id_string=state+"_"+year2+"_"+ dom_id+"_"+month1
replace my_dom_id_string=ltrim(rtrim(my_dom_id_string))

encode my_dom_id_string, gen(my_dom_id) // total with over(<overvar>) requires a numeric variable 

// keep 1 observation per year-strat-psu-id_code. This will have dom_id=1 if it targeted or caught my_common1 or my_common2. Else it will be dom_id=2
bysort year wave strat_id psu_id id_code (dom_id): gen count_obs1=_n
keep if count_obs1==1

replace wp_int=0 if wp_int<=0
svyset psu_id [pweight= wp_int], strata(strat_id) singleunit(certainty)


// retain estimation strata names (xsvmat below seems to retain encoded rather than string strata names)
preserve
keep my_dom_id my_dom_id_string
duplicates drop 
tostring my_dom_id, gen(my_dom_id2)
keep my_dom_id2 my_dom_id_string
tempfile domains
save `domains', replace 
restore

encode mode1, gen(mode2)

svy: total dtrip, over(my_dom_id) // estimate total # trips by domain

xsvmat, from(r(table)') rownames(rname) names(col) norestor // save point estimates and SEs
split rname, parse("@")
drop rname1
split rname2, parse(.)
drop rname2 rname22
rename rname21 my_dom_id2
merge 1:1 my_dom_id2 using `domains' // merge back to strata names dataset 
drop rname my_dom_id2 _merge 
order my_dom_id_string

keep my b se ll ul

split my, parse(_)
rename my_dom_id_string1 state
rename my_dom_id_string2 year
rename my_dom_id_string3 dom_id
rename my_dom_id_string4 month

rename b dtrip_mrip
rename se se_mrip
rename ll ll_mrip
rename ul ul_mrip

keep if dom_id=="1" // keep directed trip estimates for species group of interest 
drop dom
destring month, replace 
save "$misc_data_cd\mrip_dtrip_calib_state_month.dta", replace 
