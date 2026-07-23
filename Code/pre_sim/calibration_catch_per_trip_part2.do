


/*******************************************************************************
 Script:       calibration_catch_per_trip_part2.do
 Purpose:      Two jobs. First, builds the angler demographic pool (age and
               avidity) from the MRIP Fishing Effort Survey 12-month files,
               with the QA/QC screens the FES delivery notes require. Second,
               builds the calibration-year catch draw files: for every day
               with directed trips, 50 simulated trips x 30 catch-per-trip
               draws, each assigned a catch outcome sampled from the copula
               pool for its mode x wave stratum and demographics from the
               pool built in the first half.
 Inputs:       fes_person_final_2023<wave>.dta (waves 1-6),
               directed_trips_calibration_<ST>.csv,
               calib_catch_draws_raw_<ST>_<i>.dta,
               trip_costs.dta,
               preference_params.dta
 Outputs:      angler_dems.dta, calib_catch_draws_<ST>_<i>.dta
 Dependencies: Globals $misc_data_cd, $calib_catch_data_cd and $ndraws.
               Requires copula_modeling_calibration.R to have produced the
               raw calibration draws, and catch_per_trip_calibration_part1.do
               to have produced the directed-trips calendar. Uses the
               user-written command dsconcat.
 Pipeline:     Step 9 of model_wrapper.do, gated by the `catch_per_trip2'
               toggle (default ON). Its projection-year analog is
               catch_per_trip_projection_part2.do, which shares the trip
               expansion and catch-sampling logic but omits the demographics.

 Data sensitivity note: the FES 12-month files read below are NOT publicly
 available and, per the note from the data provider reproduced in the source
 comments, are essentially unedited raw responses. The QA/QC screens applied
 here are this project's own, not NOAA's - the 12-month effort fields do not
 go through the editing that the 2-month fields do.
*******************************************************************************/

display "calibration_catch_per_trip_part2.do: building the angler demographic pool, then calibration catch draws for 9 states x $ndraws draws. This is one of the longest steps in the Stata pipeline and may run for hours."

* This script create calibration catch draw files for each state that contains:
	* 50 trips in each day of the calibration year in which there were directed trips, each with 30 draws of catch-per-trip
	* demographics for each trip that are constant across catch draws
	
	
********************************
* Demographics: age and avidity (number trips past 12 months) 
	* Ages and avidity come from the fishing effort survey 12 MONTH files. 
	* These data are NOT publicly available and the data have not been processeed for QA/QC like the publicly available 2-month files. 
	* Data from 2018-2023 was delivered by Lucas Johanssen on 4/23/2025. A few notes/caveats from Lucas:
		* "FES QC processes focus on the 2-month reference periods, and we do very little evaluation and editing of 12-month effort responses.  
		* Responses for these fields are essentially unedited, raw data.  
		* The final weight trimming procedures focus on reducing the impacts of outlier values on wave-level estimates. 
		* The data may include records that are highly influential with respect to 12-month effort and any estimates may be highly variable.  
		* Wave data will produce independent estimates of 12-month effort."
		
*I will use the most recent year of FES survey data available (2023)

/**************************************************/
/**************************************************/
/* Section A: Angler demographic pool from FES    */
/**************************************************/
/**************************************************/

display "calibration_catch_per_trip_part2.do: assembling the FES angler demographic pool."

global dems
local wvs 1 2 3 4 5 6
foreach w of local wvs{
	
u  "$misc_data_cd\fes_person_final_2023`w'.dta", clear 

gen state="MA" if st==25
replace state="MD" if st==24
replace state="RI" if st==44
replace state="CT" if st==9
replace state="NY" if st==36
replace state="NJ" if st==34
replace state="DE" if st==10
replace state="VA" if st==51
replace state="NC" if st==37
keep if state!=""

tempfile dems`w'
save `dems`w'', replace
global dems "$dems "`dems`w''" " 

}
clear
dsconcat $dems

gen total_trips_12=boat_trips_12+shore_trips_12
gen total_trips_2=boat_trips+shore_trips

/* The screens below are internal consistency filters on unedited survey
   responses, not statistical outlier trimming for its own sake. In order:
   -3 is the FES missing-value sentinel for age; the 16+ restriction matches
   the choice experiment's sampling frame (licensees), so that the simulated
   age distribution and the estimated preferences describe the same
   population; reporting more trips in the last 2 months than in the last 12
   is internally impossible; 62 two-month trips is roughly one per day and
   365 twelve-month trips is more than one per day, both taken as
   implausible self-reports. */
* Lou's QA/QC on the FES data

drop if age==-3 // drop missing ages
keep if age>=16 // drop anglers below the minimum age required for license to align the age distribution with choice experiment sampling frame, which is based on licensees (16+)

replace total_trips_2=round(total_trips_2)
replace total_trips_12=round(total_trips_12)
drop if total_trips_2>total_trips_12 // drop if total trips 2 months>total trips 12 months

drop if total_trips_2>=62 // drop if total trips 2 months>60 
drop if total_trips_12>=365 // drop if total trips 12 months>365 

/* The expand below turns the weighted survey into an explicit population: an
   angler with weight w becomes w rows, so later sampling from this file is
   simple random sampling that nonetheless respects the survey weights. That
   is why the weights must first be divided by 100 - expanding on the raw
   weights would attempt to materialize ~300 million rows. Dividing
   proportionally preserves the relative weighting, so the resulting age and
   avidity distributions are unchanged; only the pool size shrinks. */
replace final=final/100 // sum of weights is almost 300 million, so I proportionally reduce the weights so my Stata doesn't blow up
replace final=round(final)

expand final
su total_trips_12, detail  

egen p9995 = pctile(total_trips_12), p(99.95) // drop total_trips_12 above the 99.95 percentile
drop if total_trips_12>p9995

keep age total_trips_12 wave state
save "$misc_data_cd\angler_dems.dta", replace 

********************************

/*
* Check to make sure the copula model did not produce NAs
local statez "MA RI"
foreach s of local statez{
	
forv i=1/100{

import excel using "$iterative_input_data_cd\catch_draws_`s'_`i'.xlsx", clear firstrow
gen sf_cat=sf_keep_sim+sf_rel_sim
gen bsb_cat=bsb_keep_sim+bsb_rel_sim
gen scup_cat=scup_keep_sim+scup_rel_sim
*di "`s'"
*di `i'
qui{
	 count if sf_keep_sim==. | sf_rel_sim==. | bsb_keep_sim==. | bsb_rel_sim==. | scup_keep_sim==. | scup_rel_sim==. | sf_cat==. | bsb_cat ==. | scup_cat==. 
}

if `r(N)'>0{
	di `r(N)'
	levelsof my_dom if  sf_keep_sim==. | sf_rel_sim==. | bsb_keep_sim==. | bsb_rel_sim==. | scup_keep_sim==. | scup_rel_sim==. | sf_cat==. | bsb_cat ==. | scup_cat==. 
	
}
}

}
*/

/**************************************************/
/**************************************************/
/* Section B: Calibration catch draw files        */
/**************************************************/
/**************************************************/

/* The commented-out block above is a QA check that looked for missing values
   in the copula output before this stage consumed it. It is disabled and
   hardcodes both a two-state list and 100 draws regardless of $ndraws.

   From here the logic parallels catch_per_trip_projection_part2.do: expand
   each day x mode to 50 trips x 30 catch draws, then sample catch outcomes
   from the copula pool for the matching mode x wave. See that file for the
   expansion and with-replacement sampling notes; the difference here is that
   this version also merges on the demographics built in Section A, which the
   projection inherits rather than redrawing. */

display "calibration_catch_per_trip_part2.do: generating calibration catch draw files."

************** Generate catch draw files ******************

* faster version
local regions "MA RI CT NY NJ DE MD VA NC"

set more off
set rmsg off
	
foreach s of local regions {
	
	*local s "DE"
    import delimited using "$misc_data_cd\directed_trips_calibration_`s'.csv", clear

    gen double date_num = date(date, "DMY")
	drop month month1
    gen byte   month    = month(date_num)
    gen str2   month1   = string(month, "%02.0f")
    gen byte   wave     = cond(inlist(month,1,2),1, ///
                        cond(inlist(month,3,4),2, ///
                        cond(inlist(month,5,6),3, ///
                        cond(inlist(month,7,8),4, ///
                        cond(inlist(month,9,10),5,6)))))
    drop date_num

    drop if dtrip==0

	drop  dtrip *_bag *_min *_y2
	
	tempfile base
    save `base', replace

	forvalues i=1/$ndraws {
*		local i 1
        use `base', clear
        keep if draw==`i'

        * Expand to 50 trips x 30 catch draws within each (mode,date)
        egen long dom = group(mode date)   
        expand 50
        bysort mode date: gen int tripid = _n
        expand 30
        bysort mode date tripid: gen byte catch_draw = _n

		egen group=group(date tripid mode)
		
		qui distinct group if mode=="pr"
		local n_pr = `r(ndistinct)'
		
		qui distinct group if mode=="fh"
		local n_fh = `r(ndistinct)'
		
		qui distinct group if mode=="sh"
		local n_sh = `r(ndistinct)'
		
		preserve 
		keep date mode tripid
		duplicates drop 
		by mode: gen mode_id=_n
		tempfile mode_id
		save `mode_id', replace
		restore 
		
		merge m:1 date mode tripid using `mode_id', keep(3) nogen  
		
		qui distinct group if wave==1
		local n_wave1 = `r(ndistinct)'
		
		qui distinct group if wave==2
		local n_wave2 = `r(ndistinct)'
		
		qui distinct group if wave==3
		local n_wave3 = `r(ndistinct)'
		
		qui distinct group if wave==4
		local n_wave4 = `r(ndistinct)'
		
		qui distinct group if wave==5
		local n_wave5 = `r(ndistinct)'
		
		qui distinct group if wave==6
		local n_wave6 = `r(ndistinct)'
		
		preserve 
		keep date wave tripid
		duplicates drop 
		sort date wave tripid
		bysort wave: gen wave_id=_n
		tempfile wave_id
		save `wave_id', replace
		restore 
		
		merge m:1 date wave tripid using `wave_id', keep(3) nogen  
		
		
        * Costs: resample once per draw
        preserve
            use "$misc_data_cd\trip_costs.dta", clear
            keep if state == substr("`s'",1,2)  
        restore

        local st = state[1]

        preserve
            use "$misc_data_cd\trip_costs.dta", clear
            keep if state=="`st'"
            keep state mode cost 
            tempfile costspool
            save `costspool', replace
        restore

       preserve
            clear
            tempfile costs50
            save `costs50', emptyok replace
			
            foreach md in fh pr sh{   
                use `costspool', clear
                keep if mode=="`md'"
				
				local n_needed = cond("`md'"=="pr", `n_pr', ///
                         cond("`md'"=="fh", `n_fh', `n_sh'))

				quietly count
				local mult = ceil(`n_needed'/r(N))
				expand `mult'
				sample `n_needed', count
                gen int mode_id = _n
				
                keep mode mode_id cost
                append using `costs50'
                save `costs50', replace
            }	
        restore

        merge m:1 mode mode_id using `costs50', keep(3) nogen

        * Preference params: sample once per tripid
			* constant across 30 catch_draws
        preserve
            * unique trip-level skeleton from current simulated trips
            keep state draw mode date tripid
            duplicates drop

            * tripid repeats across mode/date, so create a unique merge id
            egen long pref_trip_id = group(mode date tripid)

            count
            local n_pref_needed = r(N)

            tempfile pref_keys
            save `pref_keys', replace
        restore

        preserve
            use "$misc_data_cd\preference_params.dta", clear

            * keep only current model draw
            keep if draw == `i'


            quietly count
            if r(N)==0 {
                di as error "No preference parameters found for draw `i'"
                exit 459
            }

            local mult = ceil(`n_pref_needed'/r(N))
            expand `mult'

            sample `n_pref_needed', count

            gen long pref_trip_id = _n

            * avoid conflict with master draw variable
            rename draw pref_draw

            tempfile pref_sample
            save `pref_sample', replace
        restore

        preserve
            use `pref_keys', clear
            merge 1:1 pref_trip_id using `pref_sample', keep(3) nogen
            tempfile pref_trip_params
            save `pref_trip_params', replace
        restore

        * merge preference params back to all 30 catch_draw rows per trip
        egen long pref_trip_id = group(mode date tripid)
        merge m:1 pref_trip_id using `pref_trip_params', keep(3) nogen
        drop pref_trip_id
		drop pref_draw
		
		
        * Demographics: resample once per draw
        preserve
            use "$misc_data_cd\angler_dems.dta", clear
            keep if state=="`st'"
            tempfile demspool
            save `demspool', replace
        restore

        preserve
            clear
            tempfile dems50
            save `dems50', emptyok replace

            forvalues w=1/6 {
                use `demspool', clear
                keep if wave==`w'
				
				local n_needed = cond(`w'==1, `n_wave1', ///
                 cond(`w'==2, `n_wave2', ///
				 cond(`w'==3, `n_wave3', ///
				 cond(`w'==4, `n_wave4', ///
				 cond(`w'==5, `n_wave5', `n_wave6')))))
				
				quietly count
				local mult = ceil(`n_needed'/r(N))
				expand `mult'
				sample `n_needed', count
				
                gen wave_id = _n
                keep wave wave_id age total_trips_12 /* other vars */
                append using `dems50'
                save `dems50', replace
            }
        restore

        merge m:1 wave wave_id using `dems50', keep(3) nogen


        preserve
            u  "$calib_catch_data_cd\calib_catch_draws_raw_`s'_`i'.dta", clear 
            split my_dom_id_string, parse(_)
            *rename my_dom_id_string1 state
            rename my_dom_id_string2 wave
            rename my_dom_id_string3 mode
            drop my_dom_id_string4 
            keep my_dom_id_string state wave mode  sf_* bsb_* scup_*
			ds my_dom_id_string state wave mode, not
			renvarlab `r(varlist)', postfix(_sim)
            tempfile excelpool
            save `excelpool', replace
        restore


        * sample catch outcomes by (mode,wave)
        egen long g = group(mode wave)
        bysort g: gen long gid = _n
        bysort g: gen long n_g = _N
        levelsof g, local(gs)

        tempfile trips_expanded
        save `trips_expanded', replace

        * Build catch outcomes dataset with keys (g, gid)
        clear
        tempfile catchall
        save `catchall', emptyok replace
        local seeded 0

        foreach gg of local gs {
		*local gg 10
            use `trips_expanded', clear
            keep if g==`gg'
            keep mode wave 
            local md  = mode[1]
            local wv  = wave[1]
            local n_needed = _N
			di "`md'"
			di "`wv'"
			di `n_needed'
            use `excelpool', clear
            keep if wave=="`wv'" & mode=="`md'"

			quietly count
			local mult = ceil(`n_needed'/r(N))
			expand `mult'
			sample `n_needed', count
			
            * ensure enough rows before sampling
            quietly count
            if (r(N) < `n_needed') {
                di as error "Not enough catch rows for st=`st' draw=`i' mode=`md' wave=`wv' need=`n_needed' have=" r(N)
                continue
            }

            gen long g   = `gg'
            gen long gid = _n
			destring wave, replace

            tempfile chunk
            save `chunk', replace

            if (`seeded'==0) {
                use `chunk', clear
				destring wave, replace
                save `catchall', replace
                local seeded 1
            }
            else {
                use `catchall', clear
                append using `chunk'
				destring wave, replace
                save `catchall', replace
            }
        }
		
        * Merge sampled catch onto trips by (g,gid)
        use `trips_expanded', clear
		destring wave, replace 
        merge 1:1 g gid using `catchall', keep(3) nogen

        drop g gid n_g
        compress
		
		sort date tripid catch_
		gen sf_cat  = sf_keep_sim + sf_rel_sim
        gen bsb_cat = bsb_keep_sim + bsb_rel_sim
        gen scup_cat = scup_keep_sim + scup_rel_sim

		keep state draw ///
                 sf_keep_sim sf_cat sf_rel_sim ///
                 bsb_keep_sim bsb_rel_sim bsb_cat ///
                 scup_keep_sim scup_rel_sim scup_cat ///
                 mode month date  wave ///
                 tripid catch_draw age total_trips_12 cost beta*
				 
		order state mode date tripid catch 
		compress
	
		save "$calib_catch_data_cd\calib_catch_draws_`s'_`i'.dta", replace
		
}		
}

	
			
			




display "calibration_catch_per_trip_part2.do: finished writing calib_catch_draws files."
