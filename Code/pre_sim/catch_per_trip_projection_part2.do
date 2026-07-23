


/*******************************************************************************
 Script:       catch_per_trip_projection_part2.do
 Purpose:      Builds the projection-year catch draw files the R projection
               stage consumes. Takes the calendar of days that had directed
               trips, expands each day x mode into 50 simulated trips with 30
               catch-per-trip draws each, and attaches a catch outcome to
               every one of those rows by sampling from the copula-generated
               pool of outcomes for the matching mode x wave stratum. The
               result is one file per state x draw giving simulated catch for
               every trip on every fishing day.
 Inputs:       directed_trips_calibration_<ST>.csv,
               proj_catch_draws_raw_<ST>_<i>.dta
 Outputs:      proj_catch_draws_<ST>_<i>.dta
 Dependencies: Globals $misc_data_cd, $proj_catch_data_cd and $ndraws.
               Requires copula_modeling_projection.R to have produced the
               raw projection draws, and catch_per_trip_projection_part1.do
               to have produced the directed-trips calendar.
 Pipeline:     Step 13c of model_wrapper.do, inside the `catch_per_trip_project'
               meta-toggle that runs part1, the R copula step, this script and
               compare_projection_data_to_MRIP.do as one unit. The calibration
               analog of this script is calibration_catch_per_trip_part2.do.

 Sizing note: the 50 trips x 30 catch draws per day x mode is the source of
               this stage's runtime and disk footprint - a state with ~200
               fishing days across three modes produces on the order of a
               million rows per draw, and that is repeated $ndraws times.
*******************************************************************************/

display "catch_per_trip_projection_part2.do: building projection catch draws for 9 states x $ndraws draws. This is one of the longest steps in the Stata pipeline and may run for hours."

* This script create projection catch draw files for each state that contains:
	* 50 trips in each day of the calibration year in which there were directed trips, each with 30 draws of catch-per-trip
	* Demographics: age and avidity (number trips past 12 months) come from the base_outcomes files generated in the calibration simulation 

	
********************************
 
* faster version 
local regions "MA RI CT NY NJ DE MD VA NC"
set more off

foreach s of local regions {
	
	*local s "DE"
    import delimited using "$misc_data_cd\directed_trips_calibration_`s'.csv", clear

    gen double date_num = date(date, "DMY")
	drop month month1
    gen byte   month    = month(date_num)
    gen str2   month1   = string(month, "%02.0f")
    /* MRIP waves are two-month sampling periods: wave 1 = Jan-Feb, wave 2 =
       Mar-Apr, and so on through wave 6 = Nov-Dec. Regulations, effort and
       catch are all reported and modeled at this resolution, which is why the
       month is immediately collapsed to a wave here. */
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

    * Loop draws
	forvalues i=1/$ndraws {
		*local i 1
        use `base', clear
        keep if draw==`i'

        /* Two nested expansions build the simulated trip population: each
           mode x date becomes 50 trips, and each of those trips becomes 30
           catch draws. The 50 represents variation across anglers fishing the
           same day; the 30 represents uncertainty in what any one of them
           catches. Both are fixed design constants, not estimated. */
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
		
		
        preserve
            u "$proj_catch_data_cd\proj_catch_draws_raw_`s'_`i'.dta", clear 
            split my_dom_id_string, parse(_)
            *rename my_dom_id_string1 state
            rename my_dom_id_string2 wave
            rename my_dom_id_string3 mode
            drop my_dom_id_string4 
            keep my_dom_id_string state wave mode  sf_* bsb_* scup_*
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

			/* The copula pool for this mode x wave usually holds far fewer
			   rows than the number of simulated trips needing an outcome, so
			   it is first replicated ceil(needed/have) times and then sampled
			   down to exactly the required count. This is sampling WITH
			   replacement, implemented via expand rather than bsample -
			   each pool outcome can be reused across many simulated trips. */
			quietly count
			local mult = ceil(`n_needed'/r(N))
			expand `mult'
			sample `n_needed', count

            /* This guard can no longer fire: the expand/sample above already
               guarantees exactly `n_needed' rows. It is a leftover from before
               the replication step was added. Note also that its message
               interpolates `st', which is never defined here - the state loop
               variable is `s' - so the state would print as empty if it ever
               did fire. Flagged, not fixed. */
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
		rename sf_catch sf_cat
		rename bsb_catch bsb_cat
		rename scup_catch scup_cat


		keep state draw ///
                 sf_keep sf_cat sf_rel ///
                 bsb_keep bsb_rel bsb_cat ///
                 scup_keep scup_rel scup_cat ///
                 mode month date day_i  wave ///
                 tripid catch_draw  day  
				 
		gen double date_num = date(date, "DMY")
		format date_num %td
		order state mode date tripid catch 
		compress
	
		save "$proj_catch_data_cd\proj_catch_draws_`s'_`i'.dta", replace
		
}		
}

display "catch_per_trip_projection_part2.do: finished writing proj_catch_draws files."

