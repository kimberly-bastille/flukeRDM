/*******************************************************************************
 Script:       calibration_catch_per_trip_part2_refactored.do
 Purpose:      Two jobs. First, builds the angler demographic pool (age and
               avidity) from the MRIP Fishing Effort Survey 12-month files,
               with the QA/QC screens the FES delivery notes require. Second,
               builds the calibration-year catch draw files: for every day
               with directed trips, 50 simulated trips x 30 catch-per-trip
               draws, each assigned a trip cost sampled by mode, a set of
               angler preference parameters, demographics sampled by wave
               from the pool built in the first half, and a catch outcome
               sampled from the copula pool for its mode x wave stratum.

               This is the refactored version of
               calibration_catch_per_trip_part2.do. The calendar load, the
               50 x 30 expansion with its mode_id / wave_id keys, and the
               catch-sampling loop were duplicated in the projection script;
               they are now the programs sf_load_directed_trips,
               sf_expand_trips_and_ids and sf_sample_catch_by_mode_wave in
               catch_per_trip_programs.do. The with-replacement sampling
               idiom that this file repeated four times is
               sample_with_replacement, and the state-label block of Section
               A is sf_label_states (shared with part1). The cost,
               preference-parameter and demographics resamples exist only
               here and stay inline. Behaviour is intended to be identical
               to the original, output for output; see the program headers
               for what is PRESERVED.
 Inputs:       fes_person_final_2023<wave>.dta (waves 1-6),
               directed_trips_calibration_<ST>.csv (from
               directed_trips_calibration.do),
               calib_catch_draws_raw_<ST>_<i>.dta (from
               copula_modeling_calibration.R),
               trip_costs.dta,
               preference_params.dta
 Outputs:      angler_dems.dta, calib_catch_draws_<ST>_<i>.dta
 Dependencies: Globals $misc_data_cd, $calib_catch_data_cd, $input_code_cd
               and $ndraws. Programs from catch_per_trip_programs.do (done
               below). Uses the user-written commands dsconcat, distinct and
               renvarlab.
 Pipeline:     Step 5c of model_wrapper.do, gated by the `catch_per_trip2'
               toggle (default ON). Its projection-year analog is
               catch_per_trip_projection_part2_refactored.do, which shares
               the trip expansion and catch-sampling programs but attaches
               no costs, preferences or demographics.

 Data sensitivity note: the FES 12-month files read below are NOT publicly
 available and, per the note from the data provider reproduced in the source
 comments, are essentially unedited raw responses. The QA/QC screens applied
 here are this project's own, not NOAA's - the 12-month effort fields do not
 go through the editing that the 2-month fields do.
*******************************************************************************/

display "calibration_catch_per_trip_part2_refactored.do: building the angler demographic pool, then calibration catch draws for 9 states x $ndraws draws. This is one of the longest steps in the Stata pipeline and may run for hours."

/* Load the shared programs. Each is defined behind a capture program drop
   guard, so re-running this file in the same session is safe. */
do "$input_code_cd\catch_per_trip_programs.do"

#delimit ;

/******************************************************************************/
/******************************************************************************/
/* Section A: Angler demographic pool from FES                                */
/******************************************************************************/
/******************************************************************************/

/************************************
 Demographics: age and avidity (number trips past 12 months).
 Ages and avidity come from the fishing effort survey 12 MONTH files.
 These data are NOT publicly available and the data have not been processed
 for QA/QC like the publicly available 2-month files.
 Data from 2018-2023 was delivered by Lucas Johanssen on 4/23/2025. A few
 notes/caveats from Lucas:
   "FES QC processes focus on the 2-month reference periods, and we do very
   little evaluation and editing of 12-month effort responses.
   Responses for these fields are essentially unedited, raw data.
   The final weight trimming procedures focus on reducing the impacts of
   outlier values on wave-level estimates.
   The data may include records that are highly influential with respect to
   12-month effort and any estimates may be highly variable.
   Wave data will produce independent estimates of 12-month effort."
 The most recent year of FES survey data available (2023) is used.
**************************************/

display "calibration_catch_per_trip_part2_refactored.do: assembling the FES angler demographic pool." ;

global dems ;
local wvs 1 2 3 4 5 6 ;
foreach w of local wvs {;

    use "$misc_data_cd\fes_person_final_2023`w'.dta", clear ;

    sf_label_states ;
    keep if state!="" ;

    tempfile dems`w' ;
    save `dems`w'', replace ;
    global dems "$dems "`dems`w''" " ;

};
clear ;
dsconcat $dems ;

gen total_trips_12=boat_trips_12+shore_trips_12 ;
gen total_trips_2=boat_trips+shore_trips ;

/************************************
 The screens below are internal consistency filters on unedited survey
 responses, not statistical outlier trimming for its own sake. In order:
 -3 is the FES missing-value sentinel for age. The 16+ restriction matches
 the choice experiment's sampling frame (licensees), so that the simulated
 age distribution and the estimated preferences describe the same
 population. Reporting more trips in the last 2 months than in the last 12
 is internally impossible; 62 two-month trips is roughly one per day and
 365 twelve-month trips is more than one per day, both taken as
 implausible self-reports.
**************************************/

drop if age==-3 ;
keep if age>=16 ;

replace total_trips_2=round(total_trips_2) ;
replace total_trips_12=round(total_trips_12) ;
drop if total_trips_2>total_trips_12 ;

drop if total_trips_2>=62 ;
drop if total_trips_12>=365 ;

/************************************
 The expand below turns the weighted survey into an explicit population: an
 angler with weight w becomes w rows, so later sampling from this file is
 simple random sampling that nonetheless respects the survey weights. That
 is why the weights must first be divided by 100 - expanding on the raw
 weights would attempt to materialize ~300 million rows. Dividing
 proportionally preserves the relative weighting, so the resulting age and
 avidity distributions are unchanged, only the pool size shrinks.
**************************************/
replace final=final/100 ;
replace final=round(final) ;

expand final ;
su total_trips_12, detail ;

/* drop total_trips_12 above the 99.95 percentile */
egen p9995 = pctile(total_trips_12), p(99.95) ;
drop if total_trips_12>p9995 ;

keep age total_trips_12 wave state ;
save "$misc_data_cd\angler_dems.dta", replace ;

/******************************************************************************/
/******************************************************************************/
/* Section B: Calibration catch draw files                                    */
/******************************************************************************/
/******************************************************************************/

/************************************
 For every state and draw: expand the calendar of days with directed trips
 to 50 trips x 30 catch draws per mode x day, then attach, in this order,
 a trip cost (resampled by mode), preference parameters (resampled per
 trip), demographics (resampled by wave from Section A's pool) and a catch
 outcome (sampled from the copula pool by mode x wave). The order matters:
 every resample consumes the random number stream, and every sort and
 merge consumes the sort stream, so the blocks below run in exactly the
 original sequence.
**************************************/

display "calibration_catch_per_trip_part2_refactored.do: generating calibration catch draw files." ;

local regions "MA RI CT NY NJ DE MD VA NC" ;

set more off ;
set rmsg off ;

foreach s of local regions {;

    /* this state's calendar of days with directed trips, all draws */
    tempfile base ;
    sf_load_directed_trips, state(`s') saveas(`base') ;

    forvalues i=1/$ndraws {;
        use `base', clear ;
        keep if draw==`i' ;

        /* 50 trips x 30 catch draws per mode x day, with mode_id and
           wave_id, and the distinct-trip counts per mode and per wave that
           size the cost and demographics resamples below */
        sf_expand_trips_and_ids ;
        foreach k in pr fh sh wave1 wave2 wave3 wave4 wave5 wave6 {;
            local n_`k' = r(n_`k') ;
        };

        /* Costs: resample once per draw.
           PRESERVED: the first preserve/restore loads and filters the cost
           file and throws the result away; it is inert (use and keep if do
           not sort) and kept as in the original. */
        preserve ;
            use "$misc_data_cd\trip_costs.dta", clear ;
            keep if state == substr("`s'",1,2) ;
        restore ;

        local st = state[1] ;

        preserve ;
            use "$misc_data_cd\trip_costs.dta", clear ;
            keep if state=="`st'" ;
            keep state mode cost ;
            tempfile costspool ;
            save `costspool', replace ;
        restore ;

        preserve ;
            clear ;
            tempfile costs50 ;
            save `costs50', emptyok replace ;

            foreach md in fh pr sh {;
                use `costspool', clear ;
                keep if mode=="`md'" ;

                /* n_pr / n_fh / n_sh from sf_expand_trips_and_ids */
                local n_needed = `n_`md'' ;
                sample_with_replacement, n(`n_needed') ;
                gen int mode_id = _n ;

                keep mode mode_id cost ;
                append using `costs50' ;
                save `costs50', replace ;
            };
        restore ;

        merge m:1 mode mode_id using `costs50', keep(3) nogen ;

        /* Preference params: sample once per tripid, constant across the
           30 catch_draws */
        preserve ;
            /* unique trip-level skeleton from current simulated trips */
            keep state draw mode date tripid ;
            duplicates drop ;

            /* tripid repeats across mode/date, so create a unique merge id */
            egen long pref_trip_id = group(mode date tripid) ;

            count ;
            local n_pref_needed = r(N) ;

            tempfile pref_keys ;
            save `pref_keys', replace ;
        restore ;

        preserve ;
            use "$misc_data_cd\preference_params.dta", clear ;

            /* keep only current model draw */
            keep if draw == `i' ;

            quietly count ;
            if r(N)==0 {;
                di as error "No preference parameters found for draw `i'" ;
                exit 459 ;
            };

            /* the original's expand/sample idiom sat right here, after
               this zero-row guard; sample_with_replacement repeats the
               quietly count first, which reads the data and changes
               nothing */
            sample_with_replacement, n(`n_pref_needed') ;

            gen long pref_trip_id = _n ;

            /* avoid conflict with master draw variable */
            rename draw pref_draw ;

            tempfile pref_sample ;
            save `pref_sample', replace ;
        restore ;

        preserve ;
            use `pref_keys', clear ;
            merge 1:1 pref_trip_id using `pref_sample', keep(3) nogen ;
            tempfile pref_trip_params ;
            save `pref_trip_params', replace ;
        restore ;

        /* merge preference params back to all 30 catch_draw rows per trip */
        egen long pref_trip_id = group(mode date tripid) ;
        merge m:1 pref_trip_id using `pref_trip_params', keep(3) nogen ;
        drop pref_trip_id ;
        drop pref_draw ;

        /* Demographics: resample once per draw, by wave, from Section A */
        preserve ;
            use "$misc_data_cd\angler_dems.dta", clear ;
            keep if state=="`st'" ;
            tempfile demspool ;
            save `demspool', replace ;
        restore ;

        preserve ;
            clear ;
            tempfile dems50 ;
            save `dems50', emptyok replace ;

            forvalues w=1/6 {;
                use `demspool', clear ;
                keep if wave==`w' ;

                /* n_wave1 .. n_wave6 from sf_expand_trips_and_ids */
                local n_needed = `n_wave`w'' ;
                sample_with_replacement, n(`n_needed') ;

                gen wave_id = _n ;
                keep wave wave_id age total_trips_12 ;
                append using `dems50' ;
                save `dems50', replace ;
            };
        restore ;

        merge m:1 wave wave_id using `dems50', keep(3) nogen ;

        /* one copula catch outcome per trip row, by mode x wave. simpostfix
           renames the pool's sf_* bsb_* scup_* columns with _sim, as the
           original calibration script did. */
        sf_sample_catch_by_mode_wave,
            rawfile("$calib_catch_data_cd\calib_catch_draws_raw_`s'_`i'.dta")
            state(`st') draw(`i') simpostfix ;

        /* calibration-specific tail. The sort key is written out in full:
           the original abbreviated catch_draw to catch_, the only variable
           with that prefix at this point. mode is not in the key, so rows
           of the three modes for one date x tripid x catch_draw are
           tie-ordered by the sort RNG, as in the original. */
        sort date tripid catch_draw ;
        foreach sp in sf bsb scup {;
            gen `sp'_cat = `sp'_keep_sim + `sp'_rel_sim ;
        };

        keep state draw
             sf_keep_sim sf_cat sf_rel_sim
             bsb_keep_sim bsb_rel_sim bsb_cat
             scup_keep_sim scup_rel_sim scup_cat
             mode month date  wave
             tripid catch_draw age total_trips_12 cost beta* ;

        order state mode date tripid catch_draw ;
        compress ;

        save "$calib_catch_data_cd\calib_catch_draws_`s'_`i'.dta", replace ;
    };
};

#delimit cr

display "calibration_catch_per_trip_part2_refactored.do: finished writing calib_catch_draws files."
