/*******************************************************************************
 Script:       catch_per_trip_projection_part2_refactored.do
 Purpose:      Builds the projection-year catch draw files the R projection
               stage consumes. Takes the calendar of days that had directed
               trips, expands each day x mode into 50 simulated trips with 30
               catch-per-trip draws each, and attaches a catch outcome to
               every one of those rows by sampling from the copula-generated
               pool of outcomes for the matching mode x wave stratum. The
               result is one file per state x draw giving simulated catch for
               every trip on every fishing day.

               This is the refactored version of
               catch_per_trip_projection_part2.do. Its per-state x per-draw
               loop was a copy of the calibration script's loop minus the
               cost, preference-parameter and demographics resamples. The
               shared blocks are now the programs sf_load_directed_trips,
               sf_expand_trips_and_ids and sf_sample_catch_by_mode_wave in
               catch_per_trip_programs.do; this file is the loop that calls
               them plus the projection-specific tail (rename the species
               totals, keep the projection columns, add date_num, save).
               Behaviour is intended to be identical to the original, output
               for output; see the program headers for what is PRESERVED.
 Inputs:       directed_trips_calibration_<ST>.csv (from
               directed_trips_calibration.do),
               proj_catch_draws_raw_<ST>_<i>.dta (from
               copula_modeling_projection.R)
 Outputs:      proj_catch_draws_<ST>_<i>.dta
 Dependencies: Globals $misc_data_cd, $proj_catch_data_cd, $input_code_cd and
               $ndraws. Programs from catch_per_trip_programs.do (done
               below). Uses the user-written commands distinct and renvarlab.
 Pipeline:     Step 9c of model_wrapper.do, inside the catch_per_trip_project
               meta-toggle that runs part1, the R copula step, this script and
               compare_projection_data_to_MRIP.do as one unit. The calibration
               analog of this script is
               calibration_catch_per_trip_part2_refactored.do.

 Sizing note: the 50 trips x 30 catch draws per day x mode is the source of
               this stage's runtime and disk footprint - a state with ~200
               fishing days across three modes produces on the order of a
               million rows per draw, and that is repeated $ndraws times.
*******************************************************************************/

display "catch_per_trip_projection_part2_refactored.do: building projection catch draws for 9 states x $ndraws draws. This is one of the longest steps in the Stata pipeline and may run for hours."

/* Load the shared programs. Each is defined behind a capture program drop
   guard, so re-running this file in the same session is safe. */
do "$input_code_cd\catch_per_trip_programs.do"

#delimit ;

local regions "MA RI CT NY NJ DE MD VA NC" ;
set more off ;

foreach s of local regions {;

    /* this state's calendar of days with directed trips, all draws */
    tempfile base ;
    sf_load_directed_trips, state(`s') saveas(`base') ;

    forvalues i=1/$ndraws {;
        use `base', clear ;
        keep if draw==`i' ;

        /* 50 trips x 30 catch draws per mode x day, with mode_id and
           wave_id. The trip counts it returns are not needed here (the
           projection attaches no costs or demographics); the block runs
           anyway because its sorts and merges set the sort RNG state that
           every later sort depends on. */
        sf_expand_trips_and_ids ;

        /* one copula catch outcome per trip row, by mode x wave.
           The projection keeps the pool's column names (sf_keep, sf_rel,
           sf_catch ...): no _sim postfix. */
        sf_sample_catch_by_mode_wave,
            rawfile("$proj_catch_data_cd\proj_catch_draws_raw_`s'_`i'.dta")
            state(`s') draw(`i') ;

        /* projection-specific tail. The sort key is written out in full:
           the original abbreviated catch_draw to catch_, the only variable
           with that prefix at this point. mode is not in the key, so rows
           of the three modes for one date x tripid x catch_draw are
           tie-ordered by the sort RNG, as in the original. */
        sort date tripid catch_draw ;
        foreach sp in sf bsb scup {;
            rename `sp'_catch `sp'_cat ;
        };

        keep state draw
             sf_keep sf_cat sf_rel
             bsb_keep bsb_rel bsb_cat
             scup_keep scup_rel scup_cat
             mode month date day_i  wave
             tripid catch_draw  day ;

        gen double date_num = date(date, "DMY") ;
        format date_num %td ;
        order state mode date tripid catch_draw ;
        compress ;

        save "$proj_catch_data_cd\proj_catch_draws_`s'_`i'.dta", replace ;
    };
};

#delimit cr

display "catch_per_trip_projection_part2_refactored.do: finished writing proj_catch_draws files."
