/*******************************************************************************
 Script:       catch_per_trip_projection_part1_refactored.do
 Purpose:      Projection-period analog of
               catch_per_trip_calibration_part1_refactored.do. Estimates mean
               harvest-, discard- and catch-per-trip with standard errors by
               state x wave x mode, imputing standard errors for strata that
               contain only a single MRIP primary sampling unit, and writes
               the per-stratum file the projection copula step draws from.

               The one substantive difference from the calibration version is
               the sample window: the calibration uses a single fishing year;
               this uses the rolling window of recent MRIP waves defined by
               $projection_catch_per_trip_years in model_wrapper.do, because
               the projection is made before the current year is complete.
               That global must be revised after each MRIP data release.
               There is no Part B (no MRIP totals) in the projection.

               This is the refactored version of
               catch_per_trip_projection_part1.do. Its whole body was a copy
               of the calibration script's Part A; that Part A is now the
               program sf_build_cpt_strata in catch_per_trip_programs.do, and
               this file is one call to it.
 Inputs:       The MRIP trip and catch files named by $triplist and $catchlist.
 Outputs:      $misc_data_cd\projected_mrip_catch_processed.xlsx (the
               per-stratum mean and standard-error file consumed by
               copula_modeling_projection.R).
 Dependencies: Globals $triplist, $catchlist, $misc_data_cd, $input_code_cd
               and $projection_catch_per_trip_years. Requires MRIP_lists.do
               to have run. Programs from catch_per_trip_programs.do (done
               below). Uses the user-written command dsconcat.
 Pipeline:     Step 9a of model_wrapper.do, inside the catch_per_trip_project
               meta-toggle that runs this, the R copula step, part2 and the
               MRIP comparison as one unit.
*******************************************************************************/

display "catch_per_trip_projection_part1_refactored.do: estimating projection-period mean catch-per-trip and standard errors from the rolling window of MRIP data in \$projection_catch_per_trip_years."

/* Load the shared programs. Each is defined behind a capture program drop
   guard, so re-running this file in the same session is safe. */
do "$input_code_cd\catch_per_trip_programs.do"

#delimit ;

/* Part A on the projection window. indzero reproduces the original
   projection script, which set the three *_keep_and_rel_ind columns to 0
   where they were never set; the calibration script leaves them blank. */
sf_build_cpt_strata, yearglobal(projection_catch_per_trip_years)
    saveas("$misc_data_cd\projected_mrip_catch_processed.xlsx") indzero ;

#delimit cr

display "catch_per_trip_projection_part1_refactored.do: finished. Wrote projected_mrip_catch_processed.xlsx."
