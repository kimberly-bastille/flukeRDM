/*******************************************************************************
 Script:       catch_per_trip_calibration_part1.do
 Purpose:      Estimates mean harvest-, discard- and catch-per-trip with
               standard errors, by state x wave x mode, for the calibration
               year - the inputs the copula step needs in order to simulate
               correlated per-trip catch (Part A). Also computes the
               calibration-year MRIP totals the simulation is later compared
               against, at three levels of aggregation (Part B).

               Part A: some state x wave x mode cells contain only a single
               MRIP primary sampling unit, which yields a mean but no
               variance. A standard error is imputed for those cells from
               related data (the shoulder waves of the same period) so that
               every stratum has a usable mean and SE for the copula to draw
               from. Without this, those strata would drop out of the
               simulation entirely rather than merely being uncertain.
               Part B: survey-weighted MRIP totals by state x mode, by state,
               and by state x mode x wave.

 Structure:    This file is calls, not logic. The ~150-line MRIP prep
               block, the svy/postfile domain loop (and its two
               standard-error imputation variants), the results decoder and
               the export block are each defined once in
               catch_per_trip_programs.do and invoked from here and from the
               projection script. See that file's header for the behaviour
               marked PRESERVED - things that look like mistakes but are kept
               deliberately.
 Inputs:       The MRIP trip and catch files named by $triplist and $catchlist.
 Outputs:      $misc_data_cd\baseline_mrip_catch_processed.xlsx (the
               per-stratum mean and standard-error file consumed by
               copula_modeling_calibration.R), plus the calibration-year MRIP
               totals:
               $misc_data_cd\mrip_catch_calib_state_mode.dta
               $misc_data_cd\mrip_catch_calib_state.dta
               $misc_data_cd\mrip_catch_calib_state_mode_wave.dta
 Dependencies: Globals $triplist, $catchlist, $misc_data_cd, $input_code_cd
               and $calibration_year. Requires MRIP_lists.do to have run.
               Programs from catch_per_trip_programs.do (done below). Uses
               the user-written commands dsconcat and renvarlab.
 Pipeline:     Step 5a of model_wrapper.do, gated by the toggle catch_per_trip1.
               Feeds the R copula step (5b), whose output
               calibration_catch_per_trip_part2.do then expands into daily
               catch draws. Its projection-year analog is
               catch_per_trip_projection_part1.do, which runs the
               same Part A on a rolling window of MRIP data and has no Part B.
*******************************************************************************/

display "catch_per_trip_calibration_part1.do: estimating mean catch-per-trip and standard errors by state x wave x mode, imputing standard errors for single-PSU strata. This may take a while."

/* Load the shared programs. Each is defined behind a capture program drop
   guard, so re-running this file in the same session is safe. */
do "$input_code_cd\catch_per_trip_programs.do"

#delimit ;

/******************************************************************************/
/******************************************************************************/
/* Part A: mean catch-per-trip by stratum, with SE imputation, to xlsx        */
/******************************************************************************/
/******************************************************************************/

/* The calibration script does NOT pass indzero: the three columns
   sf/bsb/scup_keep_and_rel_ind are left as-is (blank where never set), as
   in the original calibration script. The projection script passes it. */
sf_build_cpt_strata, yearglobal(calibration_year)
    saveas("$misc_data_cd\baseline_mrip_catch_processed.xlsx") ;

/******************************************************************************/
/******************************************************************************/
/* Part B: MRIP totals for comparison with simulated estimates                */
/******************************************************************************/
/******************************************************************************/

/* Each sub-block: prep the trip-level data with the sub-block's domain
   string; build the (redundant, PRESERVED) my_dom_id_string2 / my_dom_id2
   copy the original ran svy: total over; estimate totals by domain; decode
   the domain ids; keep the estimate columns; tidy and save. The local
   domain-part names passed to sf_export_mrip_totals are the column names the
   saved .dta gets (state, mode, wave). */

/* B.1: estimates by state and mode */
tempfile domains ;
sf_prep_mrip_trip_catch, domvars(state mode1 common_dom)
    yearglobal(calibration_year) domainsfile(`domains') ;

gen my_dom_id_string2=state+"_"+mode1+"_"+common_dom ;
encode my_dom_id_string2, gen(my_dom_id2) ;

sf_post_svy_by_domain, stat(total)
    vars(sf_keep sf_rel sf_cat bsb_keep bsb_rel bsb_cat scup_keep scup_rel scup_cat)
    over(my_dom_id2) civars(ll ul) ;
decode_svy_domains, domainsfile(`domains') ;
keep varname total se ll ul my_dom_id_string ;

sf_export_mrip_totals, domvars(state mode common_dom)
    saveas("$misc_data_cd\mrip_catch_calib_state_mode.dta") ;

/* B.2: estimates by state */
tempfile domains ;
sf_prep_mrip_trip_catch, domvars(state common_dom)
    yearglobal(calibration_year) domainsfile(`domains') ;

gen my_dom_id_string2=state+"_"+common_dom ;
encode my_dom_id_string2, gen(my_dom_id2) ;

sf_post_svy_by_domain, stat(total)
    vars(sf_keep sf_rel sf_cat bsb_keep bsb_rel bsb_cat scup_keep scup_rel scup_cat)
    over(my_dom_id2) civars(ll ul) ;
decode_svy_domains, domainsfile(`domains') ;
keep varname total se ll ul my_dom_id_string ;

sf_export_mrip_totals, domvars(state common_dom)
    saveas("$misc_data_cd\mrip_catch_calib_state.dta") ;

/* B.3: estimates by state, mode and wave. Note the domain string puts wave
   AFTER mode here, unlike Part A; wv2 is the wave string (see the note on
   the B.3 wave string in catch_per_trip_programs.do). */
tempfile domains ;
sf_prep_mrip_trip_catch, domvars(state mode1 wv2 common_dom)
    yearglobal(calibration_year) domainsfile(`domains') ;

gen my_dom_id_string2=state+"_"+mode1+"_"+wv2+"_"+common_dom ;
encode my_dom_id_string2, gen(my_dom_id2) ;

sf_post_svy_by_domain, stat(total)
    vars(sf_keep sf_rel sf_cat bsb_keep bsb_rel bsb_cat scup_keep scup_rel scup_cat)
    over(my_dom_id2) civars(ll ul) ;
decode_svy_domains, domainsfile(`domains') ;
keep varname total se ll ul my_dom_id_string ;

sf_export_mrip_totals, domvars(state mode wave common_dom)
    saveas("$misc_data_cd\mrip_catch_calib_state_mode_wave.dta") ;

#delimit cr

display "catch_per_trip_calibration_part1.do: finished. Wrote baseline_mrip_catch_processed.xlsx and the three mrip_catch_calib_*.dta files."
